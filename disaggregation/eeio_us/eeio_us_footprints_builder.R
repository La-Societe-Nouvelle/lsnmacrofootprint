# La Société Nouvelle

####################################################################################################
# UTILS

source("disaggregation/utils.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# BUILDING US EEIO DATA

build_us_eeio_footprints <<- function(year_i, verbose = TRUE)
{
  message("[LOG] Fetching and formatting US EEIO")

  # --------------------------------------------------
  # Metadata

  eeio_size <- 398

  eeio_inudstries <- read_delim(
      "disaggregation/eeio_us/metadata_us_eeio_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = us_eeio_industry_code
    ) %>%
    select(eeio_industry)

  table_passage_a732 <- read_delim(
      "disaggregation/eeio_us/table_passage_a732_us.csv",
      delim = ";",
      na = character(),
      show_col_types = FALSE
    ) %>%
    filter(flag_mapping_a732 != "na") %>%
    rename(
      eeio_industry = code_eeio_us
    ) %>%
    select(code_ape_a732, eeio_industry, accuracy_mapping_a732, flag_mapping_a732)

  metadata_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      code_ape_a732 = code,
      figaro_industry = industry
    ) %>%
    select(code_ape_a732, figaro_industry)

  correspondences_figaro <- table_passage_a732 %>%
    merge(metadata_nace_niv5) %>%
    select(eeio_industry, figaro_industry) %>%
    distinct()

  # --------------------------------------------------
  # EEIO Data

  link <- "https://pasteur.epa.gov/uploads/10.23719/1532178/USEEIOv2.5-catbird-22.xlsx"

  # downloading file
  excel_file <- link %>%
    curl_download(destfile = tempfile())

  # --------------------------------------------------
  # Intermediate inputs (in USD)

  # Table 398x398

  z <- suppressMessages(
      read_xlsx(excel_file, sheet = "U")
    ) %>%
    column_to_rownames("...1") %>%
    {
      rownames(.) <- sub("/US$", "", rownames(.))
      colnames(.) <- sub("/US$", "", colnames(.))
      .
    } %>% {
      ids <- sort(intersect(rownames(.), colnames(.)))
      .[ids, ids, drop = FALSE] # matrice carrée
    } %>%
    { .[. < 0] <- 0; . } %>% # /!\ remove negative inputs
    { . / 1000000 }

  if (!identical(rownames(z), colnames(z))) {
    idx <- which(rownames(z) != colnames(z))
    message("[ERROR] rownames != colnames (", length(idx), " différences)")
    print(data.frame(
      i = idx,
      row = rownames(z)[idx],
      col = colnames(z)[idx]
    ) |> head(20))
    stop("Mismatch rownames/colnames dans z")
  }
  if (nrow(z) != eeio_size || ncol(z) != eeio_size) {
    message("[ERROR] Format incorrect pour Z")
    print(z %>% as_tibble())
    stop("Error dans z")
  }
  message("[INFO] Ok - Table Z")

  # --------------------------------------------------
  # Production (in USD)

  # Table 398x1

  production <- suppressMessages(
      read_xlsx(excel_file, sheet = "x")
    ) %>%
    column_to_rownames("...1") %>%
    # parsing data
    {
      rownames(.) <- sub("/US$", "", rownames(.))
      .
    } %>%
    # filter data
    filter(rownames(.) %in% rownames(z)) %>%
    # adjust unit
    { . / 1000000 } %>%
    # format data
    rownames_to_column(var = "eeio_industry") %>%
    mutate(
      year = year_i,
      unit = "USD"
    ) %>%
    select(eeio_industry, year, x, unit) %>%
    arrange(eeio_industry)

  if (nrow(production) != eeio_size) {
    message("[ERROR] Format incorrect pour X")
    print(production %>% as_tibble())
  }
  message("[INFO] Ok - Table X")

  # --------------------------------------------------
  # Main aggregates (in USD)

  intermediate_consumptions <- data.frame(
    eeio_industry = colnames(z),
    p2 = colSums(z)
  )

  main_aggregates <- production %>%
    merge(intermediate_consumptions) %>%
    mutate(
      va = x - p2
    ) %>%
    select(eeio_industry, year, unit, x, p2, va) %>%
    arrange(eeio_industry)

  # --------------------------------------------------
  # Coef PRG / GHG Emissions

  # Table 398x1

  PRG <- suppressMessages(
      read_xlsx(excel_file, sheet = "C")
    ) %>%
    column_to_rownames('...1')

  emissions_data <- suppressMessages(
      read_xlsx(excel_file, sheet = "B")
    ) %>% # kg by USD output
    column_to_rownames('...1') %>%
    {
      colnames(.) <- sub("/US$", "", colnames(.))
      .
    } %>%
    select(matches(rownames(z))) %>% # filtre sur données Z
    # Passage des intensités (kg/USD) aux émissions totales (kg)
    {
      factors_prod <- unlist(production$x[match(colnames(.), production$eeio_industry)] * 1000)
      sweep(., 2, factors_prod, `*`)
    } %>%
    # Application des PRG
    {
      factors_prg <- unlist(PRG["Greenhouse Gases", rownames(.), drop = TRUE])
      sweep(., 1, factors_prg, `*`)
    } %>%
    # Somme des GES par secteur
    colSums() %>%
    as.data.frame() %>% {
      colnames(.) <- "emissions"
      .
    } %>%
    rownames_to_column(var = "eeio_industry") %>%
    select(eeio_industry, emissions) %>%
    arrange(eeio_industry)

  if (nrow(emissions_data) != eeio_size) {
    message("[ERROR] Format incorrect pour emissions_data")
    print(emissions_data %>% as_tibble())
  }
  message("[INFO] Ok - Table Emissions")
  # print(emissions_data %>% as_tibble())

  # --------------------------------------------------
  # Compute footprints

  # Table 398x4 (Aggregates : PRD, IC, GVA, DF)

  ghg_fpt <- compute_ghg_fpt("US", z, main_aggregates, emissions_data, correspondences_figaro, year_i)

  message("[INFO] Ok - Empreintes calculées")

  # --------------------------------------------------
  # Monetary conversion

  usd_eur <- from_usd_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt / usd_eur,
      unit = "GCO2E_EUR"
    ) %>%
    select(eeio_country, eeio_industry, aggregate, fpt, unit, year)

  # --------------------------------------------------
  # Mapping A*732

  ghg_fpt_a732 <- table_passage_a732 %>%
    merge(ghg_fpt_eur) %>%
    group_by(eeio_country, aggregate, unit, year, code_ape_a732, accuracy_mapping_a732) %>%
    summarise(
      fpt = round(mean(fpt, na.rm = TRUE), digits = 0),
      .groups = "drop"
    ) %>%
    mutate(
      eeio_model = "EEIO_US",
      country = "FR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732) %>%
    arrange(year, code_ape_a732, aggregate)

  # --------------------------------------------------

  print(ghg_fpt_a732 %>% as_tibble())

  write.csv(
    ghg_fpt_a732,
    file = "disaggregation/data_temp/footprints_us_eeio.csv",
    row.names = FALSE
  )
  message("[INFO] Ok - Empreintes EEIO US")
}
