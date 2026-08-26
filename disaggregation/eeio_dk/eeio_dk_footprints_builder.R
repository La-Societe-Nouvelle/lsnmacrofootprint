# La Société Nouvelle

####################################################################################################
# UTILS

source("disaggregation/utils.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# BUILDING DK EEIO DATA

build_dk_eeio_footprints <- function(year_i, verbose = TRUE)
{
  message("[LOG] Fetching and formatting DENMARK EEIO")

  # ----------------------------------------------------------------------------------------------------
  # Metadata

  dk_eeio_industries <- read_delim(
      "disaggregation/eeio_dk/metadata_dk_eeio_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      dk_eeio_industry = dk_eeio_industry_code
    ) %>%
    select(dk_eeio_industry)

  eeio_size <- length(dk_eeio_industries$dk_eeio_industry) # 117

  table_passage_a732 <- read_delim(
    "disaggregation/eeio_dk/table_passage_a732_dk.csv",
    delim = ";",
    na = character(),
    show_col_types = FALSE
  ) %>%
    rename(
      eeio_industry = code_eeio_dk
    ) %>%
    select(code_ape_a732, eeio_industry, accuracy_mapping_a732)

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
    filter(
      !is.na(eeio_industry),
      eeio_industry != ""
    ) %>%
    select(eeio_industry, figaro_industry) %>%
    distinct()

  # print(table_passage_a732 %>% as_tibble())
  # print(correspondences_figaro %>% as_tibble(), n = 33)

  # --------------------------------------------------
  # EEIO Data

  url_eeio <- "https://www.dst.dk/ext/4764115610808/0/inout/Excel-files-with-IO-data-for-the-period-2016-2025--zip"
  filename_eeio <- paste0("InputOutput_en_", year_i, ".xlsx")

  file_eeio_data <- curl_download(url_eeio, tempfile(fileext = ".zip")) %>%
    unzip(files = filename_eeio, exdir = tempdir())

  eeio_data <- suppressMessages(
      read_xlsx(file_eeio_data, sheet = "IO", skip = 2)
    ) %>%
    slice(-1) %>%
    select(1, 3:119) %>%
    rename(product = `From/To`)

  # --------------------------------------------------
  # Production (in DKK)

  # Table 117x1

  x <- eeio_data %>%
    filter(product == 'Total Output') %>%
    select(!product) %>%
    t() %>%
    as.data.frame() %>%
    `colnames<-`('x') %>%
    mutate(x = x / 1000) %>% # Initially in THS DKK
    rownames_to_column('eeio_industry')

  if (nrow(x) != eeio_size) {
    message("[ERROR] Format incorrect pour X")
    print(x %>% as_tibble())
  }
  message("[INFO] Ok - Table X")

  # --------------------------------------------------
  # Intermediate inputs (in ...)

  # Table IO 117x117

  z <- eeio_data %>%
    group_by(product) %>%
    filter(n() == 2) %>%
    summarise(
      across(where(is.numeric), ~ sum(.x/1000, na.rm = TRUE))
    ) %>% # Initially in THS DKK
    ungroup() %>%
    column_to_rownames('product') %>%
    { .[colnames(.), , drop = FALSE] }

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
  # Main aggregates (in ...)

  intermediate_consumptions <- data.frame(
    eeio_industry = colnames(z),
    p2 = colSums(z)
  )

  main_aggregates <- x %>%
    merge(intermediate_consumptions) %>%
    mutate(
      va = x - p2,
           unit = 'DKK',
           year = year_i) %>%
    select(eeio_industry, year, unit, x, p2, va) %>%
    arrange(eeio_industry)

  # --------------------------------------------------
  # Coef PRG / GHG Emissions

  # Table 117x1

  base_url_emissions_data <- "https://api.statbank.dk/v1/data/DRIVHUS2/CSV"

  url_emissions_data <- paste0(
    base_url_emissions_data,
    "?lang=","en",
    "&valuePresentation=","Code",
    "&delimiter=","Semicolon",
    "&EMTYPE8=","GHGBIO",
    "&Tid=",year_i,
    "&BRANCHE=","*",
    "&OPPRINCIP=","DIR"
  )

  emissions_data <- read.csv(
      url_emissions_data,
      sep = ";"
    ) %>%
    filter(
      substr(BRANCHE,1,1) == "V",
      substr(BRANCHE,2,2) %in% 0:9
    ) %>%
    mutate(
      eeio_industry = gsub("V", "", BRANCHE),
      emissions = INDHOLD * 1000 # Initially THS_T
    ) %>%
    filter(
      eeio_industry %in% dk_eeio_industries$dk_eeio_industry
    )

  if (nrow(emissions_data) != eeio_size) {
    message("[ERROR] Format incorrect pour emissions_data")
    print(emissions_data %>% as_tibble())
  }
  message("[INFO] Ok - Table Emissions")

  # --------------------------------------------------
  # Compute footprints

  # Table 117x4 (Aggregates : PRD, IC, GVA, DF)

  ghg_fpt <- compute_ghg_fpt("DK", z, main_aggregates, emissions_data, correspondences_figaro, year_i)

  # --------------------------------------------------
  # Monetary conversion

  dkk_eur <- from_dkk_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt*dkk_eur,
      unit = "GCO2E_EUR"
    ) %>%
    select(eeio_country, eeio_industry, aggregate, fpt, unit, year)

  # --------------------------------------------------
  # Mapping A*732

  ghg_fpt_a732 <- ghg_fpt_eur %>%
    merge(table_passage_a732) %>%
    group_by(eeio_country, aggregate, unit, year, code_ape_a732, accuracy_mapping_a732) %>%
    summarise(
      fpt = round(mean(fpt, na.rm = TRUE), digits = 0),
      .groups = "drop"
    ) %>%
    mutate(
      eeio_model = "EEIO_DK",
      country = "FR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732) %>%
    arrange(year, code_ape_a732, aggregate)

  # --------------------------------------------------

  print(ghg_fpt_a732 %>% as_tibble())

  write.csv(
    ghg_fpt_a732,
    file = "disaggregation/data_temp/footprints_dk_eeio.csv",
    row.names = FALSE
  )
  message("[INFO] Ok - Empreintes EEIO DENMARK")
}
