# La Société Nouvelle

####################################################################################################
# PARAMETERS

YEAR = 2022

conn <<- get_connection_db()

use_temp_data <- TRUE
do_update <- TRUE

####################################################################################################
# UTILS

source("DB/stats_db.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# FETCHING EEIO DATA

# --------------------------------------------------
# Fetching UK EEIO data

get_uk_eeio = function(year_i, verbose = T)
{
  message("[LOG] Fetching and formatting UK EEIO")

  # ----------------------------------------------------------------------------------------------------
  # Metadata

  eeio_size = 104

  eeio_industries <- read_delim(
      "disaggregation/eeio_uk/metadata_uk_eeio_industries.csv",
      delim = ";",
      na = character(),
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = uk_eeio_industry_code
    ) %>%
    select(eeio_industry)

  table_passage_a732 <- read_delim(
      "disaggregation/eeio_uk/table_passage_a732_uk.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = code_eeio_uk
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
    select(eeio_industry, figaro_industry) %>%
    rbind(data.frame(eeio_industry = "L68A", figaro_industry = "L")) %>% # ajout L68A
    distinct()

  correspondences_sic_groups <- read_delim(
      "disaggregation/eeio_uk/metadata_uk_sic_groups.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(sic_group = code) %>%
    select(sic_group, iot_ref)

  # ----------------------------------------------------------------------------------------------------
  # EEIO Data

  url_sut = "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb24.xlsx"

  file_sut = curl_download(url_sut, tempfile())

  # --------------------------------------------------
  # Production (in GBP)

  # Table 104x1

  x = suppressMessages(
        read_xlsx(file_sut, sheet = paste0("Table 1 - Supply ", year_i), skip = 2)
      ) %>%
      filter(grepl('CPA',`...1`)) %>% # garde les lignes commençant par CPA_
      mutate(`...1` = gsub("CPA_", "", `...1`)) %>%
      column_to_rownames('...1') %>%
      rownames_to_column(var = "eeio_industry") %>%
      mutate(
        x = `Total domestic\r\noutput of \r\nproducts at basic prices`,
        year = year_i,
        unit = "GBP"
      ) %>%
      select(eeio_industry, year, x, unit) %>%
      arrange(eeio_industry)

  if (nrow(x) != 104) {
    message("[ERROR] Format incorrect pour X")
    print(x %>% as_tibble())
  }
  message("[INFO] Ok - Table X")

  # --------------------------------------------------
  # Intermediate inputs (in GBP)

  # Table IO 104x104

  z = suppressMessages(
      read_xlsx(file_sut, sheet = paste0("Table 2 - Int Con ", year_i), skip = 3)
    ) %>%
    rename(ons_uk_product = `...1`) %>%
    filter(grepl('CPA',ons_uk_product)) %>% # garde les lignes commençant par CPA_
    mutate(ons_uk_product = sub("^CPA_", "", ons_uk_product)) %>% # Remove CPA_
    column_to_rownames(var = "ons_uk_product") %>%
    # /!\ Exception (different codes used in columns)
    rename(
      `C11.01-6 & C12` = `C1101T1106 & C12`,
      `C241_3` = `C241T243`,
      `F41, F42 & F43` = `F41, F42  & F43`,
      `H493_5` = `H493T495`
    ) %>%
    {.[,-c(1,ncol(.))]} %>% # supprime la première et la dernière colonne
    {
      ids <- sort(intersect(rownames(.), colnames(.)))
      .[ids, ids, drop = FALSE]
    } %>%
    mutate_all(as.numeric)

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
  if (nrow(z) != 104 || ncol(z) != 104) {
    message("[ERROR] Format incorrect pour Z")
    print(z %>% as_tibble())
    stop("Error dans z")
  }
  message("[INFO] Ok - Table Z")

  # --------------------------------------------------
  # Main aggregates (in CAD)

  intermediate_consumptions <- data.frame(
    eeio_industry = colnames(z),
    p2 = colSums(z)
  )

  main_aggregates <- x %>%
    merge(intermediate_consumptions) %>%
    mutate(
      va = x - p2
    ) %>%
    select(eeio_industry, year, unit, x, p2, va) %>%
    arrange(eeio_industry)

  # --------------------------------------------------
  # Coef PRG / GHG Emissions

  # Table 104x1

  url_emissions_data = "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/ukenvironmentalaccountsatmosphericemissionsgreenhousegasemissionsbyeconomicsectorandgasunitedkingdom/current/05atmoshpericemissionsghg.xlsx"

  file_emissions_ons_data = curl_download(url_emissions_data, tempfile())

  emissions_data = suppressMessages(
      read_xlsx(file_emissions_ons_data, sheet = "GHG total", skip = 6, col_names = FALSE, col_types = "text")[,-c(1:26)]
    ) %>%
    {
      headers <- as.character(unlist(.[1,-1], use.names = FALSE))
      headers <- ifelse(grepl("^\\d+\\.\\d{10,}$", headers),
                        format(round(suppressWarnings(as.numeric(headers)), 2), scientific = FALSE, trim = TRUE),
                        headers)
      headers <- sub("\\.$", "", ifelse(grepl("\\.", headers), sub("0+$", "", headers), headers))
      names(.) <- c("year", headers)
      .
    } %>%
    .[-c(1,2), ] %>%
    pivot_longer(-year, names_to = "sic_group", values_to = "value") %>%
    filter(sic_group != "Total", year == year_i, !(sic_group %in% c("100","101"))) %>%
    mutate(
      value = ifelse(
        value == "[low]" | is.na(value),
        NA_real_,
        round(as.numeric(value), 2)
      )
    ) %>%
    left_join(correspondences_sic_groups, by = "sic_group") %>%
    group_by(iot_ref) %>%
    summarise(
      emissions = 1000 * sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rbind(data.frame(iot_ref = "L68A", emissions = 0)) %>% # ajout L68A
    rename(eeio_industry = iot_ref) %>%
    select(eeio_industry, emissions) %>%
    arrange(eeio_industry)

  if (nrow(emissions_data) != 104) {
    message("[ERROR] Format incorrect pour emissions_data")
    print(emissions_data %>% as_tibble())
  }
  message("[INFO] Ok - Table Emissions")

  # --------------------------------------------------
  # Compute footprints

  # Table 104x4 (Aggregates : PRD, IC, GVA, DF)

  ghg_fpt = compute_ghg_fpt("GB", z, main_aggregates, emissions_data, correspondences_figaro, year_i)

  # --------------------------------------------------
  # Monetary conversion

  pound_eur <- from_pound_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt*pound_eur,
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
      eeio_model = "EEIO_UK",
      country = "FR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732) %>%
    arrange(year, code_ape_a732, aggregate)

  # --------------------------------------------------

  print(ghg_fpt_a732 %>% as_tibble())

  return(ghg_fpt_a732)
}

# ----------------------------------------------------------------------------------------------------
# Fetching US EEIO data

get_us_eeio_data = function(year_i, verbose = T)
{
  message("[LOG] Fetching and formatting US EEIO")

  # --------------------------------------------------
  # Metadata

  eeio_size = 398

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

  link = "https://pasteur.epa.gov/uploads/10.23719/1532178/USEEIOv2.5-catbird-22.xlsx"

  # downloading file
  excel_file = link %>%
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

  usd_eur = from_usd_to_euro(YEAR) # coef 1 $ x usd_eur -> 1 €
  # print(usd_eur)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt*usd_eur,
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

  message("[INFO] Ok - Empreintes US EEIO")
  print(ghg_fpt_a732 %>% as_tibble())

  return(ghg_fpt_a732)
}

# ----------------------------------------------------------------------------------------------------
# Fetching CANADA EEIO data

get_canada_eeio_data = function(year_i, verbose = T)
{
  message("[LOG] Fetching and formatting CANADA EEIO")

  # ----------------------------------------------------------------------------------------------------
  # Metadata

  statcan_sectors <- read_delim(
      "disaggregation/eeio_canada/metadata_statcan_sectors.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      statcan_sector = code,
      statcan_sector_niv7 = sector
    ) %>%
    filter(
      !is.na(statcan_sector_niv7)
    ) %>%
    select(statcan_sector, statcan_sector_niv7)

  statcan_sectors_niv7 <- unique(statcan_sectors$statcan_sector_niv7)
  eeio_size = length(statcan_sectors_niv7) # 108

  table_passage_a732 <- read_delim(
      "disaggregation/eeio_canada/table_passage_a732_canada.csv",
      delim = ";",
      na = character(),
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = code_eeio_canada
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
    select(eeio_industry, figaro_industry) %>%
    distinct()

  # --------------------------------------------------
  # EEIO Data

  url_eeio = "https://www150.statcan.gc.ca/n1/tbl/csv/36100001-eng.zip"

  file_eeio_data = curl_download(url_eeio, tempfile()) %>%
    unzip(exdir = tempdir()) %>%
    { .[basename(.) == "36100001.csv"] } %>%
    read.csv()

  # --------------------------------------------------
  # Production (in CAD)

  # Table 234x1 -> 108x1

  x = file_eeio_data %>%
    # parsing data
    mutate(
      Supply_Code  = sub(".*\\[([^]]+)\\]$", "\\1", Supply, perl=TRUE),
      Supply_Label = sub("\\s*\\[[^]]+\\]$", "", Supply, perl=TRUE),
      Use_Code     = sub(".*\\[([^]]+)\\]$", "\\1", Use, perl=TRUE),
      Use_Label    = sub("\\s*\\[[^]]+\\]$", "", Use, perl=TRUE)
    ) %>%
    # filter data
    filter(
      GEO == "Canada",
      REF_DATE == year_i,
      Valuation == "Basic price",
      SCALAR_FACTOR == "thousands"
    ) %>%
    # group data to match emissions classification
    merge(statcan_sectors, by.x = "Use_Code", by.y = "statcan_sector") %>%
    group_by(statcan_sector_niv7) %>%
    summarise(
      x = sum(VALUE, na.rm = TRUE) / 1000,
      .groups = 'drop'
    ) %>%
    # format data
    mutate(
      eeio_industry = statcan_sector_niv7,
      year = year_i,
      unit = "CAD"
    ) %>%
    select(eeio_industry, year, x, unit) %>%
    arrange(eeio_industry)

  if (nrow(x) != eeio_size) {
    message("[ERROR] Format incorrect pour X")
    print(x %>% as_tibble())
  }
  message("[INFO] Ok - Table X")

  # --------------------------------------------------
  # Intermediate inputs (in CAD)

  # Table IO 234x234 -> 108x108

  z = file_eeio_data %>%
    # parsing data
    mutate(
      Supply_Code  = sub(".*\\[([^]]+)\\]$", "\\1", Supply, perl=TRUE),
      Supply_Label = sub("\\s*\\[[^]]+\\]$", "", Supply, perl=TRUE),
      Use_Code     = sub(".*\\[([^]]+)\\]$", "\\1", Use, perl=TRUE),
      Use_Label    = sub("\\s*\\[[^]]+\\]$", "",   Use, perl=TRUE),
      VALUE = if_else(is.na(VALUE), 0, VALUE)
    ) %>%
    # filter data
    filter(
      GEO == "Canada",
      REF_DATE == year_i,
      Valuation == "Basic price",
      SCALAR_FACTOR == "thousands"
    ) %>%
    select(Supply_Code, Use_Code, VALUE) %>%
    # add statcan_sector_niv7 to group data - Supply
    merge(statcan_sectors, by.x = "Supply_Code", by.y = "statcan_sector") %>%
    rename(supply_sector = statcan_sector_niv7) %>%
    select(Supply_Code, supply_sector, Use_Code, VALUE) %>%
    # add statcan_sector_niv7 to group data - Use
    merge(statcan_sectors, by.x = "Use_Code", by.y = "statcan_sector") %>%
    rename(use_sector = statcan_sector_niv7) %>%
    select(Supply_Code, supply_sector, Use_Code, use_sector, VALUE) %>%
    # group data to match emissions data classification
    group_by(supply_sector,use_sector) %>%
    summarise(
      value = sum(VALUE) / 1000,
      .groups = 'drop'
    ) %>%
    # complete table
    complete(
      supply_sector = statcan_sectors_niv7,
      use_sector = statcan_sectors_niv7,
      fill = list(value = 0)
    ) %>%
    # format/build table
    rename(
      use_eeio_industry = use_sector,
      resource_eeio_industry = supply_sector
    ) %>%
    select(resource_eeio_industry, use_eeio_industry, value) %>%
    arrange(resource_eeio_industry, use_eeio_industry) %>%
    pivot_wider(values_from = "value", names_from = "use_eeio_industry") %>%
    column_to_rownames(var = "resource_eeio_industry")

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
  # Main aggregates (in CAD)

  intermediate_consumptions <- data.frame(
    eeio_industry = colnames(z),
    p2 = colSums(z)
  )

  main_aggregates <- x %>%
    merge(intermediate_consumptions) %>%
    mutate(
      va = x - p2
    ) %>%
    select(eeio_industry, year, unit, x, p2, va) %>%
    arrange(eeio_industry)

  # --------------------------------------------------
  # Coef PRG / GHG Emissions

  # Table 108x1

  url_emissions_data = "https://www150.statcan.gc.ca/n1/tbl/csv/38100097-eng.zip"

  file_emissions_ons_data = curl_download(url_emissions_data, tempfile()) %>%
    unzip(exdir = tempdir()) %>%
    { .[basename(.) == "38100097.csv"] } %>%
    read.csv()

  emissions_data = file_emissions_ons_data %>%
    # parsing industry code (statcan)
    mutate(
      statcan_sector = sub(".*\\[([^]]+)\\]$", "\\1", Sector, perl=TRUE),
      statcan_sector_label = sub("\\s*\\[[^]]+\\]$", "", Sector, perl=TRUE),
    ) %>%
    # filter data
    filter(
      GEO == "Canada",
      REF_DATE == year_i,
      UOM == "Kilotonnes",
      SCALAR_FACTOR == "units",
      !is.na(VALUE),
      !is.na(statcan_sector),
      substr(statcan_sector,1,2) %in% c("BS","NP",'GS')
    ) %>%
    # convert emissions in tonnes CO2
    mutate(
      value = VALUE * 1000
    ) %>%
    # format dataframe
    rename(
      eeio_industry = statcan_sector,
      emissions = value
    ) %>%
    select(eeio_industry, emissions) %>%
    arrange(eeio_industry)

  if (nrow(emissions_data) != eeio_size) {
    message("[ERROR] Format incorrect pour emissions_data")
    print(emissions_data %>% as_tibble())
  }
  message("[INFO] Ok - Table Emissions")

  # --------------------------------------------------
  # Compute footprints

  # Table 108x4 (Aggregates : PRD, IC, GVA, DF)

  ghg_fpt <- compute_ghg_fpt("CA", z, main_aggregates, emissions_data, correspondences_figaro, year_i)

  # --------------------------------------------------
  # Monetary conversion

  cad_eur = from_cad_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt*cad_eur,
      unit = "GCO2E_EUR"
    ) %>%
    rename(
      eeio_industry = eeio_industry,
      eeio_country = eeio_country
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
      eeio_model = "EEIO_CANADA",
      country = "FR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732) %>%
    arrange(year, code_ape_a732, aggregate)

  print("Attention : codes EEIO non liés à un code APE")
  codes_manquants <- setdiff(
    statcan_sectors_niv7,
    unique(table_passage_a732$eeio_industry)
  )
  print(tibble::tibble(eeio_industry = codes_manquants))

  # --------------------------------------------------

  print(ghg_fpt_a732 %>% as_tibble())

  return(ghg_fpt_a732)
}

# ----------------------------------------------------------------------------------------------------

####################################################################################################

# Etapes :
#   1- Construction de la matrice des consommations intermédiaires domestiques sur le niveau du modèle EEIO
#   2- Construction des intensités directes
#   3- Calcul des empreintes/facteurs par industrie

compute_ghg_fpt = function(eeio_country, z, main_aggregates, emissions_data, correspondences_figaro, year_i)
{
  # --------------------------------------------------
  # Metadata

  figaro_industries = read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(
      figaro_industry = code
    ) %>%
    select(figaro_industry)

  correspondence <- correspondences_figaro %>%
    group_by(eeio_industry) %>%
    mutate(share = 1 / n()) %>%
    ungroup() %>%
    select(eeio_industry, figaro_industry, share)

  # --------------------------------------------------
  # 1- Construire la matrice des consommations intermédiaires domestiques sur le niveau du modèle EEIO

  # -------------------------
  # Estimation des parts "domestiques" des consommations intermédiaires, à partir des données FR

  # Table 4096x1

  figaro_intermediate_inputs_fr_raw_data <- load_local_figaro_intermediate_inputs(year_i)

  domestic_share_intermediate_inputs_fr = figaro_intermediate_inputs_fr_raw_data %>%
    filter(
      use_country == "FR"
    ) %>%
    group_by(use_country, use_industry, resource_industry) %>%
    summarise(
      total_resources = sum(value, na.rm = TRUE),
      domestic_resources = sum(value[resource_country == "FR"], na.rm = TRUE),
      domestic_share = if_else(total_resources == 0, 0, domestic_resources / total_resources),
      .groups = "drop"
    ) %>%
    select(use_country, use_industry, resource_industry, domestic_share)

  # -------------------------
  # Projection des parts domestiques sur le niveau du modèle EEIO via correspondences_figaro

  # Matrice (size_eeiox64) avec nombre de correspondances (pondérations de ventilation par industrie)

  M <- correspondences_figaro %>%
    # completion pour alignement A*64
    mutate(
      figaro_industry = factor(
        figaro_industry,
        levels = figaro_industries$figaro_industry
      )
    ) %>%
    group_by(eeio_industry) %>%
    mutate(share = 1 / n()) %>%
    ungroup() %>%
    arrange(eeio_industry, figaro_industry) %>%
    xtabs(share ~ eeio_industry + figaro_industry, data = .) %>%
    as.matrix()

  # Matrice (64x64) des parts domestiques des consommations intermédiaires - Nomenclature FIGARO

  D <- domestic_share_intermediate_inputs_fr %>%
    arrange(use_industry, resource_industry) %>%
    xtabs(domestic_share ~ use_industry + resource_industry, data = .) %>%
    as.matrix()

  # Matrice (size_eeioxsize_eeio) des parts domestiques (FR) des consommations intermédiaires - Nomenclature EEIO

  z_domestic_shares <- M %*% D %*% t(M)

  # -------------------------
  # Construction de la matrice des consommations intermédiaires domestiques

  # Matrice (size_eeioxsize_eeio) des consommations intermédiaires domestiques - Nomenclature EEIO

  z_domestic_inputs <- z * z_domestic_shares

  message("[INFO] OK - Z Domestic Inputs")

  # ----------------------------------------------------------------------------------------------------
  # 2- Construction des intensités directes

  # -------------------------
  # Récupération des données FIGARO / données d'émission

  ghg_obs_data_filepath <- file.path("data_output", "accounts_obs_ghg.csv")
  direct_impacts_ghg_raw_data <- read.csv(ghg_obs_data_filepath)

  direct_impacts_ghg_data <- direct_impacts_ghg_raw_data %>%
    filter(year == year_i)

  figaro_main_aggregates_raw_data <- load_local_figaro_main_aggregates(year_i)

  figaro_main_aggregates_data <- figaro_main_aggregates_raw_data %>%
    pivot_wider(
      values_from = "value",
      names_from = "aggregate"
    ) %>%
    select(country, industry, year, PRD)

  # Emissions directes - Nomenclature FIGARO (128x1)

  figaro_impacts_ghg <- direct_impacts_ghg_data %>%
    filter(country %in% c("FR", eeio_country)) %>%
    rename(emissions = value) %>%
    select(country, industry, year, emissions)

  # Production - Nomenclature FIGARO (128x1)

  figaro_prd <- figaro_main_aggregates_data %>%
    filter(country %in% c("FR", eeio_country)) %>%
    rename(X = PRD) %>%
    select(country, industry, year, X)

  # Production GHG intensities - Nomenclature FIGARO (128x1)

  figaro_ghg_intensities <- figaro_impacts_ghg %>%
    merge(figaro_prd) %>%
    mutate(
      ghg_intensity = if_else(X == 0, 0, emissions / X)
    ) %>%
    select(country,industry,year,emissions,X,ghg_intensity)

  # -------------------------
  # Ecart des intensités (all industries)

  gap_ratio_by_industry <- figaro_ghg_intensities %>%
    select(country,industry,year,ghg_intensity) %>%
    # Compute gap ratios
    pivot_wider(names_from = country, values_from = ghg_intensity) %>%
    mutate(gap_ratio = FR / .data[[eeio_country]]) %>%
    mutate(
      figaro_industry = industry,
      figaro_coef_corr = ifelse(is.finite(gap_ratio), gap_ratio, 1.0)
    ) %>%
    select(year,figaro_industry,figaro_coef_corr)

  print(gap_ratio_by_industry, n = 64)

  # -------------------------
  # Ecart des intensités - Industries C

  # Part de la production par industrie, moyenne mondiale (secteur C - 19 industries) - Nomenclature FIGARO

  gap_ratio_C <- figaro_main_aggregates_data %>%
    # Share of each industry (world average)
    filter(substr(industry, 1, 1) == "C") %>%
    group_by(industry) %>%
    summarise(
      value = sum(PRD),
      .groups = "drop"
    ) %>%
    mutate(share = value / sum(value)) %>%
    select(industry, share) %>%
    # Compute average ghg intensity for FR & EEIO country
    merge(figaro_ghg_intensities) %>%
    group_by(country) %>%
    summarise(
      intensity_C = sum(ghg_intensity * share),
      .groups = "drop"
    ) %>%
    select(country, intensity_C) %>%
    # Compute gap ratio
    pivot_wider(names_from = country, values_from = intensity_C) %>%
    mutate(ratio = FR / .data[[eeio_country]]) %>%
    pull(ratio)

  # -------------------------
  # Ecart des intensités - Industries D35

  gap_ratio_D35 <- figaro_ghg_intensities %>%
    filter(industry == "D35") %>%
    select(country, ghg_intensity) %>%
    # Compute gap ratio
    pivot_wider(names_from = country, values_from = ghg_intensity) %>%
    mutate(ratio = FR / .data[[eeio_country]]) %>%
    pull(ratio)

  # -------------------------
  # Coefficients correcteurs - Nomenclature EEIO

  ghg_intensities_corr = figaro_industries %>%
    merge(gap_ratio_by_industry) %>%
    # mutate(
    #   figaro_coef_corr = case_when(
    #     substr(figaro_industry, 1, 1) == "C" ~ gap_ratio_C,
    #     figaro_industry == "D35"             ~ gap_ratio_D35,
    #     TRUE                                      ~ 1
    #   )
    # ) %>%
    merge(correspondence) %>%
    group_by(eeio_industry) %>%
    summarise(
      coef_corr = sum(figaro_coef_corr * share),
      .groups = "drop"
    ) %>%
    select(eeio_industry, coef_corr)

  # GHG intensities by FIGARO industry
  eeio_ghg_intensities <- emissions_data %>%
    merge(main_aggregates) %>%
    merge(correspondence) %>%
    group_by(figaro_industry) %>%
    summarise(
      emissions = sum(emissions * share),
      x = sum(x * share),
      .groups = "drop"
    ) %>%
    mutate(
      industry = figaro_industry,
      eeio_ghg_intensity = if_else(x > 0, (emissions / x), 0)
    ) %>%
    select(industry,eeio_ghg_intensity)

  ghg_intensities_corr_bis <- figaro_ghg_intensities %>%
    filter(country == "FR") %>%
    merge(eeio_ghg_intensities) %>%
    mutate(
      figaro_industry = industry,
      coef_corr = if_else(eeio_ghg_intensity > 0, ghg_intensity / eeio_ghg_intensity, 1.0)
    ) %>%
    select(figaro_industry,coef_corr) %>%
    merge(correspondence) %>%
    group_by(eeio_industry) %>%
    summarise(
      coef_corr = sum(coef_corr * share),
      .groups = "drop"
    ) %>%
    # filter(is.finite(coef_corr)) %>%
    select(eeio_industry, coef_corr)

  print(ghg_intensities_corr_bis %>% as_tibble())

  # --------------------------------------------------
  # Calcul des intensités d'émission (avec corrections)

  direct_ghg_intensity <- emissions_data %>%
    merge(main_aggregates) %>%
    left_join(ghg_intensities_corr_bis, by = "eeio_industry") %>%
    mutate(
      coef_corr = coalesce(coef_corr, 1),
      ghg_intensity = if_else(x > 0, (emissions / x) * coef_corr, 0)
    ) %>%
    select(eeio_industry, ghg_intensity)

  impact_vector <- direct_ghg_intensity %>%
    arrange(eeio_industry) %>%
    pull(ghg_intensity) %>%
    as.numeric()

  # ----------------------------------------------------------------------------------------------------
  # 3- Calcul des empreintes/facteurs par industrie

  A <- sweep(z_domestic_inputs, 2, main_aggregates$x, "/") %>% as.matrix()
  A[is.nan(A) | is.infinite(A)] = 0 ; diag(A)[diag(A) == 1] = 0.995
  I <- diag(1, nrow(A))
  L <- solve(I - A)

  # --------------------------------------------------
  # Empreintes - Demande finale

  fpt_fd_data <- as.numeric(t(impact_vector) %*% L)  # empreinte par unité de demande finale
  fpt_fd <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "FD",
    fpt = fpt_fd_data,
    year = year_i
  )

  # print("Empreintes FD ok")
  # print(fpt_fd %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Production

  fpt_prd_data  <- fpt_fd_data / diag(L)                      # conversion en "par unité de production"
  fpt_prd <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "PRD",
    fpt = fpt_prd_data,
    year = year_i
  )

  # print("Empreintes PRD ok")
  # print(fpt_prd %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Valeur ajoutée brute

  fpt_gva_data <- (impact_vector * main_aggregates$x) / main_aggregates$va
  fpt_gva <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "GVA",
    fpt = fpt_gva_data,
    year = year_i
  )

  # print("Empreintes GVA ok")
  # print(fpt_gva %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Consommations intermédiaires

  fpt_ic_data <- (fpt_prd_data * main_aggregates$x - fpt_gva_data * main_aggregates$va) / main_aggregates$p2
  fpt_ic <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "IC",
    fpt = fpt_ic_data,
    year = year_i
  )

  # print("Empreintes IC ok")
  # print(fpt_ic %>% as_tibble())

  # --------------------------------------------------
  # 8- Retour

  results <- fpt_fd %>%
    rbind(fpt_prd) %>%
    rbind(fpt_gva) %>%
    rbind(fpt_ic)

  # print(results %>% as_tibble())

  return(results)
}

####################################################################################################
# FIGARO IMPORTED EMBEDDED EMISSIONS

get_figaro_imported_embedded_emissions = function(year_i)
{
  # --------------------------------------------------
  # Fetch FIGARO Model

  # Intermediate inputs
  Z <- load_local_figaro_intermediate_inputs(year_i)

  # Production
  X <- load_local_figaro_main_aggregates(year_i) %>%
    filter(aggregate == "PRD") %>%
    rename(
      x = value
    ) %>%
    select(country, industry, year, x)

  # Emissions
  ghg_obs_data_filepath <- file.path("data_output", "accounts_obs_ghg.csv")
  direct_impacts_ghg_raw_data <- read.csv(ghg_obs_data_filepath)
  E <- direct_impacts_ghg_raw_data %>%
    filter(year == year_i) %>%
    rename(
      emissions = value
    ) %>%
    select(country, industry, year, emissions)

  # --------------------------------------------------
  # Compute footprint

  C = E %>%
    merge(X) %>%
    mutate(
      value = case_when(
        country == "FR" ~ 0,
        TRUE ~ replace_na(emissions / x, 0)
      )
    ) %>%
    pull(value) %>%
    as.numeric()

  A = Z %>%
    merge(X, by.x = c("use_country","use_industry"), by.y = c("country","industry")) %>%
    mutate(
      value = if_else(x > 0, value / x, 0)
    ) %>%
    arrange(use_country, use_industry, resource_country, resource_industry) %>%
    select(use_id, resource_id, value) %>%
    pivot_wider(
      names_from = "use_id",
      values_from = "value"
    ) %>%
    column_to_rownames("resource_id")

  L = solve(diag(nrow = nrow(A)) - A)

  fpt_raw_data = sweep(sweep(L, 2, diag(L), `/`), 1, C, `*`)

  fpt_data <- fpt_raw_data %>%
    as.data.frame() %>%
    rownames_to_column("resource_id") %>%
    pivot_longer(
      cols = -resource_id,
      names_to = "use_id",
      values_to = "fpt"
    ) %>%
    mutate(
      use_country  = sub("_.*$", "", use_id),
      use_industry = sub("^[^_]*_", "", use_id),
      resource_country  = sub("_.*$", "", resource_id),
      resource_industry = sub("^[^_]*_", "", resource_id),
      year = year_i
    )

  # --------------------------------------------------
  # Imported fpt for FR + A*732

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

  imported_fpt_fr <- fpt_data %>%
    filter(
      use_country == "FR",
      resource_country != "FR",
      year == year_i
    ) %>%
    group_by(use_country, use_industry, year) %>%
    summarise(
      imported_fpt = sum(fpt),
      .groups = "drop"
    ) %>%
    rename(
      country = use_country,
      figaro_industry = use_industry
    ) %>%
    merge(metadata_nace_niv5) %>%
    select(code_ape_a732, year, imported_fpt)

  return(imported_fpt_fr)
}

####################################################################################################
# ESANE DATA

fetch_esane_data = function()
{
  url_esane_data = "https://www.insee.fr/fr/statistiques/fichier/8241021/DD_esane22ep_caracteristiques.xlsx"

  file_esane_data = curl_download(url_esane_data, tempfile())

  esane_raw_data = file_esane_data %>%
    read_xlsx(
      skip = 10,
      col_types = "text"
    ) %>%
    rename(
      niveau_naf = `NIVEAU NAF`,
      secteur_activite = `Secteur d'activité`,
      chiffre_affaires_ht = `Chiffre d'affaires Hors Taxes`,
      valeur_ajoutee = `Valeur ajoutée - y compris autres produits et autres charges`
    ) %>%
    mutate(across(-c(niveau_naf,secteur_activite), as.numeric)) %>%
    filter(
      !is.na(valeur_ajoutee),
      chiffre_affaires_ht > 0
    ) %>%
    reframe(
      niveau_naf,
      secteur_activite,
      taux_va = valeur_ajoutee / chiffre_affaires_ht
    ) %>%
    arrange(secteur_activite)

  # -------------------------
  # A*732 Format

  metadata_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    mutate(
      code_ape_a732 = code,
      nace_niv5 = gsub("\\.", "", code),
      nace_niv4 = gsub("\\.", "", classe),
      nace_niv3 = gsub("\\.", "", groupe),
      nace_niv2 = division,
      sector = section
    ) %>%
    filter(code_ape_a732 != "00.00Z") %>%
    select(code_ape_a732, nace_niv5, nace_niv4, nace_niv3, nace_niv2, sector)

  data_esane_a732 <- esane_raw_data %>% filter(niveau_naf == "a732") %>% reframe(nace_niv5 = secteur_activite, taux_va_a732 = taux_va)
  data_esane_a615 <- esane_raw_data %>% filter(niveau_naf == "a615") %>% reframe(nace_niv4 = secteur_activite, taux_va_a615 = taux_va)
  data_esane_a272 <- esane_raw_data %>% filter(niveau_naf == "a272") %>% reframe(nace_niv3 = secteur_activite, taux_va_a272 = taux_va)
  data_esane_a88  <- esane_raw_data %>% filter(niveau_naf == "a88")  %>% reframe(nace_niv2 = secteur_activite, taux_va_a88 = taux_va)
  data_esane_a21  <- esane_raw_data %>% filter(niveau_naf == "a21")  %>% reframe(sector = secteur_activite, taux_va_a21 = taux_va)
  # data_esane_a10  <- esane_raw_data %>% filter(niveau_naf == "a10")  %>% reframe(nace_a10 = secteur_activite, taux_va_a10 = taux_va)

  data_esane = metadata_nace_niv5 %>%
    left_join(data_esane_a732, by = "nace_niv5") %>%
    left_join(data_esane_a615, by = "nace_niv4") %>%
    left_join(data_esane_a272, by = "nace_niv3") %>%
    left_join(data_esane_a88,  by = "nace_niv2") %>%
    left_join(data_esane_a21,  by = "sector") %>%
    # left_join(data_esane_a10,  by = "nace_a10") %>%
    mutate(
      taux_va = coalesce(
        taux_va_a732,
        taux_va_a615,
        taux_va_a272,
        taux_va_a88,
        taux_va_a21
        # taux_va_a10
      )
    ) %>%
    select(code_ape_a732, taux_va) %>%
    arrange(code_ape_a732)

  return(data_esane)
}

####################################################################################################
# FIGARO DATA

fetch_figaro_data = function(year_i)
{
  macro_fpt_data_filepath <- file.path("data_output", "footprints_obs_ghg.csv")
  figaro_macro_fpt_raw_data <- read.csv(macro_fpt_data_filepath)

  figaro_macro_fpt <- figaro_macro_fpt_raw_data %>%
    filter(
      year == year_i,
      country == "FR"
    ) %>%
    rename(
      fpt = value,
      figaro_industry = industry
    ) %>%
    select(country, figaro_industry, aggregate, year, fpt)

  return(figaro_macro_fpt)
}

####################################################################################################
# INDICES PRIX

fetch_na_prices = function(year_i)
{
  na_prices_raw = dbGetQuery(conn, paste0(
    "SELECT * ",
    "FROM macrodata.na_prices ",
    "WHERE year >= '",year_i,"'"
  ))

  na_prices <- na_prices_raw %>%
    filter(aggregate == "P1") %>%
    mutate(
      price_index = value,
      figaro_industry = industry
    ) %>%
    select(figaro_industry, year, price_index)

  base_indexes <- na_prices %>%
    filter(year == year_i) %>%
    mutate(
      base_index = price_index,
      base_year = year
    ) %>%
    select(figaro_industry, base_year, base_index)

  prices_indexes <- na_prices %>%
    merge(base_indexes) %>%
    mutate(
      coef_price_index = base_index / price_index
    ) %>%
  select(figaro_industry, year, base_year, coef_price_index) %>%
  arrange(figaro_industry, year)

  return(prices_indexes)
}

####################################################################################################
# TRAITEMENT

# ----------------------------------------------------------------------------------------------------
# Building EEIO Data

# Fetching UK data
if (!use_temp_data) {
  data_uk_eeio <<- get_uk_eeio(YEAR)
  message("[INFO] Ok - Empreintes EEIO UK")
  write.csv(data_uk_eeio, file = "disaggregation/data_temp/data_uk_eeio.csv", row.names = FALSE)
}
data_uk_eeio <- read_delim("disaggregation/data_temp/data_uk_eeio.csv", delim = ",", show_col_types = FALSE)

# Fetching US data
if (!use_temp_data) {
  data_us_eeio <<- get_us_eeio_data(YEAR)
  message("[INFO] Ok - Empreintes EEIO US")
  write.csv(data_us_eeio, file = "disaggregation/data_temp/data_us_eeio.csv", row.names = FALSE)
}
data_us_eeio <- read_delim("disaggregation/data_temp/data_us_eeio.csv", delim = ",", show_col_types = FALSE)

# Fetching CANADA data
if (!use_temp_data) {
  data_canada_eeio <<- get_canada_eeio_data(YEAR)
  message("[INFO] Ok - Empreintes EEIO CANADA")
  write.csv(data_canada_eeio, file = "disaggregation/data_temp/data_canada_eeio.csv", row.names = FALSE)
}
data_canada_eeio <- read_delim("disaggregation/data_temp/data_canada_eeio.csv", delim = ",", show_col_types = FALSE)

# Fetching FIGARO data
data_figaro <<- fetch_figaro_data(YEAR)
message("[INFO] Ok - Empreintes EEIO FIGARO")

# ----------------------------------------------------------------------------------------------------
# Building Imported embedded emissions

if (!use_temp_data) {
  imported_fpt_fr <- get_figaro_imported_embedded_emissions(YEAR)
  message("[INFO] Ok - Empreintes (Importations)")
  write.csv(imported_fpt_fr, file = "disaggregation/data_temp/imported_fpt_fr.csv", row.names = FALSE)
}
imported_fpt_fr <- read_delim("disaggregation/data_temp/imported_fpt_fr.csv", delim = ",", show_col_types = FALSE)

# ----------------------------------------------------------------------------------------------------
# Fetching ESANE Data

if (!use_temp_data) {
  data_esane <- fetch_esane_data()
  message("[INFO] Ok - Données ESANE")
  write.csv(data_esane, file = "disaggregation/data_temp/data_esane.csv", row.names = FALSE)
}
data_esane <- read_delim("disaggregation/data_temp/data_esane.csv", delim = ",", show_col_types = FALSE)

# ----------------------------------------------------------------------------------------------------
# Fetching prices data

data_prices <<- fetch_na_prices(YEAR)
print(data_prices %>% as_tibble())
print(unique(data_prices$year))
message("[INFO] Ok - Indices prix")

# ----------------------------------------------------------------------------------------------------
# Build A*732 Data

# --------------------------------------------------
# Metadata A*732

metadata_nace_niv5 <- read_delim(
    "metadata/metadata_nace_niv5.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
  rename(
    code_ape_a732 = code,
    libelle_ape_a732 = label_fr,
    figaro_industry = industry
  ) %>%
  select(code_ape_a732, figaro_industry, libelle_ape_a732)

# --------------------------------------------------
# EEIO data Compilation

data_figaro_eeio <- metadata_nace_niv5 %>%
  merge(data_figaro) %>%
  mutate(
    eeio_model = "FIGARO",
    accuracy_mapping_a732 = "4",
    unit = "GCO2E_EUR"
  ) %>%
  select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732)

data_eeio_models <- data_figaro_eeio %>%
  # -------------------------
  # bind EEIO fpt data
  rbind(data_us_eeio) %>%
  rbind(data_uk_eeio) %>%
  rbind(data_canada_eeio) %>%
  # -------------------------
  # metadata A*732
  merge(metadata_nace_niv5) %>%
  # -------------------------
  # add imported embedded fpt
  merge(imported_fpt_fr) %>% # by figaro_industry
  mutate(
    fpt = if_else(eeio_model == "FIGARO", fpt, fpt + imported_fpt) # /!\ FIGARO imported embedded fpt included
  ) %>%
  # -------------------------
  # filtrage
  group_by(eeio_model, figaro_industry, aggregate, year) %>%
  mutate(
    fpt_min_industry = min(fpt),
    fpt_max_industry = max(fpt),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  merge(data_figaro %>% rename(fpt_ref_industry = fpt)) %>%
  filter(
    accuracy_mapping_a732 < 5            # ignore data less relevant than FIGARO
    # fpt_min_industry <= fpt_ref_industry, # fpt -> ref FIGARO
    # fpt_max_industry >= fpt_ref_industry  # fpt -> ref FIGARO
  ) %>%
  # -------------------------
  # add coef
  mutate(
    coef_accuracy = case_when(
      accuracy_mapping_a732 == "1"  ~ 8, # 5
      accuracy_mapping_a732 == "2"  ~ 5, # 3
      accuracy_mapping_a732 == "3"  ~ 2, # 2
      accuracy_mapping_a732 == "4"  ~ 1,
      T ~ 0
    )
  ) %>%
  # -------------------------
  # ouput
  select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732, coef_accuracy)

# --------------------------------------------------
# EEIO data agregation

nace_a732_fpt <- metadata_nace_niv5 %>%
  crossing(aggregate = c("FD", "PRD", "IC", "GVA")) %>%
  mutate(year = YEAR) %>%
  # merge EEIO models fpt
  merge(data_eeio_models) %>%
  filter(fpt > 0) %>% # /!\ remove negative fpt (temp)
  group_by(code_ape_a732, aggregate, unit, year) %>%
  summarise(
    fpt = sum(fpt * coef_accuracy) / sum(coef_accuracy),
    accuracy_fpt = sum(as.numeric(accuracy_mapping_a732) * coef_accuracy) / sum(coef_accuracy),
    .groups = "drop"
  ) %>%
  {
    print(as_tibble(.))
   .
  } %>%
  # compute prd fpt with ESANE data
  filter(aggregate %in% c("GVA", "IC", "PRD")) %>%
  # Actualisation des empreintes
  rename(base_year = year) %>%
  merge(data_prices %>% filter(year == "2022")) %>%
  mutate(
    fpt = fpt * coef_price_index
  ) %>%
  # output
  mutate(
    fpt = round(fpt, 0),
    accuracy_fpt = round(accuracy_fpt, 1)
  ) %>%
  merge(metadata_nace_niv5) %>%
  select(year, code_ape_a732, aggregate, fpt, accuracy_fpt, unit, libelle_ape_a732)

message("Traitement terminé")
print(nace_a732_fpt %>% as_tibble())
write.csv(nace_a732_fpt, file = "disaggregation/data_temp/nace_a732_fpt.csv", row.names = FALSE)

if (do_update)
{
  formatted_data <- nace_a732_fpt %>%
    mutate(
      country = "FR",
      indic = "GHG",
      value = fpt,
      accuracy_index = accuracy_fpt,
      flag = "",
      currency = "CPEUR",
      lastupdate = Sys.Date()
    ) %>%
    select(year, country, code_ape_a732, aggregate, indic, value, accuracy_index, flag, currency, lastupdate)

  message("Push data")

  dbExecute(conn, paste0(
    "DELETE FROM macrodata.macro_fpt_a732 "
  ))

  dbWriteTable(conn, SQL("macrodata.macro_fpt_a732"), formatted_data, append = T)

  message("Data updated")
}

####################################################################################################
x = c('tidyverse','curl','httr2','rvest','readxl')
lapply(x,library,character.only = T)
get_denmark_eeio_data = function()
{

  # ----------------------------------------------------------------------------------------------------
  # Metadata

  eeio_size = 117

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
    select(eeio_industry, figaro_industry) %>%
    distinct()

  #get_eeio_data

  year = 2018

  dst.url = 'https://www.dst.dk/en/Statistik/emner/oekonomi/nationalregnskab/input-output'

  dst.urls = read_html(dst.url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    subset(grepl('Excel-files',.)) %>%
    subset(!grepl('69-industries',.))


  dst.time_span = lapply(dst.urls, function(x) {
    matches <- regmatches(x, gregexpr("(?<=-)\\d{4}(?=-)", x, perl = TRUE)) %>% unlist()
    as.numeric(matches[1]):as.numeric(matches[2])
  })

  dst.url_io = file.path("https://www.dst.dk",
                         dst.urls[which(sapply(dst.time_span, function(seq) year %in% seq))])

  dst.files_io = curl_download(dst.url_io,tempfile())

  dst.file_io = unzip(dst.file_io,list = T) %>%
    filter(grepl(year,Name)) %>%
    pull(Name)

  dst.excel_io = unzip(dst.files_io,files = dst.file_io,exdir = tempdir())

  dst.excel_sheet = read_xlsx(dst.excel_io,sheet = "IO",skip = 2) %>%
    {.[-1,c(1,3:(3+116))]}

  dst.excel_sheet_formatted =
    dst.excel_sheet %>%
    rename('product' = `From/To`) %>%
    mutate(Origin = case_when(n() < which(dst.excel_sheet$`From/To` == 'Imports') ~ 'Domestic',
                              T ~ 'Imports')) %>%
    filter(product != 'Imports') %>%
    relocate(product,Origin,.before = 1)

  message("[LOG] Fetching and formatting CANADA EEIO")



}


summarise_foreign_options = function()
{
metadata_paths = list.files(here::here('disaggregation'),recursive = T,pattern = 'eeio_industries.csv',full.names = T)

metadata_labels = map_dfr(metadata_paths,
                          ~ read.csv(.x, sep = ";",colClasses = 'character') %>%
                            select(contains("label"),contains("code")) %>%
                            rename(LABEL = 1,CODE = 2) %>%
                            mutate(COUNTRY = toupper(gsub("eeio_", "", basename(dirname(.x)))),
                                   LABEL = trimws(LABEL)))

correspondence_paths = list.files(here::here('disaggregation'),recursive = T,pattern = 'table_passage',full.names = T)


correspondence_references = map_dfr(correspondence_paths,
                                    ~ read.csv(.x,sep = ";",colClasses = 'character') %>%
                                      select(code_ape_a732,code_eeio = contains('code_eeio'))) %>%
  distinct()


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

correspondences_figaro_top_level <- correspondence_references %>%
  merge(metadata_nace_niv5) %>%
  distinct(code_eeio,figaro_industry) %>%
  left_join(metadata_labels,by = c('code_eeio' = 'CODE'),relationship = "many-to-many")

formatted = correspondences_figaro_top_level %>%
  filter(code_eeio != "") %>%
  group_by(figaro_industry) %>%
  mutate(
    option_nb = row_number(),  # Numérote les options par figaro_industry
    combined = paste(code_eeio, LABEL, COUNTRY, sep = " | ")
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = option_nb,       # 1 ligne par numéro d'option
    names_from = figaro_industry,
    values_from = combined
  ) %>%
  select(!option_nb)

writexl::write_xlsx(formatted,here::here('disaggregation/option_summary.xlsx'))

return(formatted)

}
