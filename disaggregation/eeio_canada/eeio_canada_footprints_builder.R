# La Société Nouvelle

####################################################################################################
# UTILS

source("disaggregation/utils.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# BUILDING CANADA EEIO DATA

build_canada_eeio_footprints <<- function(year_i, verbose = TRUE)
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

  url_eeio <- "https://www150.statcan.gc.ca/n1/tbl/csv/36100001-eng.zip"

  file_eeio_data <- curl_download(url_eeio, tempfile()) %>%
    unzip(exdir = tempdir()) %>%
    { .[basename(.) == "36100001.csv"] } %>%
    read.csv()

  # --------------------------------------------------
  # Production (in CAD)

  # Table 234x1 -> 108x1

  x <- file_eeio_data %>%
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

  z <- file_eeio_data %>%
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

  url_emissions_data <- "https://www150.statcan.gc.ca/n1/tbl/csv/38100097-eng.zip"

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

  cad_eur <- from_cad_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt / cad_eur,
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

  write.csv(
    ghg_fpt_a732,
    file = "disaggregation/data_temp/footprints_canada_eeio.csv",
    row.names = FALSE
  )
  message("[INFO] Ok - Empreintes EEIO CANADA")
}