# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for hazardous substances use (HAZ)
#'
#' Main sources :
#'   - PRODCOM (Eurostat)
#'
#' Output data
#'   Accounts are in tonnes
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_haz_obs_accounts()

build_haz_obs_accounts <- function(
  years = 2010:2023,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build HAZ accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  years <- tibble(year = as.character(years))

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(
      industry = code
    ) %>%
    select(industry)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      country = code
    ) %>%
    select(country)

  haz_products <- read.csv(
    "obs_accounts/haz/reference-substances.txt",
    sep = "\t",
    check.names = FALSE,
    colClasses = "character"
  )

  prodcom_countries <- read_delim(
      "obs_accounts/haz/prodcom_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(geonum, country)

  # -------------------------------------------------------------------
  if (verbose) cat("Metadata loaded\n")

  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # FIGARO Economic data (C20 ressources)

  intermediate_inputs_data_raw <- map_dfr(
    years$year,
    load_local_figaro_intermediate_inputs
  )

  intermediate_inputs_data <- intermediate_inputs_data_raw %>%
    filter(resource_industry != "C20") %>%
    select(year, use_country, use_industry, resource_country, resource_industry, value)

  # -------------------------------------------------------------------
  # PRODCOM data (Eurostat)

  base_url_prodcom_data <- "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-059358/1.0?"
  url_prodcom_data = paste0(base_url_prodcom_data,
    "format=","csvdata",
    "&c[freq]=","A",
    "&c[TIME_PERIOD]=",paste(years$year, collapse = ","),
    "&c[indicators]=","PRODQNT,QNTUNIT",
    "&compressed=","true"
  )

  prodcom_file_path  <- file.path(download_dir, "ds-059358.csv")

  if (!file.exists(prodcom_file_path) || !use_temp_data)
  {
    if (verbose) print("Téléchargement données PRODCOM...")

    prodcom_raw_data <- fread(
      url_prodcom_data,
      colClasses = "character",
      sep = ","
    )

    if (verbose) print("Données PRODCOM téléchargées")
    write.csv(prodcom_raw_data, prodcom_file_path, row.names = FALSE)
  }

  prodcom_raw_data <- read.csv(prodcom_file_path)

  prodcom_data <- prodcom_raw_data %>%
    pivot_wider(
      names_from = indicators,
      values_from = OBS_VALUE
    ) %>%
    filter(
      QNTUNIT == "KG",
      TIME_PERIOD %in% years$year,
      freq == "A"
    ) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      country = reporter,
      value = as.numeric(PRODQNT),
      unit = QNTUNIT
    ) %>%
    select(year, country, product, value, unit)

  # -------------------------------------------------------------------

  # hazardous substances production by country (Tonnes)

  haz_production <- prodcom_data %>%
    filter(
      product %in% haz_products$`PRODCOM Code`
    ) %>%
    group_by(year, country) %>%
    summarise(
      haz_production = round(sum(value / 1e3, na.rm = TRUE), digits = 1),
      unit = "T"
    ) %>%
    select(year, country, haz_production, unit)

  # hazardous substances use by country & industry (Tonnes)

  resource_c20_shares <- figaro_resource_c20 %>%
    group_by(year, resource_country, resource_industry) %>%
    mutate(
      total_resource_c20 = sum(value, na.rm = TRUE),
      share = value / total_resource_c20
    ) %>%
    ungroup() %>%
    select(year, use_country, use_industry, resource_country, share)

  haz_use <- haz_production %>%
    rename(resource_country = country) %>%
    merge(resource_c20_shares) %>% # by year, resource_country
    mutate(
      haz_use = haz_production * share
    ) %>%
    group_by(year, use_country, use_industry) %>%
    summarise(
      haz_use = sum(haz_use, na.rm = TRUE) # sum over resource country
    ) %>%
    rename(
      country = use_country,
      industry = use_industry
    ) %>%
    select(year, country, industry, haz_use)

  # HAZ accounts data for each FIGARO Country/Industry

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_haz_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(haz_use) %>%
    mutate(
      value = round(haz_use, digits = 0),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_haz_accounts <- figaro_haz_accounts_raw %>%
    proxy_missing_value_by_similarity(., "HAZ") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  figaro_haz_accounts <- figaro_haz_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_haz_accounts) != size) {
    error_data <<- figaro_haz_accounts
    stop("ERROR - Wrong size for obs accounts (HAZ)")
  } else if (any(is.na(figaro_haz_accounts$value))) {
    error_data <<- figaro_haz_accounts
    stop("ERROR - NA values in obs accounts (HAZ)")
  }

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <- figaro_haz_accounts %>%
    mutate(
      serie_id    = "haz_obs",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  # -------------------------------------------------------------------
  if (verbose) print(formatted_data %>% as_tibble())

  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_haz.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
