# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Environmental accounts builder for water consumption (WAT)
#'
#' Main sources :
#'   - Water use and supply (OECD)
#'   - Consumption coefficients (Publications France Stratégie)
#'       Source : https://www.strategie-plan.gouv.fr/files/files/Publications/Rapport/fs-2024-na_136_annexe_methodologique_avril.pdf
#'
#' Output data
#'   Accounts are in thousands of m3 (to match MEUR)
#'
#' Missing values filled by proxy using industry and country similarity.#
#'
#' build_wat_obs_accounts()

build_wat_obs_accounts <- function(
  years = 2010:2022, # OECD available since 1990
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build WAT accounts for observed data")
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

  coefficients_consommation <- read_delim(
      "obs_accounts/wat/coefficients_consommation.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      industry = figaro_industry
    ) %>%
    select(industry, coef_consumption)

  table_passage_ocde_data <- read_delim(
      "obs_accounts/wat/table_passage_ocde.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      industry = figaro_industry
    ) %>%
    select(industry, oecd_industry)

  if (verbose) cat("Metadata loaded\n")

  # -------------------------------------------------------------------
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
  # Fetch OECD data

  if (verbose) cat("Loading OECD data...\n")

  # Water accounts - supply and use

  base_url_oecd_data = "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WATER_PSUT@DF_WATER_PSUT,1.0/.A.SURFACE_GROUND..USE.SEC.?"
  url_oecd_data = paste0(base_url_oecd_data,
    "startPeriod=",min(years$year),
    # "&endPeriod=",max(years$year),
    "&dimensionAtObservation=","AllDimensions",
    "&format=","csvfilewithlabels"
  )

  oecd_file_path  <- file.path(download_dir, "DSD_WATER_PSUT.csv")

  if (!file.exists(oecd_file_path) | !use_temp_data)
  {
    if (verbose) cat("Downloadding OECD data...\n")
    oecd_raw_data <- read.csv(url_oecd_data)

    write.csv(oecd_raw_data, oecd_file_path, row.names = FALSE)
  }

  oecd_raw_data <- read.csv(oecd_file_path)

  oecd_data <- oecd_raw_data %>%
    filter(
      VARIABLE == "SEC",
      SUPPLY_USE == "USE",
      MEASURE == "SURFACE_GROUND",
      FREQ == "A",
      UNIT_MEASURE == "M3", # m3
      UNIT_MULT == 6 # millions
    ) %>%
    mutate(
      REF_AREA = countrycode(REF_AREA, 'iso3c', 'iso2c', nomatch = NULL)
    ) %>%
    # format
    mutate(
      year = as.character(TIME_PERIOD),
      country = REF_AREA,
      oecd_industry = ACTIVITY,
      water_withdrawal = round(OBS_VALUE * 1e3, digits = 0),
      unit = "THS_M3",
    ) %>%
    select(year,country,oecd_industry,water_withdrawal,unit)

  if (verbose) cat("OECD data loaded\n")

  # -------------------------------------------------------------------
  # Building WAT impact vector

  if (verbose) cat("Building FIGARO accounts...\n")

  # use VA to split impacts between FIGARO industries
  va_distribution_oecd_industries <- table_passage_ocde_data %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, oecd_industry) %>%
    mutate(
      share_oecd_industry = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, oecd_industry, share_oecd_industry)

  raw_wat_accounts <- oecd_data %>%
    merge(va_distribution_oecd_industries) %>%
    merge(coefficients_consommation) %>%
    mutate(
      value = round(water_withdrawal * share_oecd_industry * coef_consumption, digits = 0),
      flag = "",
    ) %>%
    select(year, country, industry, value, flag)

  # -------------------------
  # Accounts data

  figaro_wat_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_wat_accounts,
      by = c("year", "country", "industry")
    ) %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("Completing with similarity...\n")

  # Complete with similarity
  figaro_wat_accounts <- figaro_wat_accounts_raw %>%
    proxy_missing_value_by_similarity(., "WAT") %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("Cleaning outliers...\n")

  # Clean outliers
  figaro_wat_accounts <- figaro_wat_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
    clean_outliers(
      .,
      serie_pkey = c("country", "industry"),
      verbose = TRUE
    ) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_wat_accounts) != size) {
    error_data <<- figaro_wat_accounts
    stop("ERROR - Wrong size for obs accounts (WAT)")
  } else if (any(is.na(figaro_wat_accounts$value))) {
    error_data <<- figaro_wat_accounts
    stop("ERROR - NA values in obs accounts (WAT)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_wat_accounts %>%
    mutate(
      serie_id    = "wat_obs",
      value       = round(value, digits = 0),
      unit        = "M3",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_wat.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
