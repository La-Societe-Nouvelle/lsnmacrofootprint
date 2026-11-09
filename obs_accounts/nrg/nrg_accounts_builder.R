# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Environmental FIGARO accounts builder for energy consumptions (NRG)
#'
#' Main sources :
#'   - Energy supply and use by NACE Rev. 2 activity (Eurostat)
#'      Source : https://ec.europa.eu/eurostat/databrowser/view/ENV_AC_PEFASU/default/table?lang=fr
#'
#' Output data
#'   Accounts are in GJ (gigajoules)
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_nrg_obs_accounts()

build_nrg_obs_accounts <- function(
  years = 2014:2023,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build NRG accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_imputations.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/nrg/nrg_accounts_sources.R", encoding = "UTF-8-BOM")

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
  # EUROSTAT data

  if (verbose) cat("Loading EUROSTAT data...\n")

  eurostat_data <- load_eurostat_nrg_source(years$year, use_temp_data, verbose)

  if (verbose) cat("EROSTAT data loaded\n")

  # -------------------------------------------------------------------
  # JRC data (2015)

  if (verbose) cat("Loading JRC data...\n")

  jrc_data <- load_jrc_nrg_source(years$year, use_temp_data, verbose)

  if (verbose) cat("JRC data loaded\n")

  # -------------------------------------------------------------------

  jrc_corr <- jrc_data %>%
    merge(eurostat_data) %>%
    mutate(
      coef = value / nrg_use
    ) %>%
    select(country,industry,coef)

  # -------------------------
  # Accounts data based on Eurostat data

  eurostat_nrg_accounts <- eurostat_data %>%
    filter(nrg_use >= 0) %>%
    mutate(
      eurostat_value = nrg_use,
      eurostat_flag = ""
    ) %>%
    select(year, country, industry, eurostat_value, eurostat_flag)

  # -------------------------
  # Accounts data

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_nrg_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      eurostat_nrg_accounts,
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      value = if_else(!is.na(eurostat_value), eurostat_value, NA),
      flag = if_else(!is.na(eurostat_flag), eurostat_flag, NA),
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_nrg_accounts <- figaro_nrg_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "NRG", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_nrg_accounts) != size) {
    error_data <<- figaro_nrg_accounts
    stop("ERROR - Wrong size for obs accounts (WAS)")
  } else if (any(is.na(figaro_nrg_accounts$value))) {
    error_data <<- figaro_nrg_accounts
    stop("ERROR - NA values in obs accounts (WAS)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_nrg_accounts %>%
    mutate(
      serie_id    = "nrg_obs",
      value       = round(value, digits = 0),
      unit        = "GJ",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_nrg.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
