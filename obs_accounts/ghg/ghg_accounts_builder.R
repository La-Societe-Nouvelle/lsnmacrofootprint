# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for ghg emissions (GHG)
#'
#' Main sources :
#'   - Greenhouse gas emission footprints (in CO2 equivalent, FIGARO application) - EUROSTAT
#'   - Extension EDGAR / AEA selon la methode FIGARO, puis Air Emissions Accounts - OECD
#'
#' Output data
#'   Accounts are in tonnes of CO2e
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_ghg_obs_accounts()

build_ghg_obs_accounts <- function(
  years = 2010:2024,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build GHG accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_imputations.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/ghg/ghg_accounts_sources.R", encoding = "UTF-8-BOM")

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
  # Shared sources: Eurostat, EDGAR / AEA extension, OECD (all in TCO2E).

  eurostat_data <- load_eurostat_ghg_source(years$year, use_temp_data, verbose)
  estimated_data <- load_estimated_ghg_source(years$year, use_temp_data, verbose)
  oecd_data <- load_oecd_ghg_source(years$year, use_temp_data, verbose)
  selected_data <- select_ghg_accounts_sources(eurostat_data, estimated_data, oecd_data)

  # -------------------------------------------------------------------
  # Building FIGARO accounts: Eurostat, then extension, then OECD per row.

  figaro_ghg_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(selected_data, by = c("year", "country", "industry")) %>%
    mutate(flag = "") %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_ghg_accounts <- figaro_ghg_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "GHG", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_ghg_accounts) != size) {
    error_data <<- figaro_ghg_accounts
    stop("ERROR - Wrong size for obs accounts (GHG)")
  } else if (any(is.na(figaro_ghg_accounts$value))) {
    error_data <<- figaro_ghg_accounts
    stop("ERROR - NA values in obs accounts (GHG)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <<- figaro_ghg_accounts %>%
    mutate(
      serie_id    = "ghg_obs",
      value       = round(value, digits = 0),
      unit        = "TCO2E",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_ghg.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
