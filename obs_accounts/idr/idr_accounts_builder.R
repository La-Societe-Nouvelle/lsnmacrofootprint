# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for pay gap index (IDR)
#'
#' Main sources :
#'   - Base Tous Salariés (BTS, Insee)
#'
#' Disposinibilité des données BTS : https://www.insee.fr/fr/recherche/recherche-statistiques?q=base+tous+salari%C3%A9s&debut=0&idprec=8730395&categorie=1&collection=4
#'
#' Output data
#'   No unit (ratio greater than 1)
#'   Digits: 2
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_idr_obs_accounts()

build_idr_obs_accounts <- function(
  years = 2016:2023,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build IDR accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/idr/idr_accounts_sources.R", encoding = "UTF-8-BOM")
  source("utils/utils_imputations.R")
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
    select(industry, branch)

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
  # BTS Data (Insee)

  if (verbose) cat("Loading BTS data...\n")

  bts_data <- load_bts_idr_source(years$year, use_temp_data, verbose)

  if (verbose) cat("BTS data loaded\n")

  # -------------------------------------------------------------------
  # ILOSTAT data

  if (verbose) cat("Loading ILOSTAT data...\n")

  ilostat_data <- load_ilostat_idr_source(years$year, use_temp_data, verbose)

  if (verbose) cat("ILOSTAT data loaded\n")

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  # BTS - Pay gap by industry

  bts_idr_fr <- bts_data %>%
    # compute hourly rate
    mutate(
      hourly_rate = round(net_pay / working_hours, digits = 2),
    ) %>%
    filter(hourly_rate != "Inf") %>%
    group_by(year, country, branch) %>%
    summarise(
      decile_9 = quantile(hourly_rate, 0.9, na.rm = TRUE),
      decile_1 = quantile(hourly_rate, 0.1, na.rm = TRUE),
      interdecile_range = round(decile_9 / decile_1, digits = 2),
      .groups = "drop"
    ) %>%
    merge(figaro_industries) %>%
    rename(base_country = country) %>%
    select(year, base_country, industry, interdecile_range)

  countries_idr_coef <- ilostat_data %>%
    pivot_wider(
      names_from = decile,
      values_from = value
    ) %>%
    group_by(year) %>%
    mutate(
      pay_gap_index = round((DCL_DECILE_09 + DCL_DECILE_10) / (DCL_DECILE_01 + DCL_DECILE_02), digits = 2), # /!\ definition differs
      coef = pay_gap_index / pay_gap_index[country == "FR"]
    ) %>%
    select(year, country, coef)

  # IDR accounts data for each FIGARO Country/Industry

  raw_idr_accounts <- bts_idr_fr %>%
    merge(countries_idr_coef) %>% # by year
    mutate(
      value = round(interdecile_range * coef, digits = 2),
      flag = ifelse(country == "FR", "", "e")
    ) %>%
    select(year, country, industry, value, flag)

  if (verbose) print("Finalisation des données")

  figaro_idr_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_idr_accounts,
      by = c("year", "country", "industry")
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_idr_accounts <- figaro_idr_accounts_raw %>%
    complete_series(min_value = 1) %>%
    proxy_missing_value_by_similarity(., "IDR", min_value = 1) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_idr_accounts) != size) {
    error_data <<- figaro_idr_accounts
    stop("ERROR - Wrong size for obs accounts (IDR)")
  } else if (any(is.na(figaro_idr_accounts$value))) {
    error_data <<- figaro_idr_accounts
    stop("ERROR - NA values in obs accounts (IDR)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_idr_accounts %>%
    mutate(
      serie_id    = "idr_obs",
      value       = round(value, digits = 2),
      unit        = "IDX",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_idr.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
