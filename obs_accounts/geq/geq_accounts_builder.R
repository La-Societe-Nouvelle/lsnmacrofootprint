# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for gender pay gap (GEQ)
#'
#' Source : ILOSTAT EAR_EHRA_SEX_ECO_CUR_NB_A.
#' Gender pay gaps are computed from hourly earnings by sex and sector for all
#' countries. The closest available source years are used by the source loader.
#' Output values are percentages; remaining gaps use economic similarity.
#'
#' build_geq_obs_accounts()

build_geq_obs_accounts <- function(
  years = 2016:2023,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build GEQ accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/geq/geq_accounts_sources.R", encoding = "UTF-8-BOM")
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
    select(industry, branch, sector)

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
  # ILOSTAT data

  if (verbose) cat("Loading ILOSTAT data...\n")

  ilostat_data <- load_ilostat_geq_source(years$year, use_temp_data, verbose)

  if (verbose) cat("ILOSTATS data loaded\n")

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  # ILOSTAT - Gender pay gap by industry

  ilostat_geq <- ilostat_data %>%
    pivot_wider(
      names_from = sex,
      values_from = hourly_rate,
      names_glue = "hourly_rate_{sex}"
    ) %>%
    # temp missing hourly women in ILOSTAT for FR B 2017
    mutate(
      hourly_rate_women = if_else(
        is.na(hourly_rate_women),
        hourly_rate_all + (hourly_rate_all - hourly_rate_men),
        hourly_rate_women
      )
    ) %>%
    mutate(
      gender_pay_gap = round( abs(hourly_rate_men - hourly_rate_women) / hourly_rate_all *100, digits = 1)
    ) %>%
    merge(figaro_industries) %>%
    select(year, country, industry, gender_pay_gap)

  # FIGARO accounts

  raw_geq_accounts <- ilostat_geq %>%
    # merge(coefs_ilostat) %>%
    mutate(
      # value = round(gender_pay_gap * coef_ilostat, digits = 1), # prev methodology
      value = round(gender_pay_gap, digits = 1),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  figaro_geq_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_geq_accounts,
      by = c("year", "country", "industry")
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_geq_accounts <- figaro_geq_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "GEQ", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_geq_accounts) != size) {
    error_data <<- figaro_geq_accounts
    stop("ERROR - Wrong size for obs accounts (GEQ)")
  } else if (any(is.na(figaro_geq_accounts$value))) {
    error_data <<- figaro_geq_accounts
    stop("ERROR - NA values in obs accounts (GEQ)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <<- figaro_geq_accounts %>%
    mutate(
      serie_id    = "geq_obs",
      value       = round(value, digits = 1),
      unit        = "PCT",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_geq.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
