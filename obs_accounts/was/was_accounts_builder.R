# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Environmental accounts builder for waste generation (WAS)
#'
#' Main sources :
#'   - Generation of waste by waste category, hazardousness and NACE Rev. 2 activity (Eurostat)
#'      Source : https://ec.europa.eu/eurostat/databrowser/view/env_wasgen/default/table?lang=fr
#'   - Waste by sector (OECD)
#'      Source : https://data-explorer.oecd.org/vis?df[ds]=DisseminateFinalDMZ&df[id]=DSD_WSECTOR%40DF_WSECTOR&df[ag]=OECD.ENV.EPI&dq=FRA.A.TOTAL.T.A%2BA01%2BB%2BC%2BC10T12%2BC13T15%2BC16%2BC17_18%2BC17%2BC18%2BC19%2BC20_21%2BC20T22%2BC22%2BC23%2BC24_25%2BC24T33%2BC24%2BC25T28%2BC25%2BC26T30%2BC29_30%2BC29T33%2BC31T33%2BD%2BE%2BF%2BT%2BT98%2B_O%2B_T&lom=LASTNPERIODS&lo=1&to[TIME_PERIOD]=false&vw=tb
#'
#' Output data
#'   Accounts are in tonnes (T)
#'
#' Missing values filled by proxy using industry and country similarity.#
#'
#' to execute: build_was_obs_accounts()

build_was_obs_accounts <- function(
  years = seq(2010, 2022, 2),
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build WAS accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_imputations.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/was/was_accounts_sources.R", encoding = "UTF-8-BOM")

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

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/was/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  table_passage_ocde_data <- read_delim(
      "obs_accounts/was/table_passage_ocde.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      industry = figaro_industry
    ) %>%
    select(oecd_activity, oecd_activity_level, industry)

  # -------------------------------------------------------------------
  if (verbose) cat("Metadata loaded\n")

  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  if (verbose) print("Load FIGARO data")

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
  # Eurostat data

  if (verbose) print("Load EUROSTAT data")

  eurostat_data <- load_eurostat_was_source(years$year, use_temp_data, verbose)

  # -------------------------------------------------------------------
  # OECD data

  oecd_data <- load_oecd_was_source(years$year, use_temp_data, verbose)

  # -------------------------------------------------------------------
  # Building WAS impact vector


  if (verbose) cat("Building FIGARO accounts...\n")

  if (verbose) print("Build accounts data")

  # -------------------------
  # Accounts data based on Eurostat data

  # use VA to split impacts between FIGARO industries
  va_distribution_eurostat_nace_r2 <- eurostat_correspondence_table_nace_r2 %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, nace_r2) %>%
    mutate(
      share_nace_r2 = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, nace_r2, share_nace_r2)

  eurostat_was_accounts <- eurostat_data %>%
    merge(va_distribution_eurostat_nace_r2) %>%
    mutate(
      eurostat_value = waste_generation * share_nace_r2,
      eurostat_flag = ""
    ) %>%
    filter(eurostat_value >= 0) %>%
    select(year, country, industry, eurostat_value, eurostat_flag)

  # -------------------------
  # Accounts data based on OECD data

  # use VA to split impacts between FIGARO industries
  va_distribution_oecd_activities <- table_passage_ocde_data %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, oecd_activity, oecd_activity_level) %>%
    mutate(
      share_oecd_activity = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, oecd_activity, oecd_activity_level, share_oecd_activity)

  oecd_was_accounts <- oecd_data %>%
    merge(va_distribution_oecd_activities) %>%
    mutate(
      oecd_value = waste_generation * share_oecd_activity,
      oecd_flag = ""
    ) %>%
    group_by(year, country, industry) %>%
    slice_min(oecd_activity_level, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(oecd_value >= 0) %>%
    select(year, country, industry, oecd_value, oecd_flag)

  # -------------------------
  # Accounts data

  # init accounts data with eurostat (priority) or oecd data
  figaro_was_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      eurostat_was_accounts,
      by = c("year", "country", "industry")
    ) %>%
    left_join(
      oecd_was_accounts,
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      value = if_else(!is.na(eurostat_value), eurostat_value, oecd_value),
      flag = if_else(!is.na(eurostat_flag), eurostat_flag, oecd_flag),
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_was_accounts <- figaro_was_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "WAS", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_was_accounts) != size) {
    error_data <<- figaro_was_accounts
    stop("ERROR - Wrong size for obs accounts (WAS)")
  } else if (any(is.na(figaro_was_accounts$value))) {
    error_data <<- figaro_was_accounts
    stop("ERROR - NA values in obs accounts (WAS)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_was_accounts %>%
    mutate(
      serie_id    = "was_obs",
      value       = round(value, digits = 0),
      unit        = "T",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_was.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
