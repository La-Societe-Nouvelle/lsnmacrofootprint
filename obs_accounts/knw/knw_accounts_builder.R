# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for training/research contribution (KNW)
#'
#' Main sources :
#'   -
#'
#' Output data
#'   Accounts are in millions of euros (CP_MEUR)
#'
#' ANBERD: 2010-2021 (2022 limited data)
#' TRNG: 2015-2020
#' STAN_2025: 2015-2022
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_knw_obs_accounts()

build_knw_obs_accounts <- function(
  years = 2015:2020,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build KNW accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_monetary_conversion.R")
  source("utils/utils_outliers.R")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  if (verbose) print("Load metadata")

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

  anberd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/anberd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, anberd_activity)

  oecd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/oecd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

  eurostat_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

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
  # Monetary coef

  usd_eur <- data.frame(
    year = years$year,
    usd_eur = sapply(years$year, from_usd_to_euro)
  )
  # usd_eur = coef 1 $ x usd_eur -> 1 €

  # -------------------------------------------------------------------
  # ANBERD data (R&D Expenditure)

  if (verbose) print("Load ANBERD data")

  base_url_anberd_data = "https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_ANBERD@DF_ANBERDi4,/.A.MA..USD_PPP.V.?"
  url_anberd_data = paste0(base_url_anberd_data,
    "startPeriod=",min(years$year),
    "&endPeriod=",max(years$year),
    "&dimensionAtObservation=AllDimensions"
  )

  anberd_file_path  <- file.path(download_dir, "DSD_ANBERD.csv")

  if (!file.exists(anberd_file_path) | !use_temp_data)
  {
    if (verbose) print("Download ANBERD data")

    anberd_raw_data <- read_sdmx(url_anberd_data)

    write.csv(anberd_raw_data, anberd_file_path, row.names = FALSE)
  }

  anberd_raw_data <- read.csv(anberd_file_path)

  anberd_data <- anberd_raw_data %>%
    filter(
      CRITERIA == "MA", # "MA" DIRDE in the main activity of the company and not distributed according to the sectors involved
      FREQ == "A",
      MEASURE == "B",
      PRICE_BASE == "V", # "V" current prices
      TIME_PERIOD %in% years$year,
      UNIT_MEASURE == "USD_PPP" # "USD_PPP" according to $
    ) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
      anberd_activity = ACTIVITY,
      value = round(as.numeric(ObsValue) / 1e6, digits = 0),
      unit = "MUSD"
    ) %>%
    merge(usd_eur) %>%
    mutate(
      value = round(value * usd_eur, digits = 0),
      unit = "MEUR"
    ) %>%
    select(year, country, anberd_activity, value, unit) %>%
    arrange(year, country, anberd_activity)

  # -------------------------------------------------------------------
  # EUROSTAT data (TRNG CVT)

  if (verbose) print("Load EUROSTAT data")

  base_url_trng_cvt_data = "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/trng_cvt_16n2/1.0?"
  url_trng_cvt_data = paste0(base_url_trng_cvt_data,
    "compress=","false",
    "&format=","csvdata",
    "&formatVersion=","2.0"
  )

  eurostat_file_path  <- file.path(download_dir, "trng_cvt_16n2.csv")

  if (!file.exists(eurostat_file_path) | !use_temp_data)
  {
    if (verbose) print("Download EUROSTAT data")

    trng_cvt_raw_data <- read.csv(url_trng_cvt_data)

    write.csv(trng_cvt_raw_data, eurostat_file_path, row.names = FALSE)
  }

  trng_cvt_raw_data <- read.csv(eurostat_file_path)

  trng_cvt_data <- trng_cvt_raw_data %>%
    filter(
      freq == "A",
      cost == "TOTAL",
      unit == "PC"
    ) %>%
    crossing(years) %>%
    mutate(
      time_num = as.integer(TIME_PERIOD),
      ecart = abs(time_num - as.integer(year))
    ) %>%
    group_by(year) %>%
    filter(ecart == min(ecart, na.rm = TRUE)) %>%
    ungroup() %>%
    # format
    mutate(
      country = geo,
      nace_r2 = nace_r2,
      trng_cvt = as.numeric(OBS_VALUE) / 100,
    ) %>%
    select(year,country,nace_r2,trng_cvt)

  # -------------------------------------------------------------------
  # OCDE STAN data

  if (verbose) print("Load OECD STAN data")

  base_url_stan_data = "https://sdmx.oecd.org/public/rest/data/OECD.STI.PIE,DSD_STAN@DF_STAN_2025,1.0/A...D11+B1G.V.?"
  url_stan_data = paste0(base_url_stan_data,
    "startPeriod=",min(years$year),
    "&endPeriod=",max(years$year),
    "&dimensionAtObservation=","AllDimensions",
    "&format=csvfilewithlabels"
  )

  oecd_file_path  <- file.path(download_dir, "DSD_STAN.csv")

  if (!file.exists(oecd_file_path) | !use_temp_data)
  {
    if (verbose) print("Download STAN data")

    stan_raw_data <- read.csv(url_stan_data)

    write.csv(stan_raw_data, oecd_file_path, row.names = FALSE)
  }

  stan_raw_data <- read.csv(oecd_file_path)

  stan_data <- stan_raw_data %>%
    filter(
      ACTION == "I",
      FREQ == "A",
      MEASURE %in% c("D11","B1G"),
      PRICE_BASE == "V",
      UNIT_MEASURE == "XDC",
      UNIT_MULT == 6
    ) %>%
    mutate(
      year = TIME_PERIOD,
      country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
      oecd_activity = ACTIVITY,
      aggregate = MEASURE,
      value = as.numeric(OBS_VALUE),
      unit = "MEUR"
    ) %>%
    merge(oecd_correspondence_table_activity) %>%
    select(year, country, industry, aggregate, value)

  # -------------------------------------------------------------------
  # Building KNW impact vector

  if (verbose) cat("Building FIGARO accounts...\n")

  # /!\ values in percentage (rate) to build accounts

  # -------------------------
  # Research contribution

  # R&D accounts (ANBERD)

  # use VA to split contributions between FIGARO industries
  va_distribution_anberd_activities <- anberd_correspondence_table_activity %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, anberd_activity) %>%
    mutate(
      share_anberd_activity = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, anberd_activity, share_anberd_activity)

  research_contributions_raw <- anberd_data %>%
    merge(va_distribution_anberd_activities) %>%
    merge(main_aggregates_data) %>%
    mutate(
      value = round(value * share_anberd_activity / VA, digits = 6), # contribution (rate)
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  research_contributions <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(research_contributions_raw) %>%
    proxy_missing_value_by_similarity(., "KNW") %>%
    rename(
      research_value = value,
      research_flag = flag
    ) %>%
    select(year, country, industry, research_value, research_flag) %>%
    arrange(year, country, industry)

  # -------------------------
  # Training contribution

  # Share of total labor cost in net value added (STAN)

  shares_labor_cost <- stan_data %>%
    pivot_wider(
      names_from = aggregate,
      values_from = value
    ) %>%
    filter(!is.na(B1G) & !is.na(D11)) %>%
    mutate(
      share_labor_cost = ifelse(B1G > 0, pmax(pmin(D11 / B1G, 1), 0), 0)
    ) %>%
    select(year, country, industry, share_labor_cost)

  training_contributions_raw <- trng_cvt_data %>%
    merge(eurostat_correspondence_table_activity) %>% # by nace_r2
    merge(shares_labor_cost) %>% # by year, country, industry
    mutate(
      value = round(share_labor_cost * trng_cvt, digits = 6),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  training_contributions <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(training_contributions_raw) %>%
    proxy_missing_value_by_similarity(., "KNW") %>%
    rename(
      training_value = value,
      training_flag = flag
    ) %>%
    select(year, country, industry, training_value, training_flag) %>%
    arrange(year, country, industry)

  # -------------------------
  # FIGARO accounts

  figaro_knw_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(main_aggregates_data) %>%
    left_join(research_contributions) %>%
    left_join(training_contributions) %>%
    mutate(
      contribution_rate = if_else(
        industry %in% c("M72", "P85"),
        1.0,
        pmax(pmin(research_value + training_value, 1.0), 0.0)
      ),
      flag = case_when(
        industry %in% c("M72", "P85") ~ "",
        research_flag == "e" ~ "e",
        training_flag == "e" ~ "e",
        TRUE ~ ""
      )
    ) %>%
    mutate(
      value = if_else(NVA > 0, round(NVA * contribution_rate, digits = 0), 0.0),
      flag = flag
    ) %>%
    select(year, country, industry, value, flag)

  # Check max / Clean outliers
  figaro_knw_accounts <- figaro_knw_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA * 100.0, 0)) %>%
    # Clean outliers
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    # Check upper limit
    mutate(value = min(value, 100.0)) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / 100.0 * NVA, 0)) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_knw_accounts) != size) {
    error_data <<- figaro_knw_accounts
    stop("ERROR - Wrong size for obs accounts (KNW)")
  } else if (any(is.na(figaro_knw_accounts$value))) {
    error_data <<- figaro_knw_accounts
    stop("ERROR - NA values in obs accounts (KNW)")
  }

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <- figaro_knw_accounts %>%
    mutate(
      serie_id    = "knw_obs",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  # -------------------------------------------------------------------
  if (verbose) print(formatted_data %>% as_tibble())

  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_knw.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
