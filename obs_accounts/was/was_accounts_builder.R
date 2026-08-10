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
  years = seq(2010, 2022, 2), # NB: 2024 not yet published by either Eurostat (env_wasgen) or OECD (DSD_WSECTOR) as of 2026-08 - biennial reporting lag
  detect_latest_year = FALSE, # if TRUE, probe env_wasgen/DSD_WSECTOR beyond max(years) (step 2, biennial) and extend if complete
  do_clean_outliers = FALSE, # obs series treated as reliable by default; trd/tgt series keep outlier cleaning on
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build WAS accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_source_years.R")

  # -------------------------------------------------------------------
  # Optional: detect the latest usable waste-generation year beyond max(years)
  #
  # Tries Eurostat env_wasgen first (priority source in production below),
  # falls back to OECD DSD_WSECTOR for the completeness check if Eurostat has
  # nothing for that candidate year - same source priority as the real
  # extraction. step = 2 because this reporting is biennial: probing every
  # single year would waste requests on years the source structurally never
  # publishes, and (more importantly) detect_max_usable_year would stop at
  # the very first odd year instead of reaching the next real one.

  if (detect_latest_year) {
    fetch_was_year <- function(year_i) {
      url_eurostat <- paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_wasgen/1.0/*.*.*.*.*.*?",
        "c[freq]=", "A",
        "&c[hazard]=", "HAZ_NHAZ",
        "&c[TIME_PERIOD]=", year_i,
        "&compress=", "false",
        "&format=", "csvdata"
      )
      eurostat_raw <- tryCatch(read.csv(url_eurostat), error = function(e) NULL)
      eurostat_df <- NULL
      if (!is.null(eurostat_raw) && nrow(eurostat_raw) > 0) {
        eurostat_df <- eurostat_raw %>%
          filter(freq == "A", unit == "T", hazard == "HAZ_NHAZ", waste == "PRIM") %>%
          transmute(country = geo, sector = nace_r2, value = OBS_VALUE)
      }

      url_oecd <- paste0(
        "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WSECTOR@DF_WSECTOR,/.A.TOTAL.T.?",
        "startPeriod=", year_i,
        "&endPeriod=", year_i,
        "&dimensionAtObservation=", "AllDimensions",
        "&format=", "csvfilewithlabels"
      )
      oecd_raw <- tryCatch(read.csv(url_oecd), error = function(e) NULL)
      oecd_df <- NULL
      if (!is.null(oecd_raw) && nrow(oecd_raw) > 0) {
        oecd_df <- oecd_raw %>%
          filter(MEASURE == "TOTAL", ACTION == "I", FREQ == "A", UNIT_MEASURE == "TRUE") %>%
          transmute(country = REF_AREA, sector = ACTIVITY, value = OBS_VALUE)
      }

      combined <- bind_rows(eurostat_df, oecd_df)
      if (nrow(combined) == 0) return(NULL)
      combined
    }

    detected_max_year <- detect_max_usable_year(
      source_name     = "WAS_WASGEN_WSECTOR",
      fetch_year_fn   = fetch_was_year,
      group_col       = "country",
      value_col       = "value",
      sector_col      = "sector",
      known_good_year = max(years),
      step            = 2,
      verbose         = verbose
    )

    if (detected_max_year > max(years)) {
      if (verbose) message("WAS: extending years to detected max usable year ", detected_max_year)
      years <- seq(min(years), detected_max_year, by = 2)
    }
  }

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

  eurostat_correspondence_table_geo <- read_delim(
      "obs_accounts/was/eurostat_correspondence_table_geo.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, geo)

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

  base_url_eurostat_data = "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_wasgen/1.0/*.*.*.*.*.*?"
  url_eurostat_data = paste0(base_url_eurostat_data,
   "c[freq]=","A",
   "&c[hazard]=","HAZ_NHAZ",
   "&c[TIME_PERIOD]=",paste0(years$year, collapse = ","),
   "&compress=","false",
   "&format=","csvdata"
  )

  eurostat_file_path  <- file.path(download_dir, "env_wasgen.csv")

  if (!file.exists(eurostat_file_path) | !use_temp_data)
  {
    eurostat_raw_data <- read.csv(url_eurostat_data)

    write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
  }

  eurostat_raw_data <- read.csv(eurostat_file_path)

  eurostat_data <- eurostat_raw_data %>%
    filter(
      freq == "A",
      unit == "T",
      hazard == "HAZ_NHAZ",
      waste == "PRIM"
    ) %>%
    merge(eurostat_correspondence_table_geo) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      waste_generation = round(OBS_VALUE, digits = 0),
    ) %>%
    select(year, country, nace_r2, waste_generation, unit)

  # -------------------------------------------------------------------
  # OECD data

  if (verbose) print("Load OECD data")

  base_url_oecd_data = "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WSECTOR@DF_WSECTOR,/.A.TOTAL.T.?"
  url_oecd_data = paste0(base_url_oecd_data,
    "startPeriod=",min(years$year),
    "&endPeriod=",max(years$year),
    "&dimensionAtObservation=","AllDimensions",
    "&format=","csvfilewithlabels"
  )

  oecd_file_path  <- file.path(download_dir, "DSD_WSECTOR.csv")

  if (!file.exists(oecd_file_path) | !use_temp_data)
  {
    if (verbose) print("Téléchargement des données OCDE...")

    oecd_raw_data <- read.csv(url_oecd_data)

    write.csv(oecd_raw_data, oecd_file_path, row.names = FALSE)
  }

  oecd_raw_data <- read.csv(oecd_file_path)

  oecd_data <- oecd_raw_data %>%
    filter(
      MEASURE == "TOTAL",
      ACTION == "I",
      FREQ == "A",
      UNIT_MEASURE == "TRUE", # TONNES
      UNIT_MULT == 3 #
    ) %>%
    mutate(
      REF_AREA = countrycode(REF_AREA, 'iso3c', 'iso2c', nomatch = NULL)
    ) %>%
    # format
    mutate(
      year = as.character(TIME_PERIOD),
      country = REF_AREA,
      oecd_activity = ACTIVITY,
      waste_generation = round(OBS_VALUE * 1e3, digits = 0),
      unit = "T",
    ) %>%
    select(year,country,oecd_activity,waste_generation,unit)

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
    proxy_missing_value_by_similarity(., "WAS") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  if (do_clean_outliers) {
    figaro_was_accounts <- figaro_was_accounts %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
      clean_outliers(., serie_pkey = c("country", "industry")) %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
      select(year, country, industry, value, flag)
  }

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

  accounts_data_path  <- file.path(output_dir, "accounts_obs_was.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
