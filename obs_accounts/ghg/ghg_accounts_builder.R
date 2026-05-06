# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for ghg emissions (GHG)
#'
#' Main sources :
#'   - Greenhouse gas emission footprints (in CO2 equivalent, FIGARO application) - EUROSTAT
#'   - Air Emissions Accounts - OECD
#'
#' Output data
#'   Accounts are in tonnes of CO2e
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_ghg_obs_accounts()

build_ghg_obs_accounts <- function(
  years = 2010:2023,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build GHG accounts for observed data")
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

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  eurostat_correspondence_table_c_orig <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_c_orig.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, c_orig)

  oecd_correspondence_table_activity <- read_delim(
      "obs_accounts/ghg/oecd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

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
  # Eurostat data - Greenhouse gas emission footprints (in CO2 equivalent, FIGARO application)

  eurostat_file_path  <- file.path(download_dir, "env_ac_ghgfp.csv")

  if (!file.exists(eurostat_file_path) | !use_temp_data)
  {
    eurostat_raw_data <- get_eurostat(
      "env_ac_ghgfp",
      filters = list(time = years$year, na_item = "TOTAL", c_dest = "WORLD"),
      cache   = FALSE
    )

    write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
  }

  eurostat_raw_data <- read.csv(eurostat_file_path)

  eurostat_data <- eurostat_raw_data %>%
    filter(
      freq == "A",
      unit == "THS_T",
      na_item == "TOTAL",
      c_dest == "WORLD"
    ) %>%
    merge(eurostat_correspondence_table_nace_r2) %>%
    merge(eurostat_correspondence_table_c_orig) %>%
    mutate(
      year = substring(as.character(time), 1, 4),
      eurostat_ghg_emissions = values * 1e3,
      unit = "TCO2E"
    ) %>%
    select(year, country, industry, eurostat_ghg_emissions, unit)

  # -------------------------------------------------------------------
  # OECD data

  base_url_oecd_data = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.2/..EMISSIONS.T_CO2E+T...GHG...?"
  url_oecd_data = paste0(base_url_oecd_data,
    "startPeriod=",min(years$year),
    "&endPeriod=",max(years$year),
    "&dimensionAtObservation=","AllDimensions",
    "&format=","csvfilewithlabels"
  )

  oecd_raw_data <- read.csv(url_oecd_data)

  oecd_data <- oecd_raw_data %>%
    filter(
      MEASURE == "EMISSIONS",
      POLLUTANT == "GHG",
      METHODOLOGY == "EMISSIONS_SEEA",
      ACTION == "I",
      FREQ == "A",
      UNIT_MEASURE == "T_CO2E", # TONNES
      SOURCE == "REPORTED",
      ACTIVITY_SCOPE == "RES",
      ADJUSTMENT == "N"
    ) %>%
    mutate(
      REF_AREA = countrycode(REF_AREA, 'iso3c', 'iso2c', nomatch = NULL)
    ) %>%
    # format
    mutate(
      year = as.character(TIME_PERIOD),
      country = REF_AREA,
      oecd_activity = ACTIVITY,
      oecd_ghg_emissions = OBS_VALUE,
      unit = "TCO2E",
    ) %>%
    merge(oecd_correspondence_table_activity) %>%
    select(year,country,industry,oecd_ghg_emissions,unit)

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_ghg_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(eurostat_data) %>% # by year, country, industry, unit
    left_join(oecd_data) %>% # by year, country, industry, unit
    mutate(
      value = case_when(
        country %in% eurostat_data$country ~ eurostat_ghg_emissions,
        country %in% oecd_data$country ~ oecd_ghg_emissions
      ),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_ghg_accounts <- figaro_ghg_accounts_raw %>%
    proxy_missing_value_by_similarity(., "GHG") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  figaro_ghg_accounts <- figaro_ghg_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
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

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <<- figaro_ghg_accounts %>%
    mutate(
      serie_id    = "ghg_obs",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  return(formatted_data)
}
