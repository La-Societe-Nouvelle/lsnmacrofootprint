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
  years = 2014:2022,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build NRG accounts for observed data")
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

  eurostat_correspondence_table_geo <- read_delim(
      "obs_accounts/nrg/eurostat_correspondence_table_geo.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, geo)

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/nrg/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  jrc_correspondence_table_industry <- read_delim(
      "obs_accounts/nrg/jrc_correspondence_table_industry.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, jrc_industry)

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
  # EUROSTAT data

  base_url_eurostat_data = "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_ac_pefasu/1.0/*.*.*.*.*.*?"
  url_eurostat_data = paste0(base_url_eurostat_data,
    "c[freq]=","A",
    "&c[stk_flow]=","ER_USE", # SUP
    "&c[prod_nrg]=N00_P00_R00",
    "&c[unit]=TJ",
    "&c[TIME_PERIOD]=",paste0(years$year, collapse = ","),
    "&compress=","false",
    "&format=","csvdata"
  )

  eurostat_file_path  <- file.path(download_dir, "env_ac_pefasu.csv")

  if (!file.exists(eurostat_file_path) | !use_temp_data)
  {
    eurostat_raw_data <- read.csv(url_eurostat_data)

    write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
  }

  eurostat_raw_data <- read.csv(eurostat_file_path)

  eurostat_data <- eurostat_raw_data %>%
    filter(
      freq == "A",
      stk_flow == "SUP", # ER_USE
      prod_nrg == "R30", # N00_P00_R00
      unit == "TJ",
      TIME_PERIOD %in% years$year
    ) %>%
    # format
    merge(eurostat_correspondence_table_geo) %>% # by geo
    merge(eurostat_correspondence_table_nace_r2) %>% # by nace_r2
    mutate(
      year = as.character(TIME_PERIOD),
      nrg_use = round(as.numeric(OBS_VALUE) * 1e3, digits = 0),
      unit = "GJ"
    ) %>%
    select(year,country,industry,nrg_use,unit)

  # -------------------------------------------------------------------
  # JRC data (2015)

  base_url_jrc_data = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FIGARO-E3/Energy%20and%20emissions/flatfile_FIGARO-e_ENE_TJ_2015.csv"

  jrc_file_path  <- file.path(download_dir, "FIGARO-e_ENE_TJ_2015.csv")

  if (!file.exists(jrc_file_path) | !use_temp_data)
  {
    jrc_raw_data <- read.csv(base_url_jrc_data)

    write.csv(jrc_raw_data, jrc_file_path, row.names = FALSE)
  }

  jrc_raw_data <- read.csv(jrc_file_path)

  jrc_data <- jrc_raw_data %>%
    filter(
      category == "Energy",
      codeIndicator == "NEU", # Net energy use
      # codeIndicator == "FEU", # Net energy use
      codeEproduct == "Total",
      timePeriod == "2015",
      unit == "TJ"
    ) %>%
    mutate(
      year = as.character(timePeriod),
      country = refArea,
      jrc_industry = colPi,
      value = round(obsValue * 1e3, digits = 0),
      unit = "GJ"
    ) %>%
    merge(jrc_correspondence_table_industry) %>%
    group_by(year,country,industry,unit) %>%
    summarise(
      value = sum(value, na.rm = TRUE)
    ) %>%
    select(year,country,industry,value,unit)

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
    left_join(eurostat_nrg_accounts) %>%
    mutate(
      value = if_else(!is.na(eurostat_value), eurostat_value, NA),
      flag = if_else(!is.na(eurostat_flag), eurostat_flag, NA),
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_nrg_accounts <- figaro_nrg_accounts_raw %>%
    proxy_missing_value_by_similarity(., "NRG") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  figaro_nrg_accounts <- figaro_nrg_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
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

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <- figaro_nrg_accounts %>%
    mutate(
      serie_id    = "nrg_obs",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  # -------------------------------------------------------------------
  if (verbose) print(formatted_data %>% as_tibble())

  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_nrg.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
