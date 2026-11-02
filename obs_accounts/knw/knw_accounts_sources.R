# La Societe Nouvelle

# Sources partagees : les conversions et repartitions finales restent au builder.

# =============================================================================
# 1. OCDE ANBERD : depenses de R&D, en millions USD
# =============================================================================

load_anberd_knw_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_ANBERD@DF_ANBERDi4,/.A.MA..USD_PPP.V.?",
    "startPeriod=", min(years$year), "&endPeriod=", max(years$year),
    "&dimensionAtObservation=AllDimensions"
  )

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "DSD_ANBERD.csv"),
    function() {
      read_sdmx(url)
    },
    c(
      "CRITERIA",
      "FREQ",
      "MEASURE",
      "PRICE_BASE",
      "TIME_PERIOD",
      "UNIT_MEASURE",
      "REF_AREA",
      "ACTIVITY",
      "ObsValue"
    ),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Preparation commune au producteur et au controle -----------------------

  anberd_data <- raw_data %>%
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
    )

  anberd_data
}

# =============================================================================
# 2. Eurostat CVT : cout de formation / masse salariale
# =============================================================================

load_training_knw_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "trng_cvt_16n2.csv"),
    function() {
      read.csv(paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/trng_cvt_16n2/1.0?",
        "compress=false&format=csvdata&formatVersion=2.0"
      ))
    },
    c("freq", "cost", "unit", "TIME_PERIOD", "geo", "nace_r2", "OBS_VALUE"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Preparation commune au producteur et au controle -----------------------

  trng_cvt_data <- raw_data %>%
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

  trng_cvt_data
}

# =============================================================================
# 3. OCDE STAN : salaires et valeur ajoutee par branche
# =============================================================================

load_stan_knw_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  oecd_correspondence_table_activity <- read_delim(
    "obs_accounts/knw/oecd_correspondence_table_activity.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.STI.PIE,DSD_STAN@DF_STAN_2025,1.0/A...D11+B1G.V.?",
    "startPeriod=", min(years$year), "&endPeriod=", max(years$year),
    "&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "DSD_STAN.csv"),
    function() {
      read.csv(url)
    },
    c(
      "ACTION",
      "FREQ",
      "MEASURE",
      "PRICE_BASE",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "TIME_PERIOD",
      "REF_AREA",
      "ACTIVITY",
      "OBS_VALUE"
    ),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Preparation commune au producteur et au controle -----------------------

  stan_data <- raw_data %>%
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

  stan_data
}

