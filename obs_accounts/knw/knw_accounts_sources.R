# La Societe Nouvelle

# Sources partagees : les conversions et repartitions finales restent au builder.

# =============================================================================
# 1. OCDE ANBERD : depenses de R&D, en millions de monnaie nationale
# =============================================================================

load_anberd_knw_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_ANBERD@DF_ANBERDi4,/.A.MA..XDC.V.?",
    "startPeriod=", min(years$year), "&endPeriod=", max(years$year),
    "&dimensionAtObservation=AllDimensions"
  )

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "DSD_ANBERD_XDC.csv"), # nouveau nom : l'ancien cache est en USD PPA
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
      UNIT_MEASURE == "XDC" # monnaie nationale, convertie en euros par le builder
    ) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
      anberd_activity = ACTIVITY,
      value = round(as.numeric(ObsValue) / 1e6, digits = 0),
      unit = "MNCU" # millions d'unites de monnaie nationale
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

# =============================================================================
# 4. Eurostat BERD par type de depense : frais de personnel / BERD totale
# =============================================================================
# Toutes les annees publiees sont chargees : le builder interpole la part des
# frais de personnel entre les annees renseignees (collecte souvent biennale).

load_berd_labour_cost_knw_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "rd_e_berdcostr2.csv"),
    function() {
      read.csv(paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
        "rd_e_berdcostr2/A.TOTAL+CUR_LC..MIO_EUR.?format=SDMX-CSV"
      ))
    },
    c("freq", "expend", "nace_r2", "unit", "geo", "TIME_PERIOD", "OBS_VALUE"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Part des frais de personnel par pays, activite et annee ----------------
  # Codes Eurostat ramenes aux codes FIGARO (EL -> GR, UK -> GB, Chine hors HK).

  raw_data %>%
    filter(
      freq == "A",
      unit == "MIO_EUR",
      expend %in% c("TOTAL", "CUR_LC")
    ) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      country = recode(geo, EL = "GR", UK = "GB", CN_X_HK = "CN"),
      value = suppressWarnings(as.numeric(OBS_VALUE))
    ) %>%
    filter(!is.na(value)) %>%
    select(year, country, nace_r2, expend, value) %>%
    pivot_wider(names_from = expend, values_from = value) %>%
    filter(!is.na(TOTAL), !is.na(CUR_LC), TOTAL > 0, CUR_LC >= 0) %>%
    transmute(
      year,
      country,
      nace_r2,
      labour_cost_share = pmin(CUR_LC / TOTAL, 1)
    )
}

