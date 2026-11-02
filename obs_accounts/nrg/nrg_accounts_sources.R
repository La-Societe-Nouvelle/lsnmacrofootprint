# La Societe Nouvelle

# Source partagee par le producteur et le controle NRG.
# Sortie : year, country, industry, nrg_use, unit (GJ).
# use_temp_data reutilise le cache ; allow_download autorise un telechargement.

# =============================================================================
# 1. Eurostat : comptes physiques de flux d'energie (env_ac_pefasu)
# =============================================================================

load_eurostat_nrg_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Requete : flux annuels de fourniture, exprimes en terajoules ------------

  years <- as.character(years)

  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_ac_pefasu/1.0/*.*.*.*.*.*?",
    "c[freq]=A&c[stk_flow]=SUP&c[unit]=TJ&c[TIME_PERIOD]=",
    paste(years, collapse = ","),
    "&compress=false&format=csvdata"
  )

  # --- Lecture du cache ou telechargement autorise ----------------------------
  # Le helper harmonise les anciens noms time/values et verifie les colonnes.

  data <- read_accounts_source_cache(
    file.path(download_dir, "env_ac_pefasu.csv"),
    function() read.csv(
      url,
      colClasses = "character"
    ),
    c(
      "freq", "stk_flow", "prod_nrg", "unit",
      "geo", "nace_r2", "TIME_PERIOD", "OBS_VALUE"
    ),
    use_temp_data,
    allow_download,
    normalize_eurostat_accounts_source
  )

  # --- Correspondances entre les codes Eurostat et FIGARO ---------------------

  geo <- read_delim(
    "obs_accounts/nrg/eurostat_correspondence_table_geo.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(country = figaro_country, geo)

  nace <- read_delim(
    "obs_accounts/nrg/eurostat_correspondence_table_nace_r2.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(industry = figaro_industry, nace_r2)

  # --- Selection des donnees et conversion TJ -> GJ ---------------------------
  # Retenir le produit R30 et les valeurs disponibles, positives ou nulles.

  data %>%
    filter(
      freq == "A",
      stk_flow == "SUP",
      prod_nrg == "R30",
      unit == "TJ",
      TIME_PERIOD %in% years
    ) %>%
    inner_join(geo, by = "geo") %>%
    inner_join(nace, by = "nace_r2") %>%
    transmute(
      year = as.character(TIME_PERIOD),
      country,
      industry,
      nrg_use = round(as.numeric(OBS_VALUE) * 1e3),
      unit = "GJ"
    ) %>%
    filter(!is.na(nrg_use), nrg_use >= 0)
}

# =============================================================================
# 2. JRC : energie nette de reference (2015)
# =============================================================================

# Le builder conserve son diagnostic JRC ; son coefficient n'est pas applique.
load_jrc_nrg_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  jrc_correspondence_table_industry <- read_delim(
      "obs_accounts/nrg/jrc_correspondence_table_industry.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, jrc_industry)

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "FIGARO-e_ENE_TJ_2015.csv"),
    function() read.csv("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FIGARO-E3/Energy%20and%20emissions/flatfile_FIGARO-e_ENE_TJ_2015.csv"),
    c(
      "category",
      "codeIndicator",
      "codeEproduct",
      "timePeriod",
      "unit",
      "refArea",
      "colPi",
      "obsValue"
    ),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  jrc_data <- raw_data %>%
    filter(
      category == "Energy",
      codeIndicator == "NEU", # Net energy use
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

  jrc_data
}
