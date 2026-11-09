# La Societe Nouvelle

# Sources partagees par le producteur et le controle WAS, en tonnes.
# La repartition par branche FIGARO reste effectuee avec la NVA par le builder.
# use_temp_data reutilise le cache ; allow_download autorise un telechargement.

# =============================================================================
# 1. Eurostat : production de dechets (env_wasgen)
# =============================================================================
# Sortie : year, country, nace_r2, waste_generation, unit (T).

load_eurostat_was_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Requete : dechets dangereux et non dangereux, reunis --------------------

  years <- as.character(years)

  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_wasgen/1.0/*.*.*.*.*.*?",
    "c[freq]=A&c[hazard]=HAZ_NHAZ&c[TIME_PERIOD]=",
    paste(years, collapse = ","),
    "&compress=false&format=csvdata"
  )

  # --- Lecture et validation du cache Eurostat --------------------------------
  # Les anciens caches time/values sont harmonises en memoire.

  data <- read_accounts_source_cache(
    file.path(download_dir, "env_wasgen.csv"),
    function() read.csv(
      url,
      colClasses = "character"
    ),
    c(
      "freq", "unit", "hazard", "waste",
      "geo", "nace_r2", "TIME_PERIOD", "OBS_VALUE"
    ),
    use_temp_data,
    allow_download,
    normalize_eurostat_accounts_source
  )

  # --- Correspondance geographique -------------------------------------------
  # Conserver les secteurs NACE source pour la repartition ulterieure par NVA.

  geo <- read_delim(
    "obs_accounts/was/eurostat_correspondence_table_geo.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(country = figaro_country, geo)

  # --- Selection des dechets primaires, en tonnes -----------------------------

  data %>%
    filter(
      freq == "A",
      unit == "T",
      hazard == "HAZ_NHAZ",
      waste == "PRIM",
      TIME_PERIOD %in% years
    ) %>%
    inner_join(geo, by = "geo") %>%
    transmute(
      year = as.character(TIME_PERIOD),
      country,
      nace_r2,
      waste_generation = round(OBS_VALUE),
      unit
    ) %>%
    filter(!is.na(waste_generation))
}

# =============================================================================
# 2. OCDE : production de dechets par activite (DSD_WSECTOR)
# =============================================================================
# Sortie : year, country, oecd_activity, waste_generation, unit (T).

load_oecd_was_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Requete sur la plage d'annees demandee ---------------------------------

  years <- as.character(years)

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WSECTOR@DF_WSECTOR,/.A.TOTAL.T.?",
    "startPeriod=",
    min(years),
    "&endPeriod=",
    max(years),
    "&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )

  # --- Lecture et validation du cache OCDE ------------------------------------

  data <- read_accounts_source_cache(
    file.path(download_dir, "DSD_WSECTOR.csv"),
    function() read.csv(
      url,
      colClasses = "character"
    ),
    c(
      "MEASURE", "ACTION", "FREQ", "UNIT_MEASURE", "UNIT_MULT",
      "REF_AREA", "ACTIVITY", "TIME_PERIOD", "OBS_VALUE"
    ),
    use_temp_data,
    allow_download
  )

  # --- Selection et harmonisation des codes et unites -------------------------
  # Les valeurs source ont un multiplicateur de 10^3 : les convertir en tonnes.
  # Convertir les pays ISO alpha-3 en alpha-2 et conserver l'activite OCDE.

  data %>%
    filter(
      MEASURE == "TOTAL",
      ACTION == "I",
      FREQ == "A",
      UNIT_MEASURE == "TRUE",
      UNIT_MULT == 3,
      TIME_PERIOD %in% years
    ) %>%
    transmute(
      year = as.character(TIME_PERIOD),
      country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
      oecd_activity = ACTIVITY,
      waste_generation = round(OBS_VALUE * 1e3),
      unit = "T"
    ) %>%
    filter(!is.na(waste_generation))
}
