# La Societe Nouvelle

# Sources partagees par le producteur et le controle.
# Les valeurs, filtres et annees de reference conservent la methode existante.

# =============================================================================
# 1. OCDE : prelevements d'eau par activite (THS_M3)
# =============================================================================

load_oecd_wat_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WATER_PSUT@DF_WATER_PSUT,1.0/.A.SURFACE_GROUND..USE.SEC.?",
    "startPeriod=", min(years$year),
    "&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )

  # --- Lecture du cache ou acquisition explicite -----------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "DSD_WATER_PSUT.csv"),
    function() {
      read.csv(url)
    },
    c(
      "VARIABLE",
      "SUPPLY_USE",
      "MEASURE",
      "FREQ",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "REF_AREA",
      "ACTIVITY",
      "TIME_PERIOD",
      "OBS_VALUE"
    ),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Filtres et preparation utilises par le producteur et le controle --------

  oecd_data <- raw_data %>%
    filter(
      VARIABLE == "SEC",
      SUPPLY_USE == "USE",
      MEASURE == "SURFACE_GROUND",
      FREQ == "A",
      UNIT_MEASURE == "M3", # m3
      UNIT_MULT == 6 # millions
    ) %>%
    mutate(
      REF_AREA = countrycode(REF_AREA, 'iso3c', 'iso2c', nomatch = NULL)
    ) %>%
    # format
    mutate(
      year = as.character(TIME_PERIOD),
      country = REF_AREA,
      oecd_industry = ACTIVITY,
      water_withdrawal = round(OBS_VALUE * 1e3, digits = 0),
      unit = "THS_M3",
    ) %>%
    select(year,country,oecd_industry,water_withdrawal,unit)

  oecd_data
}

