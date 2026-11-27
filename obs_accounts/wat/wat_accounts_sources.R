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

# =============================================================================
# 2. FAO AQUASTAT (via Banque mondiale) : prelevements par grand usage (THS_M3)
# =============================================================================
# Prelevements totaux d'eau douce et parts agriculture / industrie (dont
# refroidissement des centrales) / usages municipaux (reseaux publics).
# Les valeurs AQUASTAT sont souvent prolongees a l'identique d'une annee a
# l'autre. FIGW1 = total mondial - pays FIGARO.

load_aquastat_wat_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  indicators <- c(
    total = "ER.H2O.FWTL.K3", # milliards de m3
    agriculture = "ER.H2O.FWAG.ZS", # % du total
    industry = "ER.H2O.FWIN.ZS",
    municipal = "ER.H2O.FWDM.ZS"
  )

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "WB_AQUASTAT_WATER_WITHDRAWAL.csv"),
    function() {
      timeout_option <- options(timeout = max(600, getOption("timeout")))
      on.exit(options(timeout_option))
      bind_rows(lapply(names(indicators), function(name) {
        json <- jsonlite::fromJSON(paste0(
          "https://api.worldbank.org/v2/country/all/indicator/", indicators[[name]],
          "?format=json&per_page=20000&date=2000:2030"
        ))
        data.frame(
          indicator = name,
          country = json[[2]]$country$id,
          iso3 = json[[2]]$countryiso3code,
          year = json[[2]]$date,
          value = json[[2]]$value
        )
      }))
    },
    c("indicator", "country", "iso3", "year", "value"),
    use_temp_data,
    allow_download
  )

  figaro_countries <- read_delim(
    "metadata/metadata_figaro_countries.csv",
    delim = ";",
    show_col_types = FALSE
  )$code

  aquastat_data <- raw_data %>%
    mutate(year = as.character(year)) %>%
    filter(year %in% as.character(years), !is.na(value)) %>%
    select(indicator, iso3, country, year, value) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    filter(!is.na(total)) %>%
    transmute(
      year,
      iso3,
      country,
      agriculture = total * 1e6 * agriculture / 100, # milliers de m3
      industry = total * 1e6 * industry / 100,
      municipal = total * 1e6 * municipal / 100
    )

  # Reste du monde FIGARO : total mondial - pays FIGARO
  rest_of_world_data <- aquastat_data %>%
    filter(iso3 == "WLD") %>%
    left_join(
      aquastat_data %>%
        filter(country %in% figaro_countries) %>%
        group_by(year) %>%
        summarise(
          across(c(agriculture, industry, municipal), ~ sum(.x, na.rm = TRUE), .names = "figaro_{.col}"),
          .groups = "drop"
        ),
      by = "year"
    ) %>%
    transmute(
      year,
      country = "FIGW1",
      agriculture = pmax(agriculture - coalesce(figaro_agriculture, 0), 0),
      industry = pmax(industry - coalesce(figaro_industry, 0), 0),
      municipal = pmax(municipal - coalesce(figaro_municipal, 0), 0)
    )

  aquastat_data %>%
    filter(country %in% figaro_countries, country != "FIGW1") %>%
    select(year, country, agriculture, industry, municipal) %>%
    bind_rows(rest_of_world_data)
}

# =============================================================================
# 3. Ember : production electrique thermique (TWh)
# =============================================================================
# Charbon, gaz, autres fossiles, nucleaire et bioenergie. FIGW1 = monde - pays
# FIGARO.

load_ember_thermal_wat_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "ember_generation_yearly.csv"),
    function() {
      timeout_option <- options(timeout = max(600, getOption("timeout")))
      on.exit(options(timeout_option))
      data <- read.csv(
        "https://files.ember-energy.org/public-downloads/generation/outputs/release_generation_yearly_global.csv",
        check.names = FALSE
      )
      data.frame(
        area = data[["Area"]],
        iso3 = data[["ISO 3 code"]],
        area_type = data[["Area type"]],
        year = data[["Year"]],
        source = data[["Electricity source"]],
        generation_twh = data[["Generation (TWh)"]]
      )
    },
    c("area", "iso3", "area_type", "year", "source", "generation_twh"),
    use_temp_data,
    allow_download,
    numeric_columns = "generation_twh"
  )

  figaro_countries <- read_delim(
    "metadata/metadata_figaro_countries.csv",
    delim = ";",
    show_col_types = FALSE
  )$code

  thermal_data <- raw_data %>%
    mutate(year = as.character(year)) %>%
    filter(
      year %in% as.character(years),
      source %in% c("Coal", "Gas", "Other fossil", "Nuclear", "Bioenergy"),
      area_type == "Country or economy" | area == "World"
    ) %>%
    mutate(
      country = if_else(
        area == "World",
        "World",
        countrycode(iso3, "iso3c", "iso2c", nomatch = NULL, warn = FALSE)
      )
    ) %>%
    group_by(year, country) %>%
    summarise(thermal_twh = sum(generation_twh, na.rm = TRUE), .groups = "drop")

  rest_of_world_data <- thermal_data %>%
    filter(country == "World") %>%
    left_join(
      thermal_data %>%
        filter(country %in% figaro_countries) %>%
        group_by(year) %>%
        summarise(figaro_twh = sum(thermal_twh), .groups = "drop"),
      by = "year"
    ) %>%
    transmute(year, country = "FIGW1", thermal_twh = pmax(thermal_twh - coalesce(figaro_twh, 0), 0))

  thermal_data %>%
    filter(country %in% figaro_countries, country != "FIGW1") %>%
    bind_rows(rest_of_world_data)
}

