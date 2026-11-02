# La Societe Nouvelle

# Les trois sources GHG sont exprimees en tonnes equivalent CO2.
# Sortie commune : year, country, industry, value, unit (TCO2E).
# use_temp_data reutilise les caches ; allow_download autorise leur preparation.

# =============================================================================
# 1. Eurostat : emissions FIGARO (env_ac_ghgfp)
# =============================================================================

load_eurostat_ghg_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Lecture du cache ou appel a Eurostat si autorise -----------------------
  # La fonction de telechargement n'est executee que si le cache doit etre cree
  # ou rafraichi. Le helper harmonise les noms time/values en format SDMX.

  years <- as.character(years)

  data <- read_accounts_source_cache(
    file.path(download_dir, "env_ac_ghgfp.csv"),
    function() get_eurostat(
      "env_ac_ghgfp",
      filters = list(
        time = years,
        na_item = "TOTAL",
        c_dest = "WORLD"
      ),
      cache = FALSE
    ),
    c(
      "freq", "unit", "na_item", "c_dest",
      "c_orig", "nace_r2", "TIME_PERIOD", "OBS_VALUE"
    ),
    use_temp_data,
    allow_download,
    normalize_eurostat_accounts_source
  )

  # --- Correspondances des branches et pays d'origine -------------------------

  nace <- read_delim(
    "obs_accounts/ghg/eurostat_correspondence_table_nace_r2.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(industry = figaro_industry, nace_r2)

  countries <- read_delim(
    "obs_accounts/ghg/eurostat_correspondence_table_c_orig.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(country = figaro_country, c_orig)

  # --- Selection du total, destination WORLD, et conversion kt -> t CO2e ------
  # Les emissions doivent etre positives ou nulles ; une valeur nulle est valide.

  data %>%
    filter(
      freq == "A",
      unit == "THS_T",
      na_item == "TOTAL",
      c_dest == "WORLD",
      OBS_VALUE >= 0,
      TIME_PERIOD %in% years
    ) %>%
    inner_join(nace, by = "nace_r2") %>%
    inner_join(countries, by = "c_orig") %>%
    transmute(
      year = as.character(TIME_PERIOD),
      country,
      industry,
      value = OBS_VALUE * 1e3,
      unit = "TCO2E"
    )
}

# =============================================================================
# 2. OCDE : comptes d'emissions atmospheriques (DSD_AEA)
# =============================================================================

load_oecd_ghg_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Choix du cache selon les annees disponibles ----------------------------

  years <- as.character(years)

  path <- file.path(download_dir, paste0("oecd_aea_", min(years), "_", max(years), ".csv"))

  # Retrouver un cache de production contenant des annees demandees, ou l'ancien
  # cache du controle. Les annees absentes restent visibles dans la couverture.

  if (!file.exists(path) && use_temp_data) {
    candidates <- list.files(
      download_dir,
      "^oecd_aea_[0-9]{4}_[0-9]{4}[.]csv$",
      full.names = TRUE
    )

    starts <- as.integer(sub("oecd_aea_([0-9]{4})_.*", "\\1", basename(candidates)))
    ends <- as.integer(sub(".*_([0-9]{4})[.]csv$", "\\1", basename(candidates)))
    overlap <- starts <= as.integer(max(years)) & ends >= as.integer(min(years))

    # Privilegier le cache couvrant la periode la plus large parmi ceux trouves.

    candidates <- candidates[order(ends - starts, decreasing = TRUE)]
    overlap <- overlap[order(ends - starts, decreasing = TRUE)]
    candidates <- candidates[overlap]

    if (length(candidates) > 0L) path <- candidates[[1]]
    else if (file.exists(file.path(download_dir, "DSD_AEA_GHG.csv"))) path <- file.path(download_dir, "DSD_AEA_GHG.csv")
  }

  # --- Requete et lecture de la source ---------------------------------------
  # Les annees sont filtrees apres lecture, meme si un cache plus large est repris.

  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.2/..EMISSIONS.T_CO2E+T...GHG...?",
    "startPeriod=",
    min(years),
    "&endPeriod=",
    max(years),
    "&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )

  data <- read_accounts_source_cache(
    path,
    function() read.csv(
      url,
      colClasses = "character"
    ),
    c(
      "MEASURE", "POLLUTANT", "METHODOLOGY", "ACTION", "FREQ", "UNIT_MEASURE", "SOURCE",
      "ACTIVITY_SCOPE", "ADJUSTMENT", "REF_AREA", "ACTIVITY", "TIME_PERIOD", "OBS_VALUE"
    ),
    use_temp_data,
    allow_download
  )

  # --- Correspondance des activites OCDE vers les branches FIGARO -------------

  activities <- read_delim(
    "obs_accounts/ghg/oecd_correspondence_table_activity.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    transmute(industry = figaro_industry, oecd_activity)

  # --- Selection des emissions GHG declarees selon le principe de residence ---
  # Les valeurs sont deja en tonnes CO2e. Convertir les pays et appliquer les
  # correspondances d'activites sans modifier l'echelle des emissions.

  data %>%
    filter(
      MEASURE == "EMISSIONS",
      POLLUTANT == "GHG",
      METHODOLOGY == "EMISSIONS_SEEA",
      ACTION == "I",
      FREQ == "A",
      UNIT_MEASURE == "T_CO2E",
      SOURCE == "REPORTED",
      ACTIVITY_SCOPE == "RES",
      ADJUSTMENT == "N",
      TIME_PERIOD %in% years
    ) %>%
    transmute(
      year = as.character(TIME_PERIOD),
      country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
      oecd_activity = ACTIVITY,
      value = OBS_VALUE,
      unit = "TCO2E"
    ) %>%
    inner_join(activities, by = "oecd_activity") %>%
    select(year, country, industry, value, unit) %>%
    filter(!is.na(value))
}

# =============================================================================
# 3. Extension des comptes : estimation EDGAR / AEA
# =============================================================================

load_estimated_ghg_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  # --- Perimetre actuel de l'extension : 2024 uniquement -----------------------
  # Retourner une table vide si cette annee n'est pas demandee.

  if (!"2024" %in% as.character(years)) {
    return(tibble(
      year = character(),
      country = character(),
      industry = character(),
      value = double(),
      unit = character()
    ))
  }

  # --- Lecture des estimations ou calcul de l'extension si autorise -----------
  # Un controle local lit le cache ; le calcul EDGAR requiert un rafraichissement.

  data <- read_accounts_source_cache(
    file.path(download_dir, "estimated_ghg_accounts.csv"),
    function() {
      source(
        "obs_accounts/ghg/extended-eurostat-ghg-accounts/ghg_accounts_extensions.R",
        encoding = "UTF-8-BOM"
      )

      estimate_ghg_from_edgar(
        years = "2024",
        use_temp_data = use_temp_data,
        verbose = verbose
      )
    },
    c("year", "country", "industry", "value", "unit"),
    use_temp_data,
    allow_download
  )

  # --- Conversion THS_TCO2E -> TCO2E ------------------------------------------
  # L'extension produit des milliers de tonnes, convertis ici en tonnes.

  data %>%
    filter(as.character(year) %in% as.character(years), !is.na(value)) %>%
    transmute(
      year = as.character(year),
      country,
      industry,
      value = value * 1e3,
      unit = "TCO2E"
    )
}

# =============================================================================
# 4. Combiner les sources selon la priorite de production
# =============================================================================
# Meme choix pour le producteur et le controle, pour chaque annee/pays/branche.
# L'ordre d'assemblage determine la priorite : Eurostat, extension, puis OCDE.
# Ecarter les valeurs manquantes avant de garder la premiere ligne disponible.

select_ghg_accounts_sources <- function(
  eurostat,
  estimated,
  oecd
) {

  bind_rows(
    list(
      eurostat = eurostat,
      estimated = estimated,
      oecd = oecd
    ),
    .id = "source"
  ) %>%
    filter(!is.na(value)) %>%
    distinct(year, country, industry, .keep_all = TRUE)
}
