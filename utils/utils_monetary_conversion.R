# La Societe Nouvelle

# Taux de change OCDE (table 4 des comptes nationaux, EXC_A : unites de monnaie
# nationale par dollar US, moyenne annuelle).
#
# Les fichiers sont des sources brutes : ils sont stockes dans download_dir
# (data_raw) et suivent les regles communes de read_accounts_source_cache() :
#   - use_temp_data = TRUE  : le cache existant est reutilise ;
#   - use_temp_data = FALSE : la source est telechargee de nouveau ;
#   - allow_download = FALSE : aucun acces reseau, erreur si le cache manque.
# Un echec de telechargement est une erreur : aucun ancien fichier ni taux de
# substitution n'est utilise silencieusement.

if (!exists("read_accounts_source_cache", mode = "function")) {
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
}

# =============================================================================
# 1. Charger une serie de taux de change OCDE
# =============================================================================

oecd_exchange_rates_url <- function(
  areas
) {
  paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.",
    paste(areas, collapse = "+"),
    "...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )
}

load_oecd_exchange_rates_source <- function(
  cache_key,
  areas,
  use_temp_data = TRUE,
  allow_download = TRUE,
  verbose = FALSE
) {

  # Les scripts de desagregation chargent ce module sans scripts/setup.R.
  raw_dir <- if (exists("download_dir", inherits = TRUE)) download_dir else "data_raw"
  url <- oecd_exchange_rates_url(areas)

  raw_data <- read_accounts_source_cache(
    file.path(raw_dir, paste0(cache_key, ".csv")),
    function() {
      if (verbose) message("Downloading OECD exchange rates: ", cache_key)
      destination <- tempfile(fileext = ".csv")
      on.exit(unlink(destination), add = TRUE)
      handle <- curl::new_handle(timeout = 180, connecttimeout = 30)
      curl::curl_download(url, destination, quiet = !verbose, handle = handle)
      read.csv(destination)
    },
    c("REF_AREA", "TIME_PERIOD", "OBS_VALUE"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = "OBS_VALUE"
  )

  raw_data %>%
    transmute(
      year = as.character(TIME_PERIOD),
      area = as.character(REF_AREA),
      value = OBS_VALUE
    )
}

# =============================================================================
# 2. Extraire un coefficient de conversion vers l'euro
# =============================================================================

# Coefficient (EUR par unite de monnaie) = taux EUR/USD / taux monnaie/USD.
# Sans denominateur (dollar US), le coefficient est directement le taux EUR/USD.
# Vectorise sur year ; erreur si un taux manque, est duplique ou invalide.
oecd_rate_to_euro <- function(
  rates,
  year,
  currency_area = NULL,
  cache_key = ""
) {

  year <- as.character(year)

  rate_of <- function(
    area,
    year_i
  ) {
    value <- rates$value[rates$area == area & rates$year == year_i]
    if (length(value) != 1L || !is.finite(value) || value <= 0) {
      stop(
        "OECD exchange rate unavailable for ", area, " / ", year_i,
        " (", cache_key, "): ", length(value), " valid value(s) expected 1. ",
        "Refresh the source with use_temp_data = FALSE.",
        call. = FALSE
      )
    }
    value
  }

  vapply(year, function(year_i) {
    eur <- rate_of("EU27_2020", year_i)
    if (is.null(currency_area)) eur else eur / rate_of(currency_area, year_i)
  }, numeric(1), USE.NAMES = FALSE)
}

convert_to_euro_coef <- function(
  year,
  cache_key,
  currency_area,
  use_temp_data,
  allow_download,
  verbose
) {
  areas <- c(currency_area, "EU27_2020")
  rates <- load_oecd_exchange_rates_source(cache_key, areas, use_temp_data, allow_download, verbose)
  oecd_rate_to_euro(rates, year, currency_area, cache_key)
}

# =============================================================================
# 3. Coefficients publics (1 unite de monnaie x coef -> EUR)
# =============================================================================

from_pound_to_euro <- function(year, use_temp_data = TRUE, allow_download = TRUE, verbose = FALSE) {
  convert_to_euro_coef(year, "OECD_POUND", "GBR", use_temp_data, allow_download, verbose)
}

from_usd_to_euro <- function(year, use_temp_data = TRUE, allow_download = TRUE, verbose = FALSE) {
  convert_to_euro_coef(year, "OECD_DOLLAR", NULL, use_temp_data, allow_download, verbose)
}

from_cad_to_euro <- function(year, use_temp_data = TRUE, allow_download = TRUE, verbose = FALSE) {
  convert_to_euro_coef(year, "OECD_CDOLLAR", "CAN", use_temp_data, allow_download, verbose)
}

from_dkk_to_euro <- function(year, use_temp_data = TRUE, allow_download = TRUE, verbose = FALSE) {
  convert_to_euro_coef(year, "OECD_DKK", "DNK", use_temp_data, allow_download, verbose)
}
