# La Societe Nouvelle

# Sources partagees par le producteur et le controle.
# Les valeurs, filtres et annees de reference conservent la methode existante.

# =============================================================================
# 1. Eurostat PRODCOM : quantites produites en kilogrammes
# =============================================================================

load_prodcom_haz_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture du cache ou acquisition explicite -----------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "ds-059358.csv"),
    function() {
      map_dfr(years$year, function(
        year_i
      ) {
        url <- paste0(
          "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-059358/1.0?",
          "format=csvdata&c[freq]=A&c[TIME_PERIOD]=", year_i,
          "&c[indicators]=PRODQNT,QNTUNIT&compressed=true"
        )
        as.data.frame(fread(url, colClasses = "character", sep = ",", showProgress = verbose))
      })
    },
    c("indicators", "OBS_VALUE", "TIME_PERIOD", "freq", "reporter", "product"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Filtres et preparation utilises par le producteur et le controle --------

  prodcom_data <- raw_data %>%
    pivot_wider(
      names_from = indicators,
      values_from = OBS_VALUE
    ) %>%
    filter(
      QNTUNIT == "KG",
      TIME_PERIOD %in% years$year,
      freq == "A"
    ) %>%
    mutate(
      year = as.character(TIME_PERIOD),
      country = reporter,
      value = as.numeric(PRODQNT),
      unit = QNTUNIT
    ) %>%
    select(year, country, product, value, unit)

  prodcom_data
}

