# La Societe Nouvelle

# Sources partagees par le producteur et le controle.
# Les valeurs, filtres et annees de reference conservent la methode existante.

# =============================================================================
# 1. UNEP : extraction domestique de matieres
# =============================================================================

load_unep_mat_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture du cache ou acquisition explicite -----------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "mfa4.csv"),
    function() {
      read.csv("https://unep-irp-resourcepanel.resource-use.global/mfa4/export?flowTypes[0]=DE")
    },
    c("Country", "Flow.code", "Flow.unit", "Category"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Filtres et preparation utilises par le producteur et le controle --------

  unep_data <- raw_data %>%
    pivot_longer(
      6:ncol(.),
      names_to = "year",
      values_to = "value"
    ) %>%
    filter(
      Flow.code == "DE",
      Flow.unit == "t",
      Category %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    ) %>%
    mutate(
      year = sub("^X", "", year),
      country = countrycode(Country, 'country.name', 'iso2c', nomatch = NULL),
      flow = Flow.code,
      category = Category,
      unit = "T"
    ) %>%
    filter(year %in% years$year) %>%
    select(year, country, flow, category, value, unit)

  unep_data
}

