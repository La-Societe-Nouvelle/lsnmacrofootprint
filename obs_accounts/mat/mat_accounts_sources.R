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
    mutate(value = as.numeric(value)) %>%
    # Entites historiques ramenees au meme code (USSR/RU, Sudan (Former)/SD...) :
    # valeurs nulles ou absentes sur la periode, agregees par code pays.
    group_by(year, country, flow, category, unit) %>%
    summarise(
      value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(year, country, flow, category, value, unit)

  # --- Reste du monde FIGARO (FIGW1) : total mondial - pays FIGARO ------------
  # Calcule en residu plutot qu'impute par similarite : le total mondial UNEP
  # est ainsi respecte.

  figaro_countries <- read_delim(
    "metadata/metadata_figaro_countries.csv",
    delim = ";",
    show_col_types = FALSE
  )$code

  figaro_countries_data <- unep_data %>%
    filter(country %in% figaro_countries, country != "FIGW1") %>%
    group_by(year, flow, category, unit) %>%
    summarise(figaro_value = sum(value, na.rm = TRUE), .groups = "drop")

  rest_of_world_data <- unep_data %>%
    filter(country == "World") %>%
    left_join(figaro_countries_data, by = c("year", "flow", "category", "unit")) %>%
    mutate(
      country = "FIGW1",
      value = pmax(value - coalesce(figaro_value, 0), 0)
    ) %>%
    select(year, country, flow, category, value, unit)

  unep_data %>%
    filter(country != "FIGW1") %>%
    bind_rows(rest_of_world_data)
}

