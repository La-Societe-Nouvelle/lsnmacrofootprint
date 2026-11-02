# La Societe Nouvelle

# Sources partagees par le producteur et le controle.
# Les valeurs, filtres et annees de reference conservent la methode existante.

# =============================================================================
# 1. ILOSTAT : remuneration horaire par sexe et secteur
# =============================================================================

load_ilostat_geq_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture du cache ou acquisition explicite -----------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "EAR_EHRA_SEX_ECO_CUR_NB_A.csv"),
    function() {
      get_ilostat("EAR_EHRA_SEX_ECO_CUR_NB_A", segment = "indicator", quiet = TRUE)
    },
    c("classif2", "classif1", "sex", "time", "ref_area", "obs_value"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Filtres et preparation utilises par le producteur et le controle --------

  ilostat_data <- raw_data %>%
    filter(
      classif2 == "CUR_TYPE_PPP",
      str_starts(classif1, "ECO_ISIC4_"),
      sex %in% c("SEX_M","SEX_F","SEX_T")
    ) %>%
    # keep closest years
    crossing(years) %>%
    mutate(
      time_num = as.integer(time),
      ecart = abs(time_num - as.integer(year))
    ) %>%
    group_by(year) %>%
    filter(ecart == min(ecart, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      country = countrycode(ref_area, "iso3c", "iso2c",nomatch = NULL),
      sector = sub("ECO_ISIC4_", "", classif1),
      sex = case_when(
        sex == "SEX_M" ~ "men",   # Hommes
        sex == "SEX_F" ~ "women", # Femmes
        sex == "SEX_T" ~ "all",   # Total
        TRUE ~ NA
      ),
      hourly_rate = obs_value
    ) %>%
    select(year, country, sector, sex, hourly_rate)

  ilostat_data
}

