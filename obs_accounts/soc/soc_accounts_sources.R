# La Societe Nouvelle

# Sources partagees par le producteur et le controle.
# Les valeurs, filtres et annees de reference conservent la methode existante.

# =============================================================================
# 1. URSSAF : masse salariale ESS par secteur NA88
# =============================================================================

load_urssaf_soc_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture du cache ou acquisition explicite -----------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88.csv"),
    function() {
      read.csv(
        "https://open.urssaf.fr/api/explore/v2.1/catalog/datasets/nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B",
        sep = ";",
        check.names = TRUE
      )
    },
    c(
      "X.Champ.ESS..",
      "Famille.ESS",
      "Grand.secteur.d.activité",
      "Secteur.NA88",
      "Année",
      "Nombre.d.établissements",
      "Effectifs.salariés.moyens",
      "Masse.salariale..brute."
    ),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Filtres et preparation utilises par le producteur et le controle --------

  urssaf_raw_data <- raw_data %>%
    rename(
      champ_ess = X.Champ.ESS..,
      famille_ess = Famille.ESS,
      grand_secteur_activite = Grand.secteur.d.activité,
      secteur_na88 = Secteur.NA88,
      annee = Année,
      nombre_etablissements = Nombre.d.établissements,
      effectifs_salaries_moyens = Effectifs.salariés.moyens,
      masse_salariale_brute = Masse.salariale..brute.
    ) %>%
    mutate(
      secteur_na88 = sub(" .*", "", secteur_na88),
      champ_ess = champ_ess == "OUI"
    ) %>%
    select(annee, secteur_na88, champ_ess, masse_salariale_brute)

  urssaf_data <- urssaf_raw_data %>%
    filter(annee %in% years$year) %>%
    group_by(annee, secteur_na88) %>%
    summarise(
      masse_salariale = sum(masse_salariale_brute, na.rm = TRUE),
      masse_salariale_ess = sum(if_else(champ_ess, masse_salariale_brute, 0), na.rm = TRUE),
      .groups = "drop"
    )

  urssaf_data
}

