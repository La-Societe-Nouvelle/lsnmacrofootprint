# La Societe Nouvelle

# Chargeurs partages par le producteur et le controle de disponibilite.

# =============================================================================
# SIRENE : stock des etablissements
# =============================================================================

load_sirene_art_stock_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture Parquet et validation avant remplacement du cache --------------

  stock_etablissements <- read_accounts_file_source(
    file.path(download_dir, "StockEtablissements.parquet"),
    fetch = function(
      destination
    ) {
      curl_download(
        "https://www.data.gouv.fr/api/1/datasets/r/a29c1297-1f92-4e2a-8f6b-8c902ce96c5f",
        destination,
        quiet = !verbose
      )
    },
    read_file = function(
      path
    ) open_dataset(path, format = "parquet"),
    columns = c(
      "nomenclatureActivitePrincipaleEtablissement",
      "activitePrincipaleEtablissement",
      "activitePrincipaleRegistreMetiersEtablissement",
      "etatAdministratifEtablissement"
    ),
    use_temp_data = use_temp_data,
    allow_download = allow_download
  )

  # --- Filtrer les etablissements actifs et compter par code APE --------------

  etablissements_artisanaux <- stock_etablissements %>%
    select(
      nomenclatureActivitePrincipaleEtablissement,
      activitePrincipaleEtablissement,
      activitePrincipaleRegistreMetiersEtablissement,
      etatAdministratifEtablissement
    ) %>%
    filter(
      etatAdministratifEtablissement == "A",
      nomenclatureActivitePrincipaleEtablissement == "NAFRev2",
      activitePrincipaleEtablissement != "00.00Z"
    ) %>%
    group_by(activitePrincipaleEtablissement) %>%
    summarise(
      n_ets = n(),
      n_ets_artisanaux = sum(
        activitePrincipaleRegistreMetiersEtablissement != "",
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    collect() %>%
    select(activitePrincipaleEtablissement, n_ets, n_ets_artisanaux) %>%
    arrange(activitePrincipaleEtablissement)

  etablissements_artisanaux
}

# =============================================================================
# SIRENE : historique des etablissements
# =============================================================================

load_sirene_art_history_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture Parquet et validation avant remplacement du cache --------------

  stock_etablissements_historique <- read_accounts_file_source(
    file.path(download_dir, "StockEtablissementsHistorique.parquet"),
    fetch = function(
      destination
    ) {
      curl_download(
        "https://www.data.gouv.fr/api/1/datasets/r/2b3a0c79-f97b-46b8-ac02-8be6c1f01a8c",
        destination,
        quiet = !verbose
      )
    },
    read_file = function(
      path
    ) open_dataset(path, format = "parquet"),
    columns = c(
      "nomenclatureActivitePrincipaleEtablissement",
      "activitePrincipaleEtablissement",
      "dateDebut",
      "dateFin",
      "etatAdministratifEtablissement"
    ),
    use_temp_data = use_temp_data,
    allow_download = allow_download
  )

  # --- Filtrer les etablissements actifs et compter par code APE --------------

  etablissements <- map_dfr(years$year, function(
    y
  )
  {
    # dénombrement des établissements par année
    stock_etablissements_historique %>%
      select(
        nomenclatureActivitePrincipaleEtablissement,
        activitePrincipaleEtablissement,
        dateDebut,
        dateFin,
        etatAdministratifEtablissement
      ) %>%
      filter(
        etatAdministratifEtablissement == "A",
        nomenclatureActivitePrincipaleEtablissement == "NAFRev2",
        activitePrincipaleEtablissement != "00.00Z",
        dateDebut <= as.Date(paste0(y, "-01-01")),
        is.na(dateFin) | dateFin >= as.Date(paste0(y, "-01-01"))
      ) %>%
      group_by(activitePrincipaleEtablissement) %>%
      count(name = "n_ets") %>%
      collect() %>%
      mutate(year = y)
  }) %>%
    arrange(year, activitePrincipaleEtablissement, n_ets)

  etablissements
}

