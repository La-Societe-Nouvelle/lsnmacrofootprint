# La Societe Nouvelle

check_art_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for ART accounts (observed data)")
  # -------------------------------------------------------------------
  # Utils

  format_percent <- function(x) round(100 * x, digits = 1)

  section_title <- function(title) {
    cat("\n", strrep("-", 80), "\n", title, "\n", strrep("-", 80), "\n", sep = "")
  }
  empty_check <- function(years) {
    tibble(
      year = as.character(years),
      status = "NOK",
      available_data_fr = 0,
      available_data_row = 0,
      available_data_countries = 0
    )
  }

  ensure_file <- function(file_path, url, source_name) {
    if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

    if (!file.exists(file_path) || !use_temp_data) {
      if (verbose) cat("Downloading ", source_name, "...\n", sep = "")
      curl_download(
        url = url,
        destfile = file_path,
        quiet = !verbose
      )
    }

    file.exists(file_path)
  }

  years <- as.character(years)

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  insee_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    mutate(
      code_ape = gsub("\\.", "", code)
    ) %>%
    select(code_ape, industry)

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(industry = code) %>%
    select(industry)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  n_industries <- nrow(figaro_industries)

  # -------------------------------------------------------------------
  # SIRENE data - StockEtablissements

  if (verbose) cat("Checking SIRENE data - StockEtablissements...\n")

  stock_etablissements_file_path <- file.path(download_dir, "StockEtablissements.parquet")
  stock_etablissements_url <- "https://www.data.gouv.fr/api/1/datasets/r/a29c1297-1f92-4e2a-8f6b-8c902ce96c5f"

  stock_etablissements_available <- FALSE

  if (ensure_file(
    stock_etablissements_file_path,
    stock_etablissements_url,
    "StockEtablissements"
  )) {
    stock_etablissements_available <- TRUE
    stock_etablissements <- open_dataset(stock_etablissements_file_path, format = "parquet")

    stock_etablissements_industries <- stock_etablissements %>%
      select(
        nomenclatureActivitePrincipaleEtablissement,
        activitePrincipaleEtablissement,
        etatAdministratifEtablissement
      ) %>%
      filter(
        etatAdministratifEtablissement == "A",
        nomenclatureActivitePrincipaleEtablissement == "NAFRev2",
        activitePrincipaleEtablissement != "00.00Z"
      ) %>%
      distinct(activitePrincipaleEtablissement) %>%
      collect() %>%
      mutate(code_ape = gsub("\\.", "", activitePrincipaleEtablissement)) %>%
      inner_join(insee_nace_niv5, by = "code_ape") %>%
      distinct(industry)

    available_fr <- format_percent(nrow(stock_etablissements_industries) / n_industries)

    stock_etablissements_message <- paste0(
      "Ann\u00e9e de r\u00e9f\u00e9rence pour la part d'\u00e9tablissements artisanaux par code APE : ",
      as.integer(format(Sys.Date(), "%Y")),
      " (available_data_fr = ",
      available_fr,
      "%)"
    )
  } else {
    stock_etablissements_message <- paste0(
      "Ann\u00e9e de r\u00e9f\u00e9rence pour la part d'\u00e9tablissements artisanaux par code APE : ",
      as.integer(format(Sys.Date(), "%Y")),
      " (StockEtablissements indisponible)"
    )
  }

  # -------------------------------------------------------------------
  # SIRENE data - StockEtablissementsHistorique

  if (verbose) cat("Checking SIRENE data - StockEtablissementsHistorique...\n")

  stock_etablissements_historique_file_path <- file.path(download_dir, "StockEtablissementsHistorique.parquet")
  stock_etablissements_historique_url <- "https://www.data.gouv.fr/api/1/datasets/r/2b3a0c79-f97b-46b8-ac02-8be6c1f01a8c"

  stock_etablissements_historique_check <- empty_check(years)

  if (ensure_file(
    stock_etablissements_historique_file_path,
    stock_etablissements_historique_url,
    "StockEtablissementsHistorique"
  )) {
    stock_etablissements_historique <- open_dataset(
      stock_etablissements_historique_file_path,
      format = "parquet"
    )

    stock_etablissements_historique_check <- map_dfr(years, function(y) {
      active_industries <- stock_etablissements_historique %>%
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
        distinct(activitePrincipaleEtablissement) %>%
        collect() %>%
        mutate(code_ape = gsub("\\.", "", activitePrincipaleEtablissement)) %>%
        inner_join(insee_nace_niv5, by = "code_ape") %>%
        distinct(industry)

      available_fr <- format_percent(nrow(active_industries) / n_industries)

      tibble(
        year = y,
        status = if_else(nrow(active_industries) > 0, "OK", "NOK"),
        available_data_fr = available_fr,
        available_data_row = 100,
        available_data_countries = 100
      )
    })
  }

  # -------------------------------------------------------------------
  # Synthese

  synthesis <- if (stock_etablissements_available) {
    stock_etablissements_historique_check
  } else {
    stock_etablissements_historique_check %>%
      mutate(
        status = "NOK",
        available_data_fr = 0,
        available_data_row = 0,
        available_data_countries = 0
      )
  }

  # -------------------------------------------------------------------
  # Output

  section_title("StockEtablissements")
  cat(stock_etablissements_message, "\n", sep = "")

  section_title("StockEtablissementsHistorique")
  print(stock_etablissements_historique_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    StockEtablissements = stock_etablissements_message,
    StockEtablissementsHistorique = stock_etablissements_historique_check,
    synthesis = synthesis
  ))
}







