# La Societe Nouvelle

# Helpers communs aux chargeurs de comptes observes et a leurs controles.

# =============================================================================
# 1. Harmoniser les formats Eurostat
# =============================================================================

# Les nouveaux caches Eurostat utilisent les colonnes du CSV SDMX.
# Accepter aussi les anciens caches get_eurostat(), sans les reecrire a la lecture.
normalize_eurostat_accounts_source <- function(
  data
) {

  if (!"TIME_PERIOD" %in% names(data) && "time" %in% names(data)) {
    names(data)[names(data) == "time"] <- "TIME_PERIOD"
  }

  if (!"OBS_VALUE" %in% names(data) && "values" %in% names(data)) {
    names(data)[names(data) == "values"] <- "OBS_VALUE"
  }

  if ("TIME_PERIOD" %in% names(data)) data$TIME_PERIOD <- substr(as.character(data$TIME_PERIOD), 1, 4)

  data
}

# =============================================================================
# 2. Lire, valider et enregistrer un cache de source
# =============================================================================

# Valider le telechargement avant de remplacer un cache existant.
# allow_download = FALSE reserve la lecture aux fichiers deja presents.
# fetch est une fonction appelee uniquement quand une acquisition est necessaire.
read_accounts_source_cache <- function(
  path,
  fetch,
  columns,
  use_temp_data = TRUE,
  allow_download = TRUE,
  normalize = identity,
  read_cache = function(
    path
  ) read.csv(path, colClasses = "character"),
  numeric_columns = c("OBS_VALUE", "value")
) {

  # --- Decider si une acquisition est necessaire et autorisee -----------------

  must_fetch <- !file.exists(path) || !use_temp_data

  if (must_fetch && !allow_download) {
    stop("Cache unavailable: ", path, ". Use refresh_sources = TRUE to download it.")
  }

  # --- Lire les donnees et verifier les colonnes attendues --------------------
  # Garder les codes tels que "T" en texte : read.csv() peut sinon les lire
  # comme des booleens. Les seules mesures numeriques sont converties ensuite.

  data <- normalize(if (must_fetch) fetch() else read_cache(path))

  missing <- setdiff(columns, names(data))

  if (length(missing) > 0L) {
    stop(
      "Invalid source ",
      basename(path),
      ": missing columns ",
      paste(missing, collapse = ", ")
    )
  }

  # --- Convertir les mesures en nombres, en conservant les codes en texte -----

  for (measure in intersect(numeric_columns, names(data))) {
    data[[measure]] <- suppressWarnings(as.numeric(data[[measure]]))
  }

  # --- Enregistrer uniquement les donnees nouvellement acquises ---------------
  # Une simple lecture de cache ne modifie aucun fichier.

  if (must_fetch) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data, path, row.names = FALSE)
  }

  data
}

# =============================================================================
# 3. Lire les sources en fichiers binaires (notamment les Parquet SIRENE)
# =============================================================================
# Le fichier temporaire est valide avant de remplacer le cache. Une table Arrow
# est rouverte sur le chemin final pour conserver un fichier source accessible.
read_accounts_file_source <- function(
  path,
  fetch,
  read_file,
  columns,
  use_temp_data = TRUE,
  allow_download = TRUE
) {

  must_fetch <- !file.exists(path) || !use_temp_data

  if (must_fetch && !allow_download) {
    stop("Cache unavailable: ", path, ". Use refresh_sources = TRUE to download it.")
  }

  candidate <- path

  if (must_fetch) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    candidate <- tempfile(".source-", tmpdir = dirname(path), fileext = paste0(".", tools::file_ext(path)))
    on.exit(unlink(candidate), add = TRUE)
    fetch(candidate)
  }

  data <- read_file(candidate)
  missing <- setdiff(columns, names(data))

  if (length(missing) > 0L) {
    stop("Invalid source ", basename(path), ": missing columns ", paste(missing, collapse = ", "))
  }

  if (must_fetch) {
    if (!file.copy(candidate, path, overwrite = TRUE)) stop("Unable to save source: ", path)
    # Les donnees deja materialisees (CSV BTS, agregats Parquet) sont reutilisees.
    # Un Dataset Arrow doit pointer vers le fichier final apres retrait du temporaire.
    if (inherits(data, "Dataset")) data <- read_file(path)
  }

  data
}

# =============================================================================
# 4. Mesurer la couverture des donnees chargees, avant imputation
# =============================================================================
# units contient les branches (ou secteurs) attendus. Une source nationale
# utilise scope = "country" ; une source francaise utilise "fr_industry".
accounts_source_coverage <- function(
  data,
  years,
  units,
  countries,
  scope = "industry",
  structural_zero_row = FALSE
) {

  map_dfr(as.character(years), function(
    year_i
  ) {
    rows <- data %>% filter(as.character(year) == year_i, country %in% countries)

    if (scope == "country") {
      rows <- distinct(rows, country)
      fr <- as.numeric(any(rows$country == "FR"))
      row <- sum(rows$country != "FR") / max(sum(countries != "FR"), 1)
    } else {
      rows <- rows %>% filter(industry %in% units) %>% distinct(country, industry)
      fr <- sum(rows$country == "FR") / max(length(units), 1)
      row <- sum(rows$country != "FR") / max(sum(countries != "FR") * length(units), 1)
    }

    country_share <- n_distinct(rows$country) / max(length(countries), 1)

    if (scope == "fr_industry") row <- 0
    if (structural_zero_row && nrow(rows) > 0L) {
      row <- 1
      country_share <- 1
    }

    tibble(
      year = year_i,
      status = if (nrow(rows) > 0L) "OK" else "NOK",
      available_data_fr = round(100 * fr, 1),
      available_data_row = round(100 * row, 1),
      available_data_countries = round(100 * country_share, 1)
    )
  })
}

print_accounts_source_checks <- function(
  reports
) {

  for (name in names(reports)) {
    cat("\n", strrep("-", 80), "\n", name, "\n", strrep("-", 80), "\n", sep = "")
    print(reports[[name]])
  }

  invisible(reports)
}

# =============================================================================
# 5. Charger une source pour un rapport de disponibilite
# =============================================================================

# Un echec de lecture reste visible dans le rapport de controle. Il ne declenche
# ni nouvelle tentative reseau ni utilisation silencieuse d'un ancien fichier.
load_accounts_source_for_check <- function(
  loader,
  years,
  refresh_sources,
  verbose
) {

  tryCatch(
    # Un rafraichissement explicite autorise l'acquisition et ignore le cache.

    loader(
      years = years,
      use_temp_data = !refresh_sources,
      allow_download = refresh_sources,
      verbose = verbose
    ),

    error = function(
      error
    ) {

      # Fournir une table vide au calcul de couverture et conserver le diagnostic.

      data <- tibble(
        year = character(),
        country = character(),
        industry = character(),
        value = double()
      )

      attr(data, "source_error") <- conditionMessage(error)

      if (verbose) message(conditionMessage(error))

      data
    }
  )
}
