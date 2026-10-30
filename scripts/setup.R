# La Societe Nouvelle

# Charger depuis la racine du projet : source("scripts/setup.R", encoding = "UTF-8").
# Etapes : configuration, dependances, puis fonctions de calcul et d'export.
# Les calculs et les exports sont lances ensuite par les commandes de l'utilisateur.

# =============================================================================
# 1. Reperer le projet et preparer le chargement des scripts
# =============================================================================

# Retrouver le source() le plus recent, y compris depuis une fonction R.
setup_path <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) "" else frame$ofile
}, character(1))
setup_path <- normalizePath(tail(setup_path[nzchar(setup_path)], 1L), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(setup_path), ".."), winslash = "/", mustWork = TRUE)

# Charger les fichiers du projet en UTF-8, avec ou sans marqueur BOM.
source_project_file <- function(path) {
  source(file.path(project_root, path), encoding = "UTF-8-BOM")
}

# =============================================================================
# 2. Lire la configuration locale
# =============================================================================

# Les options de ce fichier sont relues a chaque chargement du setup.
# Regler notamment install_missing_packages et do_clean_outliers avant ce source().
source_project_file("config/config.R")

# =============================================================================
# 3. Declarer les dependances et les fonctions d'installation
# =============================================================================

# CRAN : packages communs aux scripts. GitHub : FBI pour le nettoyage optionnel.
project_packages <- list(
  cran = c(
    "arrow", "countrycode", "curl", "data.table", "DBI", "doFuture",
    "dplyr", "eurostat", "foreach", "future", "jsonlite", "parallel",
    "progressr", "purrr", "readr", "readsdmx", "readxl", "Rilostat",
    "RPostgres", "rvest", "stringr", "tibble", "tidyr"
  ),
  github = c(fbi = "cykbennie/fbi")
)

# Inclure FBI uniquement lorsque le nettoyage des valeurs atypiques est active.
project_dependencies <- function(do_clean_outliers = TRUE) {
  c(project_packages$cran, if (isTRUE(do_clean_outliers)) names(project_packages$github))
}

# Verifier la disponibilite des namespaces sans attacher les packages a la session.
missing_project_dependencies <- function(do_clean_outliers = TRUE) {
  packages <- project_dependencies(do_clean_outliers)
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}

# Installer remotes au besoin, puis le package GitHub et ses dependances.
install_github_dependency <- function(repo, repos) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos = repos)
  if (!requireNamespace("remotes", quietly = TRUE)) stop("Install remotes to install GitHub dependencies.")
  remotes::install_github(repo, dependencies = NA, upgrade = "never", repos = repos)
}

# Installer les packages manquants, puis verifier le resultat de l'installation.
install_project_dependencies <- function(do_clean_outliers = TRUE, repos = getOption("repos")) {
  # Fournir un miroir CRAN si aucun n'a ete choisi dans la session.
  if (is.null(repos) || length(repos) == 0L) repos <- c(CRAN = "https://cloud.r-project.org")
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"

  # Installer d'abord les dependances CRAN, puis celles provenant de GitHub.
  missing <- missing_project_dependencies(do_clean_outliers)
  missing_cran <- intersect(missing, project_packages$cran)
  if (length(missing_cran) > 0L) {
    message("Installing CRAN dependencies: ", paste(missing_cran, collapse = ", "))
    install.packages(missing_cran, repos = repos)
  }

  missing_github <- intersect(missing, names(project_packages$github))
  if (length(missing_github) > 0L) {
    for (package in missing_github) {
      message("Installing GitHub dependency: ", package)
      install_github_dependency(project_packages$github[[package]], repos)
    }
  }

  # install.packages() peut terminer avec un avertissement sans avoir tout installe.
  remaining <- missing_project_dependencies(do_clean_outliers)
  if (length(remaining) > 0L) {
    stop("Dependencies still missing after installation: ", paste(remaining, collapse = ", "))
  }
  message("All selected project dependencies are available.")
  invisible(missing)
}

# =============================================================================
# 4. Verifier les dependances et charger les packages
# =============================================================================

# L'installation automatique depend de config/config.R. Si elle est desactivee,
# arreter le setup en indiquant les packages manquants et l'option a modifier.
required_packages <- project_packages$cran
missing_packages <- missing_project_dependencies(do_clean_outliers)
if (length(missing_packages) > 0) {
  if (isTRUE(install_missing_packages)) {
    install_project_dependencies(do_clean_outliers)
  } else {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Set install_missing_packages <- TRUE in config/config.R, then source scripts/setup.R again."
    )
  }
}

# Attacher les packages CRAN utilises par les scripts. FBI reste appele via fbi::.
invisible(lapply(required_packages, function(package) {
  suppressPackageStartupMessages(
    library(package, character.only = TRUE)
  )
}))

# =============================================================================
# 5. Charger les fonctions du projet
# =============================================================================

# --- Base de donnees ----------------------------------------------------------
# Definir la connexion et les exports ; la connexion est ouverte lors d'un appel.
source_project_file("db/stats_db.R")
source_project_file("db/upload.R")

# --- Utilitaires communs ------------------------------------------------------
# Chargement FIGARO, conversions monetaires, imputation et pays similaires.
source_project_file("utils/utils_figaro_data.R")
source_project_file("utils/utils_monetary_conversion.R")
source_project_file("utils/utils_imputations.R")
source_project_file("utils/utils_outliers.R")
source_project_file("utils/utils_proxy_by_similarity.R")

# --- Comptes tendanciels ------------------------------------------------------
# Charger les modeles de prevision avant le builder qui les utilise.
source_project_file("trd_accounts/utils_montecarlo_forecasts.R")
source_project_file("trd_accounts/utils_regression_forecasts.R")
source_project_file("trd_accounts/trend_accounts_builder.R")

# --- Comptes observes et controles de disponibilite des sources ----------------
# Pour chaque indicateur : builder, puis fonctions de controle des sources.
source_project_file("obs_accounts/art/art_accounts_builder.R")
source_project_file("obs_accounts/art/art_accounts_output_data.R")
source_project_file("obs_accounts/eco/eco_accounts_builder.R")
source_project_file("obs_accounts/eco/eco_accounts_output_data.R")
source_project_file("obs_accounts/geq/geq_accounts_builder.R")
source_project_file("obs_accounts/geq/geq_accounts_output_data.R")
source_project_file("obs_accounts/ghg/ghg_accounts_builder.R")
source_project_file("obs_accounts/ghg/ghg_accounts_output_data.R")
source_project_file("obs_accounts/haz/haz_accounts_builder.R")
source_project_file("obs_accounts/haz/haz_accounts_output_data.R")
source_project_file("obs_accounts/idr/idr_accounts_builder.R")
source_project_file("obs_accounts/idr/idr_accounts_output_data.R")
source_project_file("obs_accounts/knw/knw_accounts_builder.R")
source_project_file("obs_accounts/knw/knw_accounts_output_data.R")
source_project_file("obs_accounts/mat/mat_accounts_builder.R")
source_project_file("obs_accounts/mat/mat_accounts_output_data.R")
source_project_file("obs_accounts/nrg/nrg_accounts_builder.R")
source_project_file("obs_accounts/nrg/nrg_accounts_output_data.R")
source_project_file("obs_accounts/soc/soc_accounts_builder.R")
source_project_file("obs_accounts/soc/soc_accounts_output_data.R")
source_project_file("obs_accounts/was/was_accounts_builder.R")
source_project_file("obs_accounts/was/was_accounts_output_data.R")
source_project_file("obs_accounts/wat/wat_accounts_builder.R")
source_project_file("obs_accounts/wat/wat_accounts_output_data.R")

# --- Comptes cibles -----------------------------------------------------------
# Charger les trajectoires des indicateurs disposant d'un objectif explicite.
source_project_file("tgt_accounts/geq/geq_targets_builder.R")
source_project_file("tgt_accounts/ghg/ghg_targets_builder.R")
source_project_file("tgt_accounts/knw/knw_targets_builder.R")
source_project_file("tgt_accounts/mat/mat_targets_builder.R")
source_project_file("tgt_accounts/nrg/nrg_targets_builder.R")
source_project_file("tgt_accounts/soc/soc_targets_builder.R")
source_project_file("tgt_accounts/was/was_targets_builder.R")
source_project_file("tgt_accounts/wat/wat_targets_builder.R")

# --- Empreintes macroeconomiques ----------------------------------------------
# La desagregation et les modeles EEIO sont charges par leur workflow dedie.
source_project_file("footprints/footprints_builder.R")
# source_project_file("disaggregation/disaggregation.R")

# --- Orchestration ------------------------------------------------------------
# Charger les workflows en dernier, une fois leurs fonctions de calcul disponibles.
source_project_file("scripts/workflows.R")

# Le setup est termine : les fonctions sont disponibles dans la session R.
message("Setup complete. Functions are loaded in the current R session.")
