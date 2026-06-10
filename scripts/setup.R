# La Societe Nouvelle

# Setup for interactive use from the repository root.
# This file sources the project functions without launching the full pipeline.

setup_path <- normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(setup_path), ".."), winslash = "/", mustWork = TRUE)

source_project_file <- function(path) {
  source(file.path(project_root, path), encoding = "UTF-8")
}

# Local project configuration.
source_project_file("config/config.R")

required_packages <- c(
  "arrow",
  "countrycode",
  "curl",
  "data.table",
  "DBI",
  "doFuture",
  "dplyr",
  "eurostat",
  "foreach",
  "future",
  "parallel",
  "progressr",
  "purrr",
  "readr",
  "readsdmx",
  "readxl",
  "Rilostat",
  "RPostgres",
  "rvest",
  "stringr",
  "tibble",
  "tidyr"
)

runtime_packages <- if (isTRUE(do_clean_outliers)) {
  c("fbi")
} else {
  character()
}

missing_packages <- unique(c(required_packages, runtime_packages))
missing_packages <- missing_packages[
  !vapply(missing_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  if (isTRUE(install_missing_packages)) {
    install.packages(missing_packages)
  } else {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them before running the project, or set install_missing_packages <- TRUE in config/config.R."
    )
  }
}

invisible(lapply(required_packages, function(package) {
  suppressPackageStartupMessages(
    library(package, character.only = TRUE)
  )
}))

# Database helpers.
source_project_file("db/stats_db.R")
source_project_file("db/upload.R")

# Common utilities.
source_project_file("utils/utils_figaro_data.R")
source_project_file("utils/utils_monetary_conversion.R")
source_project_file("utils/utils_outliers.R")
source_project_file("utils/utils_proxy_by_similarity.R")

# Trend accounts helpers.
source_project_file("trd_accounts/utils_montecarlo_forecasts.R")
source_project_file("trd_accounts/utils_regression_forecasts.R")
source_project_file("trd_accounts/trend_accounts_builder.R")

# Observed accounts builders.
source_project_file("obs_accounts/art/art_accounts_builder.R")
source_project_file("obs_accounts/eco/eco_accounts_builder.R")
source_project_file("obs_accounts/geq/geq_accounts_builder.R")
source_project_file("obs_accounts/ghg/ghg_accounts_builder.R")
source_project_file("obs_accounts/haz/haz_accounts_builder.R")
source_project_file("obs_accounts/idr/idr_accounts_builder.R")
source_project_file("obs_accounts/knw/knw_accounts_builder.R")
source_project_file("obs_accounts/mat/mat_accounts_builder.R")
source_project_file("obs_accounts/nrg/nrg_accounts_builder.R")
source_project_file("obs_accounts/soc/soc_accounts_builder.R")
source_project_file("obs_accounts/was/was_accounts_builder.R")
source_project_file("obs_accounts/wat/wat_accounts_builder.R")

# Target accounts builders.
source_project_file("tgt_accounts/geq/geq_targets_builder.R")
source_project_file("tgt_accounts/ghg/ghg_targets_builder.R")
source_project_file("tgt_accounts/knw/knw_targets_builder.R")
source_project_file("tgt_accounts/mat/mat_targets_builder.R")
source_project_file("tgt_accounts/nrg/nrg_targets_builder.R")
source_project_file("tgt_accounts/soc/soc_targets_builder.R")
source_project_file("tgt_accounts/was/was_targets_builder.R")
source_project_file("tgt_accounts/wat/wat_targets_builder.R")

# Footprints and EEIO helpers.
source_project_file("footprints/footprints_builder.R")
# source_project_file("disaggregation/disaggregation.R")

# Pipeline orchestration.
source_project_file("scripts/workflows.R")

message("Setup complete. Functions are loaded in the current R session.")
