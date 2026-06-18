# La Societe Nouvelle

# Install R dependencies used by the project.
# Run from the repository root with:
# source("scripts/install_dependencies.R")

cran_packages <- c(
  "arrow",
  "countrycode",
  "curl",
  "data.table",
  "DBI",
  "dplyr",
  "eurostat",
  "foreach",
  "doFuture",
  "future",
  "parallel",
  "progressr",
  "purrr",
  "readr",
  "readxl",
  "RPostgres",
  "stringr",
  "tibble",
  "tidyr"
)

optional_packages <- c(
  "fbi"
)

install_missing_cran_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) == 0) {
    message("All CRAN dependencies are already installed.")
    return(invisible(character()))
  }

  message(
    "Installing missing CRAN packages: ",
    paste(missing_packages, collapse = ", ")
  )

  install.packages(missing_packages)

  invisible(missing_packages)
}

check_optional_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) == 0) {
    message("All optional dependencies are available.")
    return(invisible(character()))
  }

  message(
    "Optional packages not installed: ",
    paste(missing_packages, collapse = ", ")
  )

  message(
    "These packages may be required for specific workflows. ",
    "For example, fbi is used by clean_outliers()."
  )

  invisible(missing_packages)
}

install_missing_cran_packages(cran_packages)
check_optional_packages(optional_packages)

message("Dependency installation check complete.")
