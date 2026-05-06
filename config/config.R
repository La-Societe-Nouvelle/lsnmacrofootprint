# La Societe Nouvelle

# Local paths.
download_dir <- "data_raw"
output_dir <- "data_output"
figaro_data_dir <- "data_figaro"

# Default pipeline scope.
default_years <- 2010:2030

default_indics <- c(
  "ART", "ECO", "GEQ", "GHG", "HAZ", "IDR",
  "KNW", "MAT", "NRG", "SOC", "WAS", "WAT"
)

default_tgt_indics <- c(
  "GEQ", "GHG", "KNW", "MAT", "NRG", "SOC", "WAS", "WAT"
)

# Default execution options.
install_missing_packages <- FALSE
do_update <- FALSE
do_clean_outliers <- TRUE
verbose <- TRUE
