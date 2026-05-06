# La Societe Nouvelle

# =============================================================================
# Main pipeline
# =============================================================================
#
# This script runs the main workflow:
#   1. Load setup and configuration
#   2. Build accounts data: observed, trend and target
#   3. Build footprint data from the generated account series

# -----------------------------------------------------------------------------
# 1. Project setup
# -----------------------------------------------------------------------------

# Resolve the project root from this file location, then switch the working
# directory to the repository root so relative paths remain stable.
main_path <- normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(main_path), ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

# Load packages, configuration and project functions.
source(file.path(project_root, "scripts/setup.R"), encoding = "UTF-8")

# Ensure local data directories exist before running builders.
dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figaro_data_dir, showWarnings = FALSE, recursive = TRUE)

# Use the default execution scope defined in config/config.R.
indics <- default_indics
tgt_indics <- default_tgt_indics

# -----------------------------------------------------------------------------
# 2. Accounts data
# -----------------------------------------------------------------------------

# Observed accounts: historical direct impacts by country, industry and year.
if (verbose) message("Building observed accounts")
update_obs_accounts(
  indics = indics,
  do_clean_outliers = do_clean_outliers,
  do_update = do_update,
  verbose = verbose
)

# Trend accounts: forecasts extending observed series.
if (verbose) message("Building trend accounts")
update_trd_accounts(
  indics = indics,
  do_update = do_update,
  verbose = verbose
)

# Target accounts: target trajectories for indicators with explicit targets.
if (verbose) message("Building target accounts")
update_tgt_accounts(
  indics = tgt_indics,
  do_update = do_update,
  verbose = verbose
)

# -----------------------------------------------------------------------------
# 3. Footprint data
# -----------------------------------------------------------------------------

# Build the list of direct-impact series that will be converted into footprints.
if (verbose) message("Building footprint data")
footprint_series <- c(
  paste0(tolower(indics), "_obs"),
  paste0(tolower(indics), "_trd"),
  paste0(tolower(tgt_indics), "_tgt")
)

# Compute and write footprint files in data_output.
footprints_data <- build_footprints(
  series = footprint_series,
  verbose = verbose
)

# -----------------------------------------------------------------------------
# 4. End
# -----------------------------------------------------------------------------

if (verbose) message("Pipeline complete")
