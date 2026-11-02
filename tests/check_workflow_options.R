# Run from the repository root: Rscript --vanilla tests/check_workflow_options.R
# Small local fixtures; no downloads, FIGARO production files or database calls.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
})

check_workflow_options <- function() {
  root <- tempfile("ct1-")
  dir.create(root)
  on.exit({
    # Only remove this test's verified temporary directory.
    target <- normalizePath(root, winslash = "/", mustWork = TRUE)
    temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    stopifnot(startsWith(target, paste0(temp_root, "/")))
    unlink(target, recursive = TRUE)
  }, add = TRUE)

  fixture <- new.env(parent = globalenv())
  base::source("config/config.R", local = fixture, encoding = "UTF-8-BOM")
  files <- c(
    list.files("obs_accounts", "_accounts_builder[.]R$", recursive = TRUE, full.names = TRUE),
    list.files("tgt_accounts", "_targets_builder[.]R$", recursive = TRUE, full.names = TRUE),
    "trd_accounts/trend_accounts_builder.R", "scripts/workflows.R"
  )
  for (file in files) base::source(file, local = fixture, encoding = "UTF-8-BOM")

  # Check public named calls against all 20 account builder signatures.
  for (indic in fixture$default_indics) {
    builder <- get(paste0("build_", tolower(indic), "_obs_accounts"), fixture)
    match.call(builder, quote(builder(use_temp_data = TRUE, write_output = FALSE, verbose = FALSE)))
  }
  for (indic in fixture$default_tgt_indics) {
    builder <- get(paste0("build_", tolower(indic), "_tgt_accounts"), fixture)
    match.call(builder, quote(builder(write_output = FALSE, verbose = FALSE)))
  }

  # Keep actual builders, forecasts and metadata, with one country and industry.
  # Their internal source() calls must not replace the fixture data loaders.
  fixture$source <- function(...) invisible(NULL)
  fixture$read_delim <- function(file, ...) {
    data <- readr::read_delim(file, ...)
    if (basename(file) == "metadata_figaro_countries.csv") data <- filter(data, code == "FR")
    if (basename(file) == "metadata_figaro_industries.csv") data <- slice(filter(data, code != "TOTAL"), 1)
    data
  }
  industry <- fixture$read_delim(
    "metadata/metadata_figaro_industries.csv", delim = ";", show_col_types = FALSE
  )$code
  fixture$load_local_figaro_main_aggregates <- function(year) {
    tibble(year = as.character(year), country = "FR", industry = industry,
           aggregate = c("NVA", "PRD"), value = c(100, 200))
  }
  fixture$load_figaro_eco_source <- function(years, ...) {
    map_dfr(as.character(years), fixture$load_local_figaro_main_aggregates)
  }
  writes <- character()
  fixture$write.csv <- function(x, file, ...) {
    writes <<- c(writes, basename(file))
    utils::write.csv(x, file, ...)
  }
  fixture$upload_accounts_data <- fixture$upload_footprints_data <- function(...) {
    stop("Unexpected PostgreSQL export")
  }

  # Observed accounts create a missing output directory only when writing.
  fixture$output_dir <- file.path(root, "accounts")
  obs <- fixture$build_eco_obs_accounts(years = 2024, write_output = FALSE)
  stopifnot(nrow(obs) == 1L, obs$value == 100, !dir.exists(fixture$output_dir))
  fixture$update_obs_accounts(indics = "ECO", write_output = TRUE)
  stopifnot(sum(writes == "accounts_obs_eco.csv") == 1L)
  saved <- readLines(file.path(fixture$output_dir, "accounts_obs_eco.csv"))
  fixture$update_obs_accounts(indics = "ECO", write_output = FALSE, use_temp_data = FALSE)
  stopifnot(identical(saved, readLines(file.path(fixture$output_dir, "accounts_obs_eco.csv"))))

  # A constant GEQ history gives known forecasts and feeds a real target builder.
  obs_geq <- mutate(obs, serie_id = "geq_obs", value = 10)
  utils::write.csv(obs_geq, file.path(fixture$output_dir, "accounts_obs_geq.csv"), row.names = FALSE)
  cleaning_calls <- 0L
  fixture$clean_outliers <- function(data, verbose = FALSE) {
    cleaning_calls <<- cleaning_calls + 1L
    mutate(data, value = value + 1)
  }
  trd <- fixture$build_trd_accounts("GEQ", years = 2024:2030,
                                  write_output = FALSE, do_clean_outliers = FALSE)
  stopifnot(cleaning_calls == 0L, nrow(trd) == 6L, all(trd$value == 10),
            !file.exists(file.path(fixture$output_dir, "accounts_trd_geq.csv")))
  trd_clean <- fixture$build_trd_accounts("GEQ", years = 2024:2030,
                                        write_output = FALSE, do_clean_outliers = TRUE)
  stopifnot(cleaning_calls == 1L, all(trd_clean$value == 11))
  fixture$update_trd_accounts(indics = "GEQ", write_output = TRUE, do_clean_outliers = FALSE)
  stopifnot(cleaning_calls == 1L, sum(writes == "accounts_trd_geq.csv") == 1L)
  fixture$update_trd_accounts(indics = "GEQ", write_output = FALSE, do_clean_outliers = FALSE)
  stopifnot(sum(writes == "accounts_trd_geq.csv") == 1L)

  tgt <- suppressMessages(fixture$build_geq_tgt_accounts(write_output = FALSE))
  stopifnot(nrow(tgt) == 6L, all(is.finite(tgt$value)),
            !file.exists(file.path(fixture$output_dir, "accounts_tgt_geq.csv")))
  suppressMessages(fixture$update_tgt_accounts(indics = "GEQ", write_output = TRUE, verbose = FALSE))
  suppressMessages(fixture$update_tgt_accounts(indics = "GEQ", write_output = FALSE, verbose = FALSE))
  stopifnot(sum(writes == "accounts_tgt_geq.csv") == 1L)

  # Footprint calculations are stubbed at the expensive matrix boundary.
  footprint_calls <- character()
  fixture$build_footprints <- function(serie_id, verbose = FALSE) {
    footprint_calls <<- c(footprint_calls, serie_id)
    tibble(serie_id = serie_id, country = "FR", industry = industry,
           year = "2030", aggregate = "PRD", value = 1, flag = "")
  }
  fixture$output_dir <- file.path(root, "footprints")
  fixture$update_footprints(indics = "GEQ", write_output = FALSE, verbose = FALSE)
  stopifnot(!dir.exists(fixture$output_dir), length(footprint_calls) == 3L)
  fixture$update_footprints(indics = "GEQ", write_output = TRUE, verbose = FALSE)
  footprint_files <- c("footprints_obs_geq.csv", "footprints_trd_geq.csv", "footprints_tgt_geq.csv")
  stopifnot(all(file.exists(file.path(fixture$output_dir, footprint_files))),
            all(table(writes[writes %in% footprint_files]) == 1L))

  # The final disaggregation must receive both values of write_output.
  fixture$build_canada_eeio_footprints <- fixture$build_dk_eeio_footprints <-
    fixture$build_uk_eeio_footprints <- fixture$build_us_eeio_footprints <-
    function(year_i, verbose) stopifnot(year_i == 2022, identical(verbose, FALSE))
  disaggregation_options <- logical()
  fixture$build_disaggregated_footprints <- function(YEAR, use_temp_data, write_output) {
    stopifnot(YEAR == 2022, identical(use_temp_data, FALSE))
    disaggregation_options <<- c(disaggregation_options, write_output)
    tibble(value = 1)
  }
  for (write_output in c(FALSE, TRUE)) {
    result <- fixture$update_disaggregated_footprints(
      year_i = 2022, write_output = write_output, verbose = FALSE
    )
    stopifnot(result$value == 1)
  }
  stopifnot(identical(disaggregation_options, c(FALSE, TRUE)))

  # Run the documented main entry point with real workflows and small builders.
  fixture$output_dir <- file.path(root, "pipeline")
  fixture$download_dir <- file.path(root, "raw")
  fixture$figaro_data_dir <- file.path(root, "figaro")
  fixture$default_indics <- "ECO"
  fixture$default_tgt_indics <- character()
  fixture$write_output <- TRUE
  fixture$do_clean_outliers <- FALSE
  fixture$verbose <- FALSE
  writes <- character()
  base::source("scripts/main.R", local = fixture, encoding = "UTF-8")
  stopifnot(setequal(writes, c("accounts_obs_eco.csv", "accounts_trd_eco.csv",
                              "footprints_obs_eco.csv", "footprints_trd_eco.csv")),
            length(writes) == 4L, cleaning_calls == 1L)
  message("CT1 checks passed: launch, CSV options, cleaning and disaggregation.")
}

check_workflow_options()
