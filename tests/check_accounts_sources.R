# Run from the repository root: Rscript --vanilla tests/check_accounts_sources.R
# Real loaders/builders/controls on local fixtures; remote calls are simulated.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(countrycode)
})

check_accounts_sources <- function() {
  root <- tempfile("ct4-")
  dir.create(root)
  on.exit({
    target <- normalizePath(root, winslash = "/", mustWork = TRUE)
    temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    stopifnot(startsWith(target, paste0(temp_root, "/")))
    unlink(target, recursive = TRUE)
  }, add = TRUE)
  fixture <- new.env(parent = globalenv())
  files <- c("utils/utils_accounts_sources.R", unlist(lapply(c("nrg", "was", "ghg"), function(indic) {
    paste0("obs_accounts/", indic, "/", indic,
           c("_accounts_sources.R", "_accounts_builder.R", "_accounts_output_data.R"))
  })))
  for (file in files) base::source(file, local = fixture, encoding = "UTF-8-BOM")
  fixture$source <- function(...) invisible(NULL)
  fixture$download_dir <- root
  fixture$output_dir <- file.path(root, "output")
  fixture$read_delim <- function(file, ...) {
    data <- readr::read_delim(file, ...)
    if (basename(file) == "metadata_figaro_industries.csv") data <- filter(data, code %in% c("A01", "B"))
    if (basename(file) == "metadata_figaro_countries.csv") data <- filter(data, code %in% c("FR", "DE"))
    data
  }
  fixture$load_local_figaro_main_aggregates <- function(year) {
    crossing(country = c("FR", "DE"), industry = c("A01", "B")) %>%
      mutate(year = as.character(year), aggregate = "NVA", value = 100)
  }
  fixture$complete_series <- function(data, ...) data
  fixture$proxy_missing_value_by_similarity <- function(data, ...) data

  nrg <- tibble(freq = "A", stk_flow = "SUP", prod_nrg = "R30", unit = "TJ",
                geo = c("FR", "FR", "DE", "DE"), nace_r2 = c("A01", "B", "A01", "B"),
                TIME_PERIOD = 2024, OBS_VALUE = 1:4)
  was <- tibble(freq = "A", hazard = "HAZ_NHAZ", waste = "PRIM", unit = "T",
                geo = c("FR", "FR", "DE", "DE"), nace_r2 = c("A", "B", "A", "B"),
                TIME_PERIOD = 2024, OBS_VALUE = c(100, NA, 300, 400))
  oecd_was <- tibble(MEASURE = "TOTAL", ACTION = "I", FREQ = "A", UNIT_MEASURE = "TRUE", UNIT_MULT = 3,
                     REF_AREA = c("FRA", "FRA", "DEU", "DEU"), ACTIVITY = c("A01", "B", "A01", "B"),
                     TIME_PERIOD = 2024, OBS_VALUE = c(10, 20, 30, 40))
  ghg <- tibble(freq = "A", unit = "THS_T", na_item = "TOTAL", c_dest = "WORLD",
                c_orig = c("FR", "DE"), nace_r2 = "A01", TIME_PERIOD = 2024, OBS_VALUE = c(2, 0))
  estimated <- tibble(year = 2024, country = c("FR", "FR", "DE", "DE"),
                      industry = c("A01", "B", "A01", "B"), value = c(9, 4, 8, NA), unit = "THS_TCO2E")
  oecd_ghg <- tibble(MEASURE = "EMISSIONS", POLLUTANT = "GHG", METHODOLOGY = "EMISSIONS_SEEA",
                     ACTION = "I", FREQ = "A", UNIT_MEASURE = "T_CO2E", SOURCE = "REPORTED",
                     ACTIVITY_SCOPE = "RES", ADJUSTMENT = "N", REF_AREA = c("FRA", "FRA", "DEU", "DEU"),
                     ACTIVITY = c("A01", "B", "A01", "B"), TIME_PERIOD = 2024, OBS_VALUE = c(100, 200, 300, 400))
  legacy <- function(data) rename(data, time = TIME_PERIOD, values = OBS_VALUE)
  put <- function(data, name) utils::write.csv(data, file.path(root, name), row.names = FALSE)
  # Reproduce caches previously written by get_eurostat().
  put(legacy(nrg), "env_ac_pefasu.csv")
  put(legacy(was), "env_wasgen.csv")
  put(legacy(ghg), "env_ac_ghgfp.csv")
  put(oecd_was, "DSD_WSECTOR.csv")
  put(oecd_ghg, "oecd_aea_2010_2024.csv")
  put(estimated, "estimated_ghg_accounts.csv")
  put(tibble(category = character(), codeIndicator = character(), codeEproduct = character(),
             timePeriod = integer(), unit = character(), refArea = character(), colPi = character(),
             obsValue = double()), "FIGARO-e_ENE_TJ_2015.csv")

  requests <- character()
  refresh_enabled <- FALSE
  invalid_download <- FALSE
  fixture$read.csv <- function(file, ...) {
    if (!startsWith(file, "https://")) return(utils::read.csv(file, ...))
    requests <<- c(requests, file)
    if (!refresh_enabled) stop("Unexpected network request")
    if (invalid_download) return(data.frame(broken = TRUE))
    if (grepl("env_ac_pefasu", file)) return(nrg)
    if (grepl("env_wasgen", file)) return(was)
    if (grepl("DSD_WSECTOR", file)) return(oecd_was)
    if (grepl("DSD_AEA", file)) return(oecd_ghg)
    stop("Unexpected remote source: ", file)
  }
  fixture$get_eurostat <- function(...) {
    requests <<- c(requests, "get_eurostat")
    if (!refresh_enabled) stop("Unexpected network request")
    legacy(ghg)
  }
  fixture$estimate_ghg_from_edgar <- function(...) {
    requests <<- c(requests, "EDGAR")
    if (!refresh_enabled) stop("Unexpected estimation")
    estimated
  }
  run_check <- function(indic, ...) {
    check <- get(paste0("check_", indic, "_obs_accounts_output_data"), fixture)
    invisible(capture.output(result <- check(years = 2024, ...)))
    result
  }
  hashes <- function() tools::md5sum(list.files(root, full.names = TRUE))
  before <- hashes()
  nrg_check <- run_check("nrg")
  was_check <- run_check("was")
  ghg_check <- run_check("ghg")
  stopifnot(length(requests) == 0L, identical(before, hashes()),
            nrg_check$synthesis$available_data_fr == 100,
            was_check$eurostat$available_data_fr == 50,
            was_check$synthesis$available_data_fr == 100,
            ghg_check$synthesis$available_data_fr == 100,
            ghg_check$synthesis$available_data_row == 100)

  # All three producers consume the same caches as their controls.
  nrg_accounts <- fixture$build_nrg_obs_accounts(years = 2024, write_output = FALSE)
  was_accounts <- fixture$build_was_obs_accounts(years = 2024, write_output = FALSE)
  ghg_accounts <- fixture$build_ghg_obs_accounts(years = 2024, write_output = FALSE)
  value_at <- function(data, country_i, industry_i) filter(data, country == country_i, industry == industry_i)$value
  stopifnot(value_at(nrg_accounts, "FR", "A01") == 1000,
            value_at(was_accounts, "FR", "A01") == 100,
            value_at(was_accounts, "FR", "B") == 20000,
            value_at(ghg_accounts, "FR", "A01") == 2000,
            value_at(ghg_accounts, "FR", "B") == 4000,
            value_at(ghg_accounts, "DE", "A01") == 0,
            value_at(ghg_accounts, "DE", "B") == 400,
            identical(before, hashes()), length(requests) == 0L)
  selected <- ghg_check$selected_sources
  stopifnot(filter(selected, country == "FR", industry == "B")$source == "estimated",
            filter(selected, country == "DE", industry == "B")$source == "oecd",
            filter(selected, country == "DE", industry == "A01")$source == "eurostat")

  # Missing caches are reported without downloads, writes or EDGAR computation.
  fixture$download_dir <- file.path(root, "absent")
  for (indic in c("nrg", "was", "ghg")) {
    result <- run_check(indic)
    stopifnot(result$synthesis$status == "NOK", length(Filter(Negate(is.null), result$source_errors)) > 0L)
  }
  stopifnot(!dir.exists(fixture$download_dir), length(requests) == 0L)
  fixture$download_dir <- root

  # A bad refresh must preserve the previous valid cache and report the error.
  refresh_enabled <- TRUE
  invalid_download <- TRUE
  result <- run_check("nrg", refresh_sources = TRUE)
  stopifnot(identical(before, hashes()), !is.null(result$source_errors$eurostat), result$synthesis$status == "NOK")
  invalid_download <- FALSE
  run_check("nrg", refresh_sources = TRUE)
  run_check("was", refresh_sources = TRUE)
  run_check("ghg", refresh_sources = TRUE)
  for (file in c("env_ac_pefasu.csv", "env_wasgen.csv", "env_ac_ghgfp.csv")) {
    stopifnot(all(c("TIME_PERIOD", "OBS_VALUE") %in% names(utils::read.csv(file.path(root, file)))))
  }
  stopifnot("EDGAR" %in% requests)
  # The previous explicit refresh API remains supported.
  count <- length(requests)
  run_check("nrg", use_temp_data = FALSE)
  stopifnot(length(requests) == count + 1L)
  message("CT4 checks passed: shared caches, legacy schemas, source precedence and explicit refresh.")
}

check_accounts_sources()
