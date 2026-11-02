# Run from the repository root: Rscript --vanilla tests/check_all_accounts_sources.R
# Local CSV/Parquet/BTS fixtures; no network calls or production output writes.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(countrycode)
})

check_all_accounts_sources <- function() {
  root <- tempfile("all-sources-")
  dir.create(root)
  on.exit({
    target <- normalizePath(root, winslash = "/", mustWork = TRUE)
    temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    stopifnot(startsWith(target, paste0(temp_root, "/")))
    unlink(target, recursive = TRUE)
  }, add = TRUE)

  fixture <- new.env(parent = globalenv())
  base::source("config/config.R", local = fixture, encoding = "UTF-8")
  base::source("utils/utils_accounts_sources.R", local = fixture, encoding = "UTF-8")
  indics <- c("art", "eco", "geq", "haz", "idr", "knw", "mat", "soc", "wat")

  for (indic in tolower(fixture$default_indics)) {
    for (suffix in c("sources", "builder", "output_data")) {
      base::source(paste0("obs_accounts/", indic, "/", indic, "_accounts_", suffix, ".R"),
                   local = fixture, encoding = "UTF-8-BOM")
    }
  }

  fixture$source <- function(...) invisible(NULL)
  fixture$download_dir <- file.path(root, "raw")
  fixture$figaro_data_dir <- file.path(root, "figaro")
  fixture$output_dir <- file.path(root, "output")
  fixture$open_dataset <- arrow::open_dataset
  fixture$read_parquet <- arrow::read_parquet
  fixture$get_ilostat <- fixture$get_eurostat <- fixture$read_sdmx <-
    fixture$curl_download <- fixture$download_figaro_file <- function(...) stop("Unexpected network call")
  fixture$complete_series <- fixture$proxy_missing_value_by_similarity <- function(data, ...) data
  fixture$from_usd_to_euro <- function(...) 1
  fixture$read_delim <- function(file, ...) {
    data <- readr::read_delim(file, ...)
    if (basename(file) == "metadata_figaro_industries.csv") data <- filter(data, code %in% c("A01", "B"))
    if (basename(file) == "metadata_figaro_countries.csv") data <- filter(data, code %in% c("FR", "DE"))
    data
  }

  # Each control must handle absent caches locally, including partial BTS years.
  for (indic in tolower(fixture$default_indics)) {
    check <- get(paste0("check_", indic, "_obs_accounts_output_data"), fixture)
    invisible(capture.output(result <- check(years = c(2022, 2023))))
    stopifnot(all(result$synthesis$status == "NOK"), !dir.exists(fixture$download_dir),
              !dir.exists(fixture$figaro_data_dir))
  }

  dir.create(fixture$download_dir)
  dir.create(fixture$figaro_data_dir)
  put <- function(data, name) utils::write.csv(data, file.path(fixture$download_dir, name), row.names = FALSE)
  panel <- crossing(country = c("FR", "DE"), industry = c("A01", "B"))
  aggregates <- panel %>% crossing(aggregate = c("NVA", "PRD")) %>%
    mutate(year = "2022", value = if_else(aggregate == "NVA", 100, 200))
  arrow::write_parquet(aggregates, file.path(fixture$figaro_data_dir, "figaro_main_aggregates_2022.parquet"))
  fixture$load_local_figaro_main_aggregates <- function(year) {
    aggregates %>% mutate(year = as.character(year), id = paste0(country, "_", industry))
  }
  fixture$load_local_figaro_intermediate_inputs <- function(year) {
    crossing(use_country = c("FR", "DE"), use_industry = c("A01", "B"), resource_country = c("FR", "DE")) %>%
      mutate(year = as.character(year), resource_industry = "C20", value = 1)
  }

  # ART: two active establishments per APE, one registered as a craft activity.
  stock <- tibble(
    nomenclatureActivitePrincipaleEtablissement = "NAFRev2",
    activitePrincipaleEtablissement = rep(c("01.11Z", "05.10Z"), each = 2),
    activitePrincipaleRegistreMetiersEtablissement = rep(c("craft", ""), 2),
    etatAdministratifEtablissement = "A"
  )
  history <- stock %>% mutate(dateDebut = as.Date("2020-01-01"), dateFin = as.Date(NA))
  arrow::write_parquet(stock, file.path(fixture$download_dir, "StockEtablissements.parquet"))
  arrow::write_parquet(history, file.path(fixture$download_dir, "StockEtablissementsHistorique.parquet"))

  # GEQ and IDR: complete sex / decile data for two countries and sectors.
  geq <- crossing(ref_area = c("FRA", "DEU"), classif1 = c("ECO_ISIC4_A", "ECO_ISIC4_B"),
                  sex = c("SEX_M", "SEX_F", "SEX_T")) %>%
    mutate(classif2 = "CUR_TYPE_PPP", time = 2022, obs_value = case_when(sex == "SEX_M" ~ 10, sex == "SEX_F" ~ 8, TRUE ~ 9))
  put(geq, "EAR_EHRA_SEX_ECO_CUR_NB_A.csv")
  idr <- crossing(ref_area = c("FRA", "DEU"), classif1 = c("DCL_DECILE_01", "DCL_DECILE_02", "DCL_DECILE_09", "DCL_DECILE_10")) %>%
    mutate(time = 2022, obs_value = rep(c(1, 2, 9, 10), 2))
  put(idr, "LAP_2LID_QTL_RT_A.csv")
  industries <- fixture$read_delim("metadata/metadata_figaro_industries.csv", delim = ";", show_col_types = FALSE)
  bts <- crossing(A38 = unique(industries$branch), TRNNETO = 1:3) %>% mutate(NBHEUR_TOT = 100)
  reference <- readr::read_delim("obs_accounts/idr/bts_files_ids.csv", delim = ";", show_col_types = FALSE) %>% filter(year == 2022)
  utils::write.table(bts, file.path(fixture$download_dir, reference$file_name), sep = ";", row.names = FALSE)

  # MAT and HAZ: retain mixed PRODCOM quantity / unit observations in the cache.
  put(tibble(Country = rep(c("France", "Germany"), each = 2), Flow.code = "DE", Flow.unit = "t",
              Category = rep(c("Biomass", "Fossil fuels"), 2), Description = "fixture", X2022 = c(10, 20, 30, 40)), "mfa4.csv")
  put(crossing(reporter = c("FR", "DE"), indicators = c("PRODQNT", "QNTUNIT")) %>%
        mutate(freq = "A", TIME_PERIOD = 2022, product = "20132434", OBS_VALUE = if_else(indicators == "QNTUNIT", "KG", "4000")), "ds-059358.csv")

  # SOC: cached column names match the public URSSAF export.
  soc <- data.frame(champ = "OUI", famille = "ESS", secteur = "sector", na88 = c("nca sector", "05-39 sector"),
                     annee = 2022, etablissements = 1, effectifs = 1, masse = c(0, 100))
  names(soc) <- c("X.Champ.ESS..", "Famille.ESS", "Grand.secteur.d.activité", "Secteur.NA88", "Année",
                  "Nombre.d.établissements", "Effectifs.salariés.moyens", "Masse.salariale..brute.")
  put(soc, "nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88.csv")

  # WAT and KNW: source activities map onto both FIGARO branches.
  activities <- crossing(REF_AREA = c("FRA", "DEU"), ACTIVITY = c("A", "B"))
  put(activities %>% mutate(VARIABLE = "SEC", SUPPLY_USE = "USE", MEASURE = "SURFACE_GROUND", FREQ = "A",
                             UNIT_MEASURE = "M3", UNIT_MULT = 6, TIME_PERIOD = 2022, OBS_VALUE = 2), "DSD_WATER_PSUT.csv")
  put(activities %>% mutate(CRITERIA = "MA", FREQ = "A", MEASURE = "B", PRICE_BASE = "V", TIME_PERIOD = 2022,
                             UNIT_MEASURE = "USD_PPP", ObsValue = 1e6), "DSD_ANBERD.csv")
  put(crossing(geo = c("FR", "DE"), nace_r2 = c("TOTAL", "B-E")) %>%
        mutate(freq = "A", cost = "TOTAL", unit = "PC", TIME_PERIOD = 2022, OBS_VALUE = 10), "trng_cvt_16n2.csv")
  put(crossing(REF_AREA = c("FRA", "DEU"), ACTIVITY = c("A01", "B"), MEASURE = c("D11", "B1G")) %>%
        mutate(ACTION = "I", FREQ = "A", PRICE_BASE = "V", UNIT_MEASURE = "XDC", UNIT_MULT = 6,
               TIME_PERIOD = 2022, OBS_VALUE = if_else(MEASURE == "D11", 50, 100)), "DSD_STAN.csv")

  hashes <- function() tools::md5sum(list.files(root, full.names = TRUE, recursive = TRUE))
  before <- hashes()

  # Fixed reference outputs for the panel, ordered by country then industry.
  # These were also compared with the builders before source extraction.
  expected_values <- list(
    art = c(0, 0, 50, 50),
    eco = c(0, 0, 100, 100),
    geq = rep(22.2, 4),
    haz = rep(2, 4),
    idr = rep(2.67, 4),
    knw = rep(6, 4),
    mat = c(30, 40, 10, 20),
    soc = c(0, 0, 0, 100),
    wat = c(1800, 160, 1800, 160)
  )
  for (indic in indics) {
    name <- paste0("build_", indic, "_obs_accounts")
    invisible(capture.output(actual <- suppressMessages(get(name, fixture)(years = 2022, write_output = FALSE))))
    actual <- arrange(actual, country, industry)
    stopifnot(nrow(actual) == 4L, isTRUE(all.equal(actual$value, expected_values[[indic]])), all(is.finite(actual$value)))

    check <- get(paste0("check_", indic, "_obs_accounts_output_data"), fixture)
    invisible(capture.output(report <- check(years = 2022)))
    stopifnot(report$synthesis$status == "OK", length(report$source_errors) == 0L)
    message(toupper(indic), ": source, control and production equivalence OK")
  }
  stopifnot(identical(before, hashes()), !dir.exists(fixture$output_dir))

  # Binary refresh: invalid downloads preserve the existing Parquet cache.
  fixture$curl_download <- function(url, destfile, ...) writeLines("invalid parquet", destfile)
  failure <- tryCatch(fixture$load_sirene_art_stock_source(2022, use_temp_data = FALSE), error = identity)
  stopifnot(inherits(failure, "error"), identical(before, hashes()))
  fixture$curl_download <- function(url, destfile, ...) arrow::write_parquet(stock, destfile)
  refreshed <- fixture$load_sirene_art_stock_source(2022, use_temp_data = FALSE)
  stopifnot(nrow(refreshed) == 2L, all(refreshed$n_ets == 2L))

  # FIGARO can be refreshed after a prior read in the same Windows session.
  fixture$download_figaro_file <- function(filename, verbose, data_dir, destfile) {
    arrow::write_parquet(aggregates, destfile)
  }
  renewed_figaro <- fixture$load_figaro_eco_source(2022, use_temp_data = FALSE)
  stopifnot(nrow(renewed_figaro) == 8L)

  # Exercise an actual ZIP member when the optional test archive writer is present.
  if (requireNamespace("zip", quietly = TRUE)) {
    archive <- file.path(root, "bts.zip")
    zip::zipr(archive, files = file.path(fixture$download_dir, reference$file_name))
    fixture$curl_download <- function(url, destfile, ...) file.copy(archive, destfile, overwrite = TRUE)
    previous_bts <- fixture$load_bts_idr_source(2022)
    renewed_bts <- fixture$load_bts_idr_source(2022, use_temp_data = FALSE)
    stopifnot(isTRUE(all.equal(previous_bts, renewed_bts)))
  }
  stopifnot(length(list.files(fixture$download_dir, pattern = "^[.]bts-|^[.]source-", all.files = TRUE)) == 0L)

  message("All-indicator source checks passed: caches, controls and unchanged production calculations.")
}

check_all_accounts_sources()
