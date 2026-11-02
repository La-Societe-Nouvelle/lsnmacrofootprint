# Run from the repository root: Rscript --vanilla tests/check_target_calculations.R
# Exercise actual builders on a small panel, without downloads or output writes.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
})

check_target_calculations <- function() {
  fixture <- new.env(parent = globalenv())
  for (indic in c("mat", "was", "geq", "soc")) {
    base::source(paste0("tgt_accounts/", indic, "/", indic, "_targets_builder.R"),
                 local = fixture, encoding = "UTF-8-BOM")
  }
  base::source("obs_accounts/soc/soc_accounts_builder.R", local = fixture, encoding = "UTF-8-BOM")
  base::source("utils/utils_accounts_sources.R", local = fixture, encoding = "UTF-8-BOM")
  base::source("obs_accounts/soc/soc_accounts_sources.R", local = fixture, encoding = "UTF-8-BOM")
  fixture$source <- function(...) invisible(NULL)
  fixture$output_dir <- "unused-output"
  fixture$download_dir <- "unused-cache"
  fixture$file.exists <- function(...) TRUE
  fixture$write.csv <- function(...) stop("Unexpected write")
  fixture$read_delim <- function(file, ...) {
    switch(basename(file),
      metadata_figaro_industries.csv = tibble(code = "A01", branch = "A"),
      metadata_figaro_countries.csv = tibble(code = "FR"),
      table_passage_urssaf.csv = tibble(figaro_industry = "A01", secteur_na88 = "01"),
      stop("Unexpected metadata: ", file)
    )
  }
  panel <- function(years, values) {
    tibble(year = as.character(years), country = "FR", industry = "A01", value = values, flag = "")
  }
  accounts <- list()
  fixture$read.csv <- function(file, ...) {
    result <- accounts[[basename(file)]]
    if (is.null(result)) stop("Unexpected input: ", file)
    result
  }

  # Check six compounding periods (2024-2030), and a single period (2029-2030).
  for (base_year in c(2024L, 2029L)) {
    fixture$load_local_figaro_main_aggregates <- function(year) {
      nva <- if (year == "2010") 100 else if (year == as.character(base_year)) 200 else 400
      tibble(year = as.character(year), country = "FR", industry = "A01", aggregate = "NVA", value = nva)
    }
    future_years <- seq.int(base_year + 1L, 2030L)
    for (indic in c("mat", "was", "geq", "soc")) {
      history <- switch(indic,
        mat = panel(c(2010, base_year), c(1300, 4000)),
        was = panel(c(2010, base_year), c(1000, 4000)),
        geq = panel(base_year, 10),
        soc = panel(base_year, 20)
      )
      accounts[[paste0("accounts_obs_", indic, ".csv")]] <- history
      accounts[[paste0("accounts_trd_", indic, ".csv")]] <- panel(future_years, 4000)
      builder <- get(paste0("build_", indic, "_tgt_accounts"), fixture)
      result <- suppressMessages(builder(write_output = FALSE))
      stopifnot(nrow(result) == length(future_years), all(is.finite(result$value)))
      endpoint <- result$value[result$year == "2030"]
      expected <- switch(indic,
        # 2010 intensity 13 / 1.3 = 10; base intensity 20 must halve by 2030.
        mat = 4000,
        # 2010 intensity 10 * 0.95 = 9.5; multiply by 2030 NVA (400).
        was = 3800,
        # 2050 targets: 1% gender gap and 100% social contribution.
        geq = round(10 * (1 / 10)^((2030 - base_year) / (2050 - base_year)), 1),
        soc = round(400 * 0.1 * (1 / 0.1)^((2030 - base_year) / (2050 - base_year)), 1)
      )
      stopifnot(isTRUE(all.equal(endpoint, expected)))
      if (indic == "mat") {
        # At 2030 NVA = 400, the first projected year's intensity follows the
        # six-period geometric path, rather than reaching the target early.
        first_expected <- round(20 * 0.5^(1 / (2030 - base_year)) * 400)
        stopifnot(result$value[result$year == as.character(base_year + 1L)] == first_expected)
      }
    }
  }

  # An already achieved waste target must not increase the baseline intensity.
  accounts$accounts_obs_was.csv <- panel(c(2010, 2029), c(1000, 1000))
  was <- suppressMessages(fixture$build_was_tgt_accounts(write_output = FALSE))
  stopifnot(was$value == 2000) # base intensity 5, multiplied by 2030 NVA = 400

  # Non-positive payroll totals produce zero SOC, while a positive total uses
  # the actual ESS share. The original erroneous fallback would yield 6 * NVA.
  urssaf <- data.frame(
    champ = rep("OUI", 3), famille = "ESS", secteur = "A", na88 = "01 Agriculture",
    annee = 2024:2026, etablissements = 1, effectifs = 1, masse = c(-10, 0, 100)
  )
  names(urssaf) <- c("X.Champ.ESS..", "Famille.ESS", "Grand.secteur.d.activité",
                     "Secteur.NA88", "Année", "Nombre.d.établissements",
                     "Effectifs.salariés.moyens", "Masse.salariale..brute.")
  accounts[["nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88.csv"]] <- urssaf
  fixture$load_local_figaro_main_aggregates <- function(year) {
    tibble(year = as.character(year), country = "FR", industry = "A01", aggregate = "NVA", value = 200)
  }
  soc <- suppressMessages(fixture$build_soc_obs_accounts(years = 2024:2026, write_output = FALSE))
  stopifnot(identical(soc$value, c(0, 0, 200)))
  message("CT3 checks passed: target intensities, compounding periods and SOC payroll fallback.")
}

check_target_calculations()
