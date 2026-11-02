# La Societe Nouvelle

check_nrg_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {
  if (verbose) message("Check output data for NRG accounts (observed data)")
  # -------------------------------------------------------------------
  # Utils

  format_percent <- function(x) round(100 * x, digits = 1)

  section_title <- function(title) {
    cat("\n", strrep("-", 80), "\n", title, "\n", strrep("-", 80), "\n", sep = "")
  }
  empty_check <- function(years) {
    tibble(
      year = as.character(years),
      status = "NOK",
      available_data_fr = 0,
      available_data_row = 0,
      available_data_countries = 0
    )
  }

  check_country_industry <- function(data) {
    n_industries <- nrow(figaro_industries)
    n_countries <- nrow(figaro_countries)
    n_row_countries <- n_countries - 1

    map_dfr(years, function(y) {
      year_data <- data %>%
        filter(year == y) %>%
        filter(
          country %in% figaro_countries$country,
          industry %in% figaro_industries$industry
        ) %>%
        distinct(country, industry)

      if (nrow(year_data) == 0) return(empty_check(y))

      tibble(
        year = y,
        status = "OK",
        available_data_fr = format_percent(
          nrow(year_data %>% filter(country == "FR") %>% distinct(industry)) / n_industries
        ),
        available_data_row = format_percent(
          nrow(year_data %>% filter(country != "FR")) / (n_row_countries * n_industries)
        ),
        available_data_countries = format_percent(
          nrow(year_data %>% distinct(country)) / n_countries
        )
      )
    })
  }

  years <- as.character(years)
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/nrg/nrg_accounts_sources.R", encoding = "UTF-8-BOM")
  # use_temp_data = FALSE remains an explicit refresh request for existing callers.
  refresh_sources <- isTRUE(refresh_sources) || !isTRUE(use_temp_data)
  load_source <- function(loader) {
    load_accounts_source_for_check(loader, years, refresh_sources, verbose)
  }

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(industry = code) %>%
    select(industry)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  # -------------------------------------------------------------------
  # Coverage of the exact Eurostat source used by the builder, before imputation.

  eurostat_data <- load_source(load_eurostat_nrg_source)
  eurostat_check <- check_country_industry(eurostat_data)
  section_title("Eurostat env_ac_pefasu")
  print(eurostat_check)
  invisible(list(
    eurostat = eurostat_check,
    synthesis = eurostat_check,
    source_errors = list(eurostat = attr(eurostat_data, "source_error"))
  ))
}
