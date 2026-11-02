# La Societe Nouvelle

check_was_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {
  if (verbose) message("Check output data for WAS accounts (observed data)")
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
  source("obs_accounts/was/was_accounts_sources.R", encoding = "UTF-8-BOM")
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
  # Availability is measured on the shared sources, before NVA allocation.

  eurostat_data <- load_source(function(...) {
    data <- load_eurostat_was_source(...)
    nace <- read_delim("obs_accounts/was/eurostat_correspondence_table_nace_r2.csv",
                       delim = ";", show_col_types = FALSE) %>%
      transmute(industry = figaro_industry, nace_r2)
    # Un secteur source peut couvrir plusieurs branches, pour plusieurs pays.
    data %>% filter(waste_generation >= 0) %>%
      inner_join(nace, by = "nace_r2", relationship = "many-to-many")
  })
  oecd_data <- load_source(function(...) {
    data <- load_oecd_was_source(...)
    activities <- read_delim("obs_accounts/was/table_passage_ocde.csv",
                             delim = ";", show_col_types = FALSE) %>%
      transmute(industry = figaro_industry, oecd_activity)
    data %>% filter(waste_generation >= 0) %>%
      inner_join(activities, by = "oecd_activity", relationship = "many-to-many")
  })
  eurostat_check <- check_country_industry(eurostat_data)
  oecd_check <- check_country_industry(oecd_data)
  synthesis <- check_country_industry(bind_rows(
    select(eurostat_data, year, country, industry), select(oecd_data, year, country, industry)
  ))
  section_title("Eurostat env_wasgen")
  print(eurostat_check)
  section_title("OECD WSECTOR")
  print(oecd_check)
  section_title("Synthese")
  print(synthesis)
  invisible(list(
    eurostat = eurostat_check,
    oecd = oecd_check,
    synthesis = synthesis,
    source_errors = list(eurostat = attr(eurostat_data, "source_error"), oecd = attr(oecd_data, "source_error"))
  ))
}
