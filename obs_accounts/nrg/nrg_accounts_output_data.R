# La Societe Nouvelle

check_nrg_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
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

  ensure_file <- function(file_path, download_expr) {
    if (!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE)

    if (!file.exists(file_path) || !use_temp_data) {
      tryCatch(
        suppressMessages(suppressWarnings(force(download_expr))),
        error = function(e) {
          if (verbose) message("Unable to refresh ", file_path, ": ", conditionMessage(e))
        }
      )
    }

    file.exists(file_path)
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

  eurostat_correspondence_table_geo <- read_delim(
      "obs_accounts/nrg/eurostat_correspondence_table_geo.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, geo)

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/nrg/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  # -------------------------------------------------------------------
  # Eurostat data

  eurostat_file_path <- file.path(download_dir, "env_ac_pefasu.csv")
  ensure_file(
    eurostat_file_path,
    {
      eurostat_raw_data <- get_eurostat(
        "env_ac_pefasu",
        filters = list(time = years, stk_flow = "SUP", unit = "TJ"),
        cache = FALSE
      )
      write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
    }
  )

  eurostat_data <- if (file.exists(eurostat_file_path)) {
    read.csv(eurostat_file_path) %>%
      filter(
        freq == "A",
        stk_flow == "SUP",
        prod_nrg == "R30",
        unit == "TJ",
        !is.na(OBS_VALUE)
      ) %>%
      merge(eurostat_correspondence_table_geo) %>%
      merge(eurostat_correspondence_table_nace_r2) %>%
      mutate(year = as.character(TIME_PERIOD)) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  eurostat_check <- check_country_industry(eurostat_data)
  synthesis <- eurostat_check

  # -------------------------------------------------------------------
  # Output

  section_title("Eurostat env_ac_pefasu")
  print(eurostat_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    eurostat = eurostat_check,
    synthesis = synthesis
  ))
}







