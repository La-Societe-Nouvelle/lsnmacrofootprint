# La Societe Nouvelle

check_eco_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for ECO accounts (observed data)")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")

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

  ensure_figaro_file <- function(filename) {
    if (!dir.exists(figaro_data_dir)) dir.create(figaro_data_dir, recursive = TRUE)

    file_path <- file.path(figaro_data_dir, filename)

    if (!file.exists(file_path) || !use_temp_data) {
      if (verbose) cat("Downloading ", filename, "...\n", sep = "")
      download_figaro_file(
        filename = filename,
        verbose = verbose,
        data_dir = figaro_data_dir
      )
    }

    file.exists(file_path)
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

  n_industries <- nrow(figaro_industries)
  n_countries <- nrow(figaro_countries)
  n_row_countries <- n_countries - 1

  # -------------------------------------------------------------------
  # FIGARO data - Main aggregates

  if (verbose) cat("Checking FIGARO data - Main aggregates...\n")

  figaro_main_aggregates_check <- map_dfr(years, function(y) {
    filename <- paste0("figaro_main_aggregates_", y, ".parquet")
    file_path <- file.path(figaro_data_dir, filename)

    if (!ensure_figaro_file(filename)) {
      return(empty_check(y))
    }

    main_aggregates_data <- tryCatch(
      read_parquet(file_path) %>%
        filter(
          industry != "TOTAL",
          aggregate == "NVA",
          as.character(year) == y
        ) %>%
        select(country, industry, value),
      error = function(e) NULL
    )

    if (is.null(main_aggregates_data) || nrow(main_aggregates_data) == 0) {
      return(empty_check(y))
    }

    available_fr <- main_aggregates_data %>%
      filter(country == "FR", !is.na(value)) %>%
      distinct(industry) %>%
      inner_join(figaro_industries, by = "industry") %>%
      nrow() %>%
      `/`(n_industries) %>%
      format_percent()

    available_row <- main_aggregates_data %>%
      filter(country != "FR", !is.na(value)) %>%
      distinct(country, industry) %>%
      inner_join(figaro_countries, by = "country") %>%
      inner_join(figaro_industries, by = "industry") %>%
      nrow() %>%
      `/`(n_row_countries * n_industries) %>%
      format_percent()

    available_countries <- main_aggregates_data %>%
      filter(!is.na(value)) %>%
      distinct(country) %>%
      inner_join(figaro_countries, by = "country") %>%
      nrow() %>%
      `/`(n_countries) %>%
      format_percent()

    tibble(
      year = y,
      status = "OK",
      available_data_fr = available_fr,
      available_data_row = available_row,
      available_data_countries = available_countries
    )
  })

  # -------------------------------------------------------------------
  # Synthese

  synthesis <- figaro_main_aggregates_check

  # -------------------------------------------------------------------
  # Output

  section_title("FIGARO main aggregates")
  print(figaro_main_aggregates_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    figaro_main_aggregates = figaro_main_aggregates_check,
    synthesis = synthesis
  ))
}







