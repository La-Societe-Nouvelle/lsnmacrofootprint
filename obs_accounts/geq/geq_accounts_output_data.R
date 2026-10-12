# La Societe Nouvelle

check_geq_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for GEQ accounts (observed data)")
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

  ensure_ilostat_file <- function(file_path) {
    if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

    if (!file.exists(file_path) || !use_temp_data) {
      if (verbose) cat("Downloading ILOSTAT EAR_EHRA_SEX_ECO_CUR_NB_A...\n")

      suppressMessages(
        suppressWarnings(
          capture.output(
            ilostat_raw_data <- get_ilostat(
              "EAR_EHRA_SEX_ECO_CUR_NB_A",
              segment = "indicator",
              quiet = TRUE
            ),
            type = "message"
          )
        )
      )

      write.csv(ilostat_raw_data, file_path, row.names = FALSE)
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
    select(industry, branch, sector)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  n_sectors <- figaro_industries %>%
    distinct(sector) %>%
    nrow()

  n_countries <- nrow(figaro_countries)
  n_row_countries <- n_countries - 1

  # -------------------------------------------------------------------
  # ILOSTAT data

  if (verbose) cat("Checking ILOSTAT data...\n")

  ilostat_path <- file.path(download_dir, "EAR_EHRA_SEX_ECO_CUR_NB_A.csv")
  ilostat_check <- empty_check(years)

  if (ensure_ilostat_file(ilostat_path)) {
    ilostat_raw_data <- read.csv(ilostat_path)

    ilostat_data <- ilostat_raw_data %>%
      filter(
        classif2 == "CUR_TYPE_PPP",
        str_starts(classif1, "ECO_ISIC4_"),
        sex %in% c("SEX_M", "SEX_F", "SEX_T")
      ) %>%
      mutate(
        country = countrycode(ref_area, "iso3c", "iso2c", nomatch = NULL),
        sector = sub("ECO_ISIC4_", "", classif1),
        source_year = as.integer(time),
        sex = case_when(
          sex == "SEX_M" ~ "men",
          sex == "SEX_F" ~ "women",
          sex == "SEX_T" ~ "all",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(
        !is.na(country),
        country %in% figaro_countries$country,
        sector %in% figaro_industries$sector,
        !is.na(obs_value)
      ) %>%
      select(source_year, country, sector, sex)

    ilostat_check <- map_dfr(years, function(y) {
      year_i <- as.integer(y)

      year_data <- ilostat_data %>%
        filter(source_year == year_i) %>%
        select(country, sector, sex)

      if (nrow(year_data) == 0) return(empty_check(y))

      complete_series <- year_data %>%
        distinct(country, sector, sex) %>%
        count(country, sector) %>%
        filter(n == 3)

      available_fr <- complete_series %>%
        filter(country == "FR") %>%
        distinct(sector) %>%
        nrow() %>%
        `/`(n_sectors) %>%
        format_percent()

      available_row <- complete_series %>%
        filter(country != "FR") %>%
        distinct(country, sector) %>%
        nrow() %>%
        `/`(n_row_countries * n_sectors) %>%
        format_percent()

      available_countries <- complete_series %>%
        distinct(country) %>%
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
  }

  # -------------------------------------------------------------------
  # Synthese

  synthesis <- ilostat_check

  # -------------------------------------------------------------------
  # Output

  section_title("ILOSTAT EAR_EHRA_SEX_ECO_CUR_NB_A")
  print(ilostat_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    ilostat = ilostat_check,
    synthesis = synthesis
  ))
}







