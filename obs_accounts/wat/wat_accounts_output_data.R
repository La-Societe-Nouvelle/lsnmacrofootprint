# La Societe Nouvelle

check_wat_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for WAT accounts (observed data)")
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

  table_passage_ocde_data <- read_delim(
      "obs_accounts/wat/table_passage_ocde.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_industry)

  # -------------------------------------------------------------------
  # OECD data

  oecd_file_path <- file.path(download_dir, "DSD_WATER_PSUT.csv")
  ensure_file(
    oecd_file_path,
    {
      base_url_oecd_data <- "https://sdmx.oecd.org/public/rest/data/OECD.ENV.EPI,DSD_WATER_PSUT@DF_WATER_PSUT,1.0/.A.SURFACE_GROUND..USE.SEC.?"
      url_oecd_data <- paste0(
        base_url_oecd_data,
        "startPeriod=", min(years),
        "&dimensionAtObservation=AllDimensions",
        "&format=csvfilewithlabels"
      )
      write.csv(read.csv(url_oecd_data), oecd_file_path, row.names = FALSE)
    }
  )

  oecd_data <- if (file.exists(oecd_file_path)) {
    read.csv(oecd_file_path) %>%
      filter(
        VARIABLE == "SEC",
        SUPPLY_USE == "USE",
        MEASURE == "SURFACE_GROUND",
        FREQ == "A",
        UNIT_MEASURE == "M3",
        UNIT_MULT == 6,
        !is.na(OBS_VALUE)
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
        oecd_industry = ACTIVITY
      ) %>%
      merge(table_passage_ocde_data) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  oecd_check <- check_country_industry(oecd_data)
  synthesis <- oecd_check

  # -------------------------------------------------------------------
  # Output

  section_title("OECD WATER_PSUT")
  print(oecd_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    oecd = oecd_check,
    synthesis = synthesis
  ))
}







