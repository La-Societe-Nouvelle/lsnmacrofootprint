# La Societe Nouvelle

check_knw_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for KNW accounts (observed data)")
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

  synthesis_from_sources <- function(...) {
    sources <- list(...)

    map_dfr(years, function(y) {
      rows <- map(sources, ~ filter(.x, year == y))

      tibble(
        year = y,
        status = if_else(all(map_chr(rows, "status") == "OK"), "OK", "NOK"),
        available_data_fr = min(map_dbl(rows, "available_data_fr"), na.rm = TRUE),
        available_data_row = min(map_dbl(rows, "available_data_row"), na.rm = TRUE),
        available_data_countries = min(map_dbl(rows, "available_data_countries"), na.rm = TRUE)
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

  anberd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/anberd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, anberd_activity)

  oecd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/oecd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

  eurostat_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  # -------------------------------------------------------------------
  # ANBERD data (R&D Expenditure)

  anberd_file_path <- file.path(download_dir, "DSD_ANBERD.csv")
  ensure_file(
    anberd_file_path,
    {
      base_url_anberd_data <- "https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_ANBERD@DF_ANBERDi4,/.A.MA..USD_PPP.V.?"
      url_anberd_data <- paste0(
        base_url_anberd_data,
        "startPeriod=", min(years),
        "&endPeriod=", max(years),
        "&dimensionAtObservation=AllDimensions"
      )
      write.csv(read_sdmx(url_anberd_data), anberd_file_path, row.names = FALSE)
    }
  )

  anberd_data <- if (file.exists(anberd_file_path)) {
    read.csv(anberd_file_path) %>%
      filter(
        CRITERIA == "MA",
        FREQ == "A",
        MEASURE == "B",
        PRICE_BASE == "V",
        UNIT_MEASURE == "USD_PPP",
        !is.na(ObsValue)
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
        anberd_activity = ACTIVITY
      ) %>%
      merge(anberd_correspondence_table_activity) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Eurostat data (TRNG CVT)

  trng_file_path <- file.path(download_dir, "trng_cvt_16n2.csv")
  ensure_file(
    trng_file_path,
    {
      url_trng_cvt_data <- paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/trng_cvt_16n2/1.0?",
        "compress=false",
        "&format=csvdata",
        "&formatVersion=2.0"
      )
      write.csv(read.csv(url_trng_cvt_data), trng_file_path, row.names = FALSE)
    }
  )

  trng_data <- if (file.exists(trng_file_path)) {
    read.csv(trng_file_path) %>%
      filter(
        freq == "A",
        cost == "TOTAL",
        unit == "PC",
        !is.na(OBS_VALUE)
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = geo
      ) %>%
      merge(eurostat_correspondence_table_activity) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # OECD STAN data

  stan_file_path <- file.path(download_dir, "DSD_STAN.csv")
  ensure_file(
    stan_file_path,
    {
      base_url_stan_data <- "https://sdmx.oecd.org/public/rest/data/OECD.STI.PIE,DSD_STAN@DF_STAN_2025,1.0/A...D11+B1G.V.?"
      url_stan_data <- paste0(
        base_url_stan_data,
        "startPeriod=", min(years),
        "&endPeriod=", max(years),
        "&dimensionAtObservation=AllDimensions",
        "&format=csvfilewithlabels"
      )
      write.csv(read.csv(url_stan_data), stan_file_path, row.names = FALSE)
    }
  )

  stan_data <- if (file.exists(stan_file_path)) {
    read.csv(stan_file_path) %>%
      filter(
        ACTION == "I",
        FREQ == "A",
        MEASURE %in% c("D11", "B1G"),
        PRICE_BASE == "V",
        UNIT_MEASURE == "XDC",
        UNIT_MULT == 6,
        !is.na(OBS_VALUE)
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
        oecd_activity = ACTIVITY
      ) %>%
      distinct(year, country, oecd_activity, MEASURE) %>%
      count(year, country, oecd_activity) %>%
      filter(n == 2) %>%
      merge(oecd_correspondence_table_activity) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  anberd_check <- check_country_industry(anberd_data)
  trng_check <- check_country_industry(trng_data)
  stan_check <- check_country_industry(stan_data)
  synthesis <- synthesis_from_sources(anberd_check, trng_check, stan_check)

  # -------------------------------------------------------------------
  # Output

  section_title("OECD ANBERD")
  print(anberd_check)

  section_title("Eurostat trng_cvt_16n2")
  print(trng_check)

  section_title("OECD STAN")
  print(stan_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    anberd = anberd_check,
    trng_cvt = trng_check,
    stan = stan_check,
    synthesis = synthesis
  ))
}







