# La Societe Nouvelle

check_ghg_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for GHG accounts (observed data)")
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

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  eurostat_correspondence_table_c_orig <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_c_orig.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, c_orig)

  oecd_correspondence_table_activity <- read_delim(
      "obs_accounts/ghg/oecd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

  # -------------------------------------------------------------------
  # Eurostat data - Greenhouse gas emission footprints

  eurostat_file_path <- file.path(download_dir, "env_ac_ghgfp.csv")
  ensure_file(
    eurostat_file_path,
    {
      eurostat_raw_data <- get_eurostat(
        "env_ac_ghgfp",
        filters = list(time = years, na_item = "TOTAL", c_dest = "WORLD"),
        cache = FALSE
      )
      write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
    }
  )

  eurostat_data <- if (file.exists(eurostat_file_path)) {
    read.csv(eurostat_file_path) %>%
      filter(
        freq == "A",
        unit == "THS_T",
        na_item == "TOTAL",
        c_dest == "WORLD",
        values >= 0
      ) %>%
      merge(eurostat_correspondence_table_nace_r2) %>%
      merge(eurostat_correspondence_table_c_orig) %>%
      mutate(year = substring(as.character(time), 1, 4)) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # OECD data - Air Emissions Accounts

  oecd_file_path <- file.path(download_dir, "DSD_AEA_GHG.csv")
  ensure_file(
    oecd_file_path,
    {
      base_url_oecd_data <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.2/..EMISSIONS.T_CO2E+T...GHG...?"
      url_oecd_data <- paste0(
        base_url_oecd_data,
        "startPeriod=", min(years),
        "&endPeriod=", max(years),
        "&dimensionAtObservation=AllDimensions",
        "&format=csvfilewithlabels"
      )
      write.csv(read.csv(url_oecd_data), oecd_file_path, row.names = FALSE)
    }
  )

  oecd_data <- if (file.exists(oecd_file_path)) {
    read.csv(oecd_file_path) %>%
      filter(
        MEASURE == "EMISSIONS",
        POLLUTANT == "GHG",
        METHODOLOGY == "EMISSIONS_SEEA",
        ACTION == "I",
        FREQ == "A",
        UNIT_MEASURE == "T_CO2E",
        SOURCE == "REPORTED",
        ACTIVITY_SCOPE == "RES",
        ADJUSTMENT == "N"
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = countrycode(REF_AREA, "iso3c", "iso2c", nomatch = NULL),
        oecd_activity = ACTIVITY
      ) %>%
      merge(oecd_correspondence_table_activity) %>%
      select(year, country, industry)
  } else {
    tibble(year = character(), country = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  eurostat_check <- check_country_industry(eurostat_data)
  oecd_check <- check_country_industry(oecd_data)

  eurostat_countries <- eurostat_data %>%
    distinct(country) %>%
    pull(country)

  ghg_priority_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(tibble(year = years)) %>%
    left_join(
      eurostat_data %>%
        distinct(year, country, industry) %>%
        mutate(has_eurostat = TRUE),
      by = c("year", "country", "industry")
    ) %>%
    left_join(
      oecd_data %>%
        distinct(year, country, industry) %>%
        mutate(has_oecd = TRUE),
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      has_data = if_else(
        country %in% eurostat_countries,
        !is.na(has_eurostat),
        !is.na(has_oecd)
      )
    ) %>%
    filter(has_data) %>%
    select(year, country, industry)

  synthesis <- check_country_industry(ghg_priority_data)

  # -------------------------------------------------------------------
  # Output

  section_title("Eurostat env_ac_ghgfp")
  print(eurostat_check)

  section_title("OECD Air Emissions Accounts")
  print(oecd_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    eurostat = eurostat_check,
    oecd = oecd_check,
    synthesis = synthesis
  ))
}
