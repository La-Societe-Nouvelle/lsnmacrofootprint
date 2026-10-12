# La Societe Nouvelle

check_mat_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for MAT accounts (observed data)")
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

  check_country_only <- function(data) {
    n_countries <- nrow(figaro_countries)
    n_row_countries <- n_countries - 1

    map_dfr(years, function(y) {
      year_data <- data %>%
        filter(year == y) %>%
        filter(country %in% figaro_countries$country) %>%
        distinct(country)

      if (nrow(year_data) == 0) return(empty_check(y))

      tibble(
        year = y,
        status = "OK",
        available_data_fr = format_percent(any(year_data$country == "FR")),
        available_data_row = format_percent(nrow(year_data %>% filter(country != "FR")) / n_row_countries),
        available_data_countries = format_percent(nrow(year_data) / n_countries)
      )
    })
  }

  years <- as.character(years)

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  # -------------------------------------------------------------------
  # UNEP data

  unep_file_path <- file.path(download_dir, "mfa4.csv")
  ensure_file(
    unep_file_path,
    {
      url_unep_data <- "https://unep-irp-resourcepanel.resource-use.global/mfa4/export?flowTypes[0]=DE"
      write.csv(read.csv(url_unep_data), unep_file_path, row.names = FALSE)
    }
  )

  unep_data <- if (file.exists(unep_file_path)) {
    read.csv(unep_file_path) %>%
      pivot_longer(
        starts_with("X"),
        names_to = "year",
        values_to = "value"
      ) %>%
      filter(
        Flow.code == "DE",
        Flow.unit == "t",
        Category %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals"),
        !is.na(value)
      ) %>%
      mutate(
        year = sub("^X", "", year),
        country = countrycode(Country, "country.name", "iso2c", nomatch = NULL)
      ) %>%
      select(year, country)
  } else {
    tibble(year = character(), country = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  unep_check <- check_country_only(unep_data)
  synthesis <- unep_check

  # -------------------------------------------------------------------
  # Output

  section_title("UNEP mfa4")
  print(unep_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    unep = unep_check,
    synthesis = synthesis
  ))
}







