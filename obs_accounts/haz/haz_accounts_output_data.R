# La Societe Nouvelle

check_haz_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for HAZ accounts (observed data)")
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
          if (verbose) message("Unable to refresh ", file_path)
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

  haz_products <- read.csv(
    "obs_accounts/haz/reference-substances.txt",
    sep = "\t",
    check.names = FALSE,
    colClasses = "character"
  )

  # -------------------------------------------------------------------
  # PRODCOM data (Eurostat)

  prodcom_file_path <- file.path(download_dir, "ds-059358.csv")
  ensure_file(
    prodcom_file_path,
    {
      base_url_prodcom_data <- "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/ESTAT/ds-059358/1.0?"
      url_prodcom_data <- paste0(
        base_url_prodcom_data,
        "format=csvdata",
        "&c[freq]=A",
        "&c[TIME_PERIOD]=", paste(years, collapse = ","),
        "&c[indicators]=PRODQNT,QNTUNIT",
        "&compressed=true"
      )
      suppressMessages(
        suppressWarnings(
          capture.output(
            prodcom_raw_data <- fread(
              url_prodcom_data,
              colClasses = "character",
              sep = ",",
              showProgress = FALSE
            ),
            type = "message"
          )
        )
      )
      write.csv(prodcom_raw_data, prodcom_file_path, row.names = FALSE)
    }
  )

  prodcom_data <- if (file.exists(prodcom_file_path)) {
    read.csv(prodcom_file_path) %>%
      pivot_wider(
        names_from = indicators,
        values_from = OBS_VALUE
      ) %>%
      filter(
        QNTUNIT == "KG",
        product %in% haz_products$`PRODCOM Code`,
        !is.na(PRODQNT)
      ) %>%
      mutate(
        year = as.character(TIME_PERIOD),
        country = reporter
      ) %>%
      select(year, country)
  } else {
    tibble(year = character(), country = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  prodcom_check <- check_country_only(prodcom_data)
  synthesis <- prodcom_check

  # -------------------------------------------------------------------
  # Output

  section_title("PRODCOM ds-059358")
  print(prodcom_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    prodcom = prodcom_check,
    synthesis = synthesis
  ))
}







