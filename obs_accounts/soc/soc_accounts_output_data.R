# La Societe Nouvelle

check_soc_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for SOC accounts (observed data)")
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

  check_fr_industry <- function(data) {
    n_industries <- nrow(figaro_industries)

    map_dfr(years, function(y) {
      year_data <- data %>%
        filter(year == y) %>%
        filter(industry %in% figaro_industries$industry) %>%
        distinct(industry)

      if (nrow(year_data) == 0) return(empty_check(y))

      tibble(
        year = y,
        status = "OK",
        available_data_fr = format_percent(nrow(year_data) / n_industries),
        available_data_row = 100,
        available_data_countries = 100
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

  table_passage_urssaf_data <- read_delim(
      "obs_accounts/soc/table_passage_urssaf.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, secteur_na88)

  # -------------------------------------------------------------------
  # URSSAF data

  urssaf_file_name <- "nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88.csv"
  urssaf_file_path <- file.path(download_dir, urssaf_file_name)
  ensure_file(
    urssaf_file_path,
    {
      url_urssaf_data <- "https://open.urssaf.fr/api/explore/v2.1/catalog/datasets/nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
      write.csv(
        read.csv(url_urssaf_data, sep = ";", check.names = TRUE),
        urssaf_file_path,
        row.names = FALSE
      )
    }
  )

  urssaf_data <- if (file.exists(urssaf_file_path)) {
    raw_data <- read.csv(urssaf_file_path, check.names = TRUE)
    year_col <- names(raw_data)[grepl("Ann", names(raw_data))][1]
    sector_col <- names(raw_data)[grepl("Secteur.NA88", names(raw_data))][1]
    mass_col <- names(raw_data)[grepl("Masse", names(raw_data))][1]

    raw_data %>%
      transmute(
        year = as.character(.data[[year_col]]),
        secteur_na88 = sub(" .*", "", .data[[sector_col]]),
        value = .data[[mass_col]]
      ) %>%
      filter(!is.na(value)) %>%
      merge(table_passage_urssaf_data) %>%
      select(year, industry)
  } else {
    tibble(year = character(), industry = character())
  }

  # -------------------------------------------------------------------
  # Synthese

  urssaf_check <- check_fr_industry(urssaf_data)
  synthesis <- urssaf_check

  # -------------------------------------------------------------------
  # Output

  section_title("URSSAF ESS NA88")
  print(urssaf_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    urssaf = urssaf_check,
    synthesis = synthesis
  ))
}







