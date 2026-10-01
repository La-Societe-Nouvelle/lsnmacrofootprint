# La Societe Nouvelle

check_idr_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = FALSE,
  verbose = FALSE
) {
  if (verbose) message("Check output data for IDR accounts (observed data)")
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

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(industry = code) %>%
    select(industry, branch)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  bts_files_ids <- read_delim(
      "obs_accounts/idr/bts_files_ids.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(year, file_id, zip_name, file_name)

  n_branches <- figaro_industries %>%
    distinct(branch) %>%
    nrow()
  n_countries <- nrow(figaro_countries)

  ensure_bts_file <- function(year_i) {
    bts_file <- bts_files_ids %>% filter(as.character(year) == year_i)
    if (nrow(bts_file) == 0) return(NA_character_)
    if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

    file_path <- file.path(download_dir, bts_file$file_name)
    zip_path <- file.path(download_dir, bts_file$zip_name)

    if (!file.exists(file_path) || !use_temp_data) {
      tryCatch(
        {
          curl_download(
            url = paste0(
              "https://www.insee.fr/fr/statistiques/fichier/",
              bts_file$file_id,
              "/",
              bts_file$zip_name
            ),
            destfile = zip_path,
            quiet = !verbose
          )
          bts_files <- unzip(zipfile = zip_path, exdir = download_dir, overwrite = TRUE)
          fd_file <- bts_files[grepl("^FD", basename(bts_files), ignore.case = TRUE)]
          other_files <- setdiff(bts_files, fd_file)
          if (length(other_files) > 0) file.remove(other_files)
        },
        error = function(e) {
          if (verbose) message("Unable to refresh BTS ", year_i, ": ", conditionMessage(e))
        }
      )
    }

    if (file.exists(file_path)) file_path else NA_character_
  }

  # -------------------------------------------------------------------
  # BTS data (Insee)

  bts_check <- map_dfr(years, function(y) {
    file_path <- ensure_bts_file(y)
    if (is.na(file_path)) return(empty_check(y))

    bts_data <- tryCatch(
      data.table::fread(
        file_path,
        sep = ";",
        select = c("A38", "NBHEUR_TOT", "TRNNETO"),
        showProgress = verbose
      ) %>%
        as_tibble(),
      error = function(e) NULL
    )

    if (is.null(bts_data) || nrow(bts_data) == 0) return(empty_check(y))

    available_fr <- bts_data %>%
      mutate(branch = A38) %>%
      filter(
        branch %in% figaro_industries$branch,
        !is.na(NBHEUR_TOT),
        !is.na(TRNNETO)
      ) %>%
      distinct(branch) %>%
      nrow() %>%
      `/`(n_branches) %>%
      format_percent()

    tibble(
      year = y,
      status = "OK",
      available_data_fr = available_fr,
      available_data_row = 0,
      available_data_countries = format_percent(1 / n_countries)
    )
  })

  # -------------------------------------------------------------------
  # ILOSTAT data

  ilostat_path <- file.path(download_dir, "LAP_2LID_QTL_RT_A.csv")
  ensure_file(
    ilostat_path,
    {
      ilostat_raw_data <- get_ilostat("LAP_2LID_QTL_RT_A", quiet = TRUE)
      write.csv(ilostat_raw_data, ilostat_path, row.names = FALSE)
    }
  )

  ilostat_data <- if (file.exists(ilostat_path)) {
    read.csv(ilostat_path) %>%
      filter(
        classif1 %in% c("DCL_DECILE_01", "DCL_DECILE_02", "DCL_DECILE_09", "DCL_DECILE_10"),
        !is.na(obs_value)
      ) %>%
      mutate(
        year = as.character(time),
        country = countrycode(ref_area, "iso3c", "iso2c", nomatch = NULL),
        decile = classif1
      ) %>%
      filter(country %in% figaro_countries$country) %>%
      select(year, country, decile)
  } else {
    tibble(year = character(), country = character(), decile = character())
  }

  ilostat_complete <- ilostat_data %>%
    distinct(year, country, decile) %>%
    count(year, country) %>%
    filter(n == 4) %>%
    select(year, country)

  ilostat_check <- check_country_only(ilostat_complete)

  # -------------------------------------------------------------------
  # Synthese

  synthesis <- bts_check %>%
    rename(status_bts = status, available_data_fr_bts = available_data_fr) %>%
    select(year, status_bts, available_data_fr_bts) %>%
    left_join(
      ilostat_check %>%
        rename(
          status_ilostat = status,
          available_data_row_ilostat = available_data_row,
          available_data_countries_ilostat = available_data_countries
        ) %>%
        select(year, status_ilostat, available_data_row_ilostat, available_data_countries_ilostat),
      by = "year"
    ) %>%
    transmute(
      year,
      status = if_else(status_bts == "OK" & status_ilostat == "OK", "OK", "NOK"),
      available_data_fr = available_data_fr_bts,
      available_data_row = available_data_row_ilostat,
      available_data_countries = available_data_countries_ilostat
    )

  # -------------------------------------------------------------------
  # Output

  section_title("Base Tous Salaries")
  print(bts_check)

  section_title("ILOSTAT LAP_2LID_QTL_RT_A")
  print(ilostat_check)

  section_title("Synth\u00e8se")
  print(synthesis)

  invisible(list(
    bts = bts_check,
    ilostat = ilostat_check,
    synthesis = synthesis
  ))
}







