# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for pay gap index (IDR)
#'
#' Main sources :
#'   -
#'
#' Output data
#'   No unit (ratio greater than 1)
#'   Digits: 2
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_idr_obs_accounts()

build_idr_obs_accounts <- function(
  years = 2016:2023,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build IDR accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  years <- tibble(year = as.character(years))

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(
      industry = code
    ) %>%
    select(industry, branch)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      country = code
    ) %>%
    select(country)

  bts_files_ids <- read_delim(
      "obs_accounts/idr/bts_files_ids.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(year, file_id, zip_name, file_name)

  metadata_trnneto <- read_delim(
    "obs_accounts/idr/metadata_trnneto.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    select(TRNNETO, NETO)

  # -------------------------------------------------------------------
  if (verbose) cat("Metadata loaded\n")

  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # BTS Data (Insee)

  if (verbose) print("Loading BTS data...\n")

  base_url_bts_data <- "https://www.insee.fr/fr/statistiques/fichier/"

  bts_raw_data <- lapply(years$year, function(year_i)
  {
    if (verbose) cat(paste0("Année ", year_i, "\n"))

    # File ID
    file_id <- bts_files_ids %>% filter(year == year_i) %>% pull(file_id)
    zip_name <- bts_files_ids %>% filter(year == year_i) %>% pull(zip_name)
    file_name <- bts_files_ids %>% filter(year == year_i) %>% pull(file_name)

    url_bts_data <- paste0(base_url_bts_data,"/",zip_name)

    zip_path <- file.path(download_dir, zip_name)
    file_path  <- file.path(download_dir, file_name)

    # Téléchargement des données (download & unzip)

    if (!file.exists(file_path) || !use_temp_data)
    {
      if (verbose) cat("Téléchargement des données BTS\n")

      curl_download(
        url = url_bts_data,
        destfile = zip_path,
        quiet = !verbose
      )

      if (!file.exists(zip_path) || file.info(zip_path)$size == 0) {
        stop(sprintf("Téléchargement échoué pour les données \"Base Tous Salariés\" : %s", url_bts_data))
      }

      bts_files <- unzip(
        zipfile = zip_path,
        exdir = download_dir,
        overwrite = TRUE
      )

      fd_file <- bts_files[grepl("^FD", basename(bts_files), ignore.case = TRUE)]

      # Supprimer les autres fichiers extraits
      other_files <- setdiff(bts_files, fd_file)
      if (length(other_files) > 0) {
        file.remove(other_files)
      }
    }

    bts_raw_data_year <- read.csv(
      file_path,
      sep = ";"
    ) %>%
      mutate(year = year_i)

    return(bts_raw_data_year)
  }) %>%
    bind_rows()

  bts_data <- bts_raw_data %>%
    merge(metadata_trnneto) %>%
    # Format
    mutate(
      country = "FR",
      branch = A38,
      working_hours = NBHEUR_TOT,
      net_pay = NETO # Milieu de la tranche de rémunération nette
    ) %>%
    filter(
      branch %in% figaro_industries$branch
    ) %>%
    select(year, country, branch, working_hours, net_pay)

  # -------------------------------------------------------------------
  # ILOSTAT data

  if (verbose) print("Chargement des données ILOSTAT...")

  ilostat_path  <- file.path(download_dir, "LAP_2LID_QTL_RT_A.csv")

  if (!file.exists(ilostat_path) | !use_temp_data)
  {
    if (verbose) print("Téléchargement des données ILOSTAT...")

    ilostat_raw_data <- get_ilostat(
      "LAP_2LID_QTL_RT_A",
      quiet = TRUE
    )

    write.csv(ilostat_raw_data, ilostat_path, row.names = FALSE)
  }

  ilostat_raw_data <- read.csv(ilostat_path)

  ilostat_data <- ilostat_raw_data %>%
    filter(
      classif1 %in% c("DCL_DECILE_01", "DCL_DECILE_02", "DCL_DECILE_09", "DCL_DECILE_10")
    ) %>%
    crossing(years) %>%
    mutate(
      time_num = as.integer(time),
      ecart = abs(time_num - as.integer(year))
    ) %>%
    group_by(year) %>%
    filter(ecart == min(ecart, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      year = time,
      country = countrycode(ref_area, "iso3c", "iso2c", nomatch = NULL),
      decile = classif1,
      value = obs_value
    ) %>%
    select(year, country, decile, value)

  # -------------------------------------------------------------------
  # Building FIGARO accounts


  if (verbose) cat("Building FIGARO accounts...\n")

  if (verbose) print("Construction des données")

  # BTS - Pay gap by industry

  bts_idr_fr <- bts_data %>%
    # compute hourly rate
    mutate(
      hourly_rate = round(net_pay / working_hours, digits = 2),
    ) %>%
    filter(hourly_rate != "Inf") %>%
    group_by(year, country, branch) %>%
    summarise(
      decile_9 = quantile(hourly_rate, 0.9, na.rm = TRUE),
      decile_1 = quantile(hourly_rate, 0.1, na.rm = TRUE),
      interdecile_range = round(decile_9 / decile_1, digits = 2),
      .groups = "drop"
    ) %>%
    merge(figaro_industries) %>%
    rename(base_country = country) %>%
    select(year, base_country, industry, interdecile_range)

  countries_idr_coef <- ilostat_data %>%
    pivot_wider(
      names_from = decile,
      values_from = value
    ) %>%
    mutate(
      pay_gap_index = round((DCL_DECILE_09 + DCL_DECILE_10) / (DCL_DECILE_01 + DCL_DECILE_02), digits = 2), # /!\ definition differs
      coef = pay_gap_index / pay_gap_index[country == "FR"]
    ) %>%
    select(year, country, coef)

  # IDR accounts data for each FIGARO Country/Industry

  raw_idr_accounts <- bts_idr_fr %>%
    merge(countries_idr_coef) %>% # by year
    mutate(
      value = round(interdecile_range * coef, digits = 2),
      flag = ifelse(country == "FR", "", "e")
    ) %>%
    select(year, country, industry, value, flag)

  if (verbose) print("Finalisation des données")

  figaro_idr_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_idr_accounts,
      by = c("year", "country", "industry")
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_idr_accounts <- figaro_idr_accounts_raw %>%
    proxy_missing_value_by_similarity(., "IDR") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  figaro_idr_accounts <- figaro_idr_accounts %>%
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_idr_accounts) != size) {
    error_data <<- figaro_idr_accounts
    stop("ERROR - Wrong size for obs accounts (IDR)")
  } else if (any(is.na(figaro_idr_accounts$value))) {
    error_data <<- figaro_idr_accounts
    stop("ERROR - NA values in obs accounts (IDR)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_idr_accounts %>%
    mutate(
      serie_id    = "idr_obs",
      value       = round(value, digits = 2),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_idr.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
