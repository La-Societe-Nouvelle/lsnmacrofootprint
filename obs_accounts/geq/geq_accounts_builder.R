# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for gender pay gap (GEQ)
#'
#' Main sources:
#'   - Base Tous Salariés (Insee)
#'   - EAR_EHRA_SEX_ECO_CUR_NB_A (Average hourly earnings of employees by sex, economic activity and currency - Ilostat)
#'
#' Output data
#'   Accounts are in percentage
#'
#' BTS Data :
#'  primary key :
#'    - A38
#'    - TRNNETO
#'    - DEPR - Département de résidence
#'    - DEPT - Département d'implantation de l'établissement
#'    - PCS - Profession catégorie socio-professionnelle des emplois
#'    - SEXE
#'    - AGE - Tranche d'age quadriennale
#'    - TYP_EMPLOI
#'
#' Methodology:
#' Pour la France, les écarts de rémunération femmes/hommes sont obtenus à partir de la Base Tous Salariés (BTS, Insee)
#' Pour les autres pays/régions, les données françaises sont ajustées via les données ILOSTAT
#' Les données manquantes sont complétées via la procédure de similarité économique.
#'
#' build_geq_obs_accounts()

build_geq_obs_accounts <- function(
  years = 2016:2023,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build GEQ accounts for observed data")
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
    select(industry, branch, sector)

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
      "obs_accounts/geq/bts_files_ids.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(year, file_id, zip_name, file_name)

  metadata_trnneto <- read_delim(
    "obs_accounts/geq/metadata_trnneto.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>%
    select(TRNNETO, NETO)

  if (verbose) cat("Metadata loaded\n")

  # -------------------------------------------------------------------
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
  # BTS data (Insee)

  # if (verbose) cat("Loading BTS data...\n")

  # base_url_bts_data <- "https://www.insee.fr/fr/statistiques/fichier/"

  # bts_raw_data <- lapply(years$year, function(year_i)
  # {
  #   if (verbose) cat(paste0("Année ", year_i, "\n"))

  #   # File ID / File Name
  #   file_id <- bts_files_ids %>% filter(year == year_i) %>% pull(file_id)
  #   zip_name <- bts_files_ids %>% filter(year == year_i) %>% pull(zip_name)
  #   file_name <- bts_files_ids %>% filter(year == year_i) %>% pull(file_name)

  #   url_bts_data <- paste0(base_url_bts_data, file_id,"/",zip_name)

  #   zip_path <- file.path(download_dir, zip_name)
  #   file_path  <- file.path(download_dir, file_name)

  #   # Téléchargement des données (download & unzip)

  #   if (!file.exists(file_path) || !use_temp_data)
  #   {
  #     if (verbose) cat("Téléchargement des données BTS\n")

  #     curl_download(
  #       url = url_bts_data,
  #       destfile = zip_path,
  #       quiet = !verbose
  #     )

  #     if (!file.exists(zip_path) || file.info(zip_path)$size == 0) {
  #       stop(sprintf("Téléchargement échoué pour les données \"Base Tous Salariés\" : %s", url_bts_data))
  #     }

  #     if (verbose) cat("Extraction des données BTS\n")

  #     bts_files <- unzip(
  #       zipfile = zip_path,
  #       exdir = download_dir,
  #       overwrite = TRUE
  #     )

  #     fd_file <- bts_files[grepl("^FD", basename(bts_files), ignore.case = TRUE)]

  #     # Supprimer les autres fichiers extraits
  #     other_files <- setdiff(bts_files, fd_file)
  #     if (length(other_files) > 0) {
  #       file.remove(other_files)
  #     }
  #   }

  #   bts_raw_data_year <- read.csv(
  #       file_path,
  #       sep = ";"
  #     ) %>%
  #     mutate(year = year_i)

  #   return(bts_raw_data_year)
  # }) %>%
  #   bind_rows()

  # bts_data <- bts_raw_data %>%
  #   merge(metadata_trnneto) %>%
  #   # Format
  #   mutate(
  #     country = "FR",
  #     branch = A38,
  #     sex = case_when(
  #       SEXE == 1 ~ "men",   # Hommes
  #       SEXE == 2 ~ "women", # Femmes
  #       TRUE ~ NA
  #     ),
  #     working_hours = NBHEUR_TOT,
  #     net_pay = NETO # Milieu de la tranche de rémunération (salaire net total)
  #   ) %>%
  #   filter(
  #     !is.na(sex),
  #     branch %in% figaro_industries$branch
  #   ) %>%
  #   select(year, country, branch, sex, working_hours, net_pay)

  # if (verbose) cat("BTS data loaded\n")

  # -------------------------------------------------------------------
  # ILOSTAT data

  if (verbose) cat("Loading ILOSTAT data...\n")

  ilostat_path  <- file.path(download_dir, "EAR_EHRA_SEX_ECO_CUR_NB_A.csv")

  if (!file.exists(ilostat_path) || !use_temp_data)
  {
    if (verbose) print("Téléchargement des données ILOSTAT")

    ilostat_raw_data <- get_ilostat(
      "EAR_EHRA_SEX_ECO_CUR_NB_A",
      segment = "indicator",
      quiet = TRUE
    )

    write.csv(ilostat_raw_data, ilostat_path, row.names = FALSE)
  }

  ilostat_raw_data <- read.csv(ilostat_path)

  ilostat_data <- ilostat_raw_data %>%
    filter(
      classif2 == "CUR_TYPE_PPP",
      str_starts(classif1, "ECO_ISIC4_"),
      sex %in% c("SEX_M","SEX_F","SEX_T")
    ) %>%
    # keep closest years
    crossing(years) %>%
    mutate(
      time_num = as.integer(time),
      ecart = abs(time_num - as.integer(year))
    ) %>%
    group_by(year) %>%
    filter(ecart == min(ecart, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      country = countrycode(ref_area, "iso3c", "iso2c",nomatch = NULL),
      sector = sub("ECO_ISIC4_", "", classif1),
      sex = case_when(
        sex == "SEX_M" ~ "men",   # Hommes
        sex == "SEX_F" ~ "women", # Femmes
        sex == "SEX_T" ~ "all",   # Total
        TRUE ~ NA
      ),
      hourly_rate = obs_value
    ) %>%
    select(year, country, sector, sex, hourly_rate)

  if (verbose) cat("ILOSTATS data loaded\n")

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  # BTS - Gender pay gap by industry

  # bts_geq_fr <- bts_data %>%
  #   group_by(year, country, branch, sex) %>%
  #   summarise(
  #     # rémunération moyenne (pondérée par heures salariées)
  #     net_pay = sum(working_hours * net_pay, na.rm = TRUE) / sum(working_hours, na.rm = TRUE),
  #     # somme des heures salariées
  #     working_hours = sum(working_hours, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   pivot_wider(
  #     names_from = sex,
  #     values_from = c(working_hours, net_pay),
  #     names_glue = "{.value}_{sex}"
  #   ) %>%
  #   mutate(
  #     net_pay_total = sum(working_hours_men * net_pay_men + working_hours_women * net_pay_women) / sum(working_hours_men + working_hours_women),
  #     gender_pay_gap = round( abs(net_pay_men - net_pay_women) / net_pay_total *100, digits = 1)
  #   ) %>%
  #   merge(figaro_industries) %>%
  #   select(year, country, industry, gender_pay_gap, net_pay_total, net_pay_men, net_pay_women)

  # ILOSTAT - Gender pay gap by industry

  ilostat_geq <- ilostat_data %>%
    pivot_wider(
      names_from = sex,
      values_from = hourly_rate,
      names_glue = "hourly_rate_{sex}"
    ) %>%
    # temp missing hourly women in ILOSTAT for FR B 2017
    mutate(
      hourly_rate_women = if_else(
        is.na(hourly_rate_women),
        hourly_rate_all + (hourly_rate_all - hourly_rate_men),
        hourly_rate_women
      )
    ) %>%
    mutate(
      gender_pay_gap = round( abs(hourly_rate_men - hourly_rate_women) / hourly_rate_all *100, digits = 1)
    ) %>%
    merge(figaro_industries) %>%
    select(year, country, industry, gender_pay_gap)

  # Coef BTS/ILOSTAT for FRANCE

  # coefs_ilostat <- bts_geq_fr %>%
  #   rename(bts_gender_pay_gap = gender_pay_gap) %>%
  #   merge(ilostat_geq) %>% # by year, country, industry
  #   mutate(
  #     coef_ilostat = round(bts_gender_pay_gap / gender_pay_gap, digits = 3)
  #   ) %>%
  #   select(year, industry, coef_ilostat)

  # -------------------------
  # FIGARO accounts

  raw_geq_accounts <- ilostat_geq %>%
    # merge(coefs_ilostat) %>%
    mutate(
      # value = round(gender_pay_gap * coef_ilostat, digits = 1), # prev methodology
      value = round(gender_pay_gap, digits = 1),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  figaro_geq_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_geq_accounts,
      by = c("year", "country", "industry")
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_geq_accounts <- figaro_geq_accounts_raw %>%
    proxy_missing_value_by_similarity(., "GEQ") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  figaro_geq_accounts <- figaro_geq_accounts %>%
    clean_outliers(., serie_pkey = c("country", "industry")) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_geq_accounts) != size) {
    error_data <<- figaro_geq_accounts
    stop("ERROR - Wrong size for obs accounts (GEQ)")
  } else if (any(is.na(figaro_geq_accounts$value))) {
    error_data <<- figaro_geq_accounts
    stop("ERROR - NA values in obs accounts (GEQ)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <<- figaro_geq_accounts %>%
    mutate(
      serie_id    = "geq_obs",
      value       = round(value, digits = 1),
      unit        = "PCT",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_geq.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
