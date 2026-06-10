# La Société Nouvelle

#' TARGETS BUILDER - NRG
#'
#' Note :
#'   Update target function for indic NRG
#'
#' Targets :
#'   - FRA : PPE data projections (period 2023-2028)
#'   - other countries : trend
#'
#' Les données PPE sont utilisées pour déterminer un taux de réduction annuel (à partir de la période 2023-2028).
#' Ce taux est appliqué à la dernière année observée (impacts bruts).
#' L'empreinte doit obligatoirement diminuer.
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_nrg_tgt_accounts <- function(
  verbose = FALSE
) {
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")

  # -------------------------------------------------------------------
  # Metadata

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

  ppe_correspondence_table_secteur <- read_delim(
      "tgt_accounts/nrg/ppe_correspondence_table_secteur.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, secteur_ppe)

  ppe_data <- read_delim(
      "tgt_accounts/nrg/ppe.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(secteur_ppe, annee, consommation_energie)

  # -------------------------------------------------------------------
  # OBS Accounts

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_nrg.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_nrg.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    ) %>%
    mutate(
      year = as.character(year)
    )

  # -------------------------------------------------------------------

  last_year_obs <- max(as.integer(obs_data$year), na.rm = TRUE)

  tgt_years <- (last_year_obs + 1) : 2030
  n_years <- 2030 - tgt_years[1]

  years <- tibble(year = as.character(tgt_years))

  # -------------------------------------------------------------------
  # FIGARO Economic data

  main_aggregates_years <- c(years$year, last_year_obs)

  main_aggregates_data_raw <- map_dfr(
    main_aggregates_years,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, NVA)

  # -------------------------------------------------------------------

  # -------------------------
  # Start point (base)

  base_targets <- obs_data %>%
    filter(year == "2023") %>%
    merge(main_aggregates_data) %>%
    mutate(
      base_year = year,
      base_impact = value,
      base_fpt = ifelse(NVA > 0, value / NVA, 0)
    ) %>%
    select(country,industry,base_year,base_impact,base_fpt)

  # -------------------------
  # Targets

  target_ppe_coefs <- ppe_data %>%
    pivot_wider(
      names_from = "annee",
      values_from = "consommation_energie",
      names_glue = "consommation_energie_{annee}"
    ) %>%
    mutate(
      coef_yearly = (consommation_energie_2028 / consommation_energie_2023)^(1/5)
    ) %>%
    select(secteur_ppe,coef_yearly)

  targets_raw_data_fr <- base_targets %>%
    filter(country == "FR") %>%
    crossing(years) %>%
    left_join(ppe_correspondence_table_secteur) %>%
    left_join(target_ppe_coefs) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      tgt_value = round(base_impact * (coef_yearly^n), digits = 3)
    ) %>%
    select(country, industry, year, tgt_value)

  # -------------------------

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build accounts tgt data --------------------------
    left_join(targets_raw_data_fr) %>%
    left_join(trd_data) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check decreasing fpt -----------------------------
    left_join(base_targets) %>%
    left_join(main_aggregates_data) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, tgt_value / NVA, 0)
    ) %>%
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = pmin(fpt_tgt, base_fpt),
      fpt_tgt = cummin(fpt_tgt),
      tgt_value = fpt_tgt * NVA
    ) %>%
    ungroup() %>%
    # select -------------------------------------------
    rename(
      value = tgt_value
    ) %>%
    select(country, industry, year, value)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(targets_data) != size) {
    error_data <<- targets_data
    stop("ERROR - Wrong size for tgt accounts (NRG)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (NRG)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "nrg_tgt",
      value       = round(value, digits = 0),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_nrg.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
