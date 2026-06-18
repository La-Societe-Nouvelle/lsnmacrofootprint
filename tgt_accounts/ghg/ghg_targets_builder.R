# La Société Nouvelle

#' TARGETS BUILDER - GHG
#'
#' Note :
#'   Update target function for indic GHG
#'
#' Targets :
#'   - FRA : SNBC
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_ghg_tgt_accounts <- function(
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

  snbc_correspondence_table_secten <- read_delim(
      "tgt_accounts/ghg/snbc_correspondence_table_secten.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, secten)

  snbc_data <- read_delim(
      "tgt_accounts/ghg/snbc.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(secten, annee, budget_carbone)

  # -------------------------------------------------------------------
  # OBS Accounts

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_ghg.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_ghg.csv")

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
    mutate(
      rate = ifelse(PRD == 0, 0, NVA / PRD)
    ) %>%
    select(year, country, industry, PRD, NVA, rate)

  # -------------------------------------------------------------------

  # -------------------------
  # Start point (base)

  last_impacts_obs <- obs_data %>%
    filter(year == last_year_obs) %>%
    merge(main_aggregates_data) %>%
    mutate(
      last_year_obs = year,
      last_impact_obs = value,
      last_fpt_obs = ifelse(NVA > 0, value / NVA, 0)
    ) %>%
    select(country,industry,last_year_obs,last_impact_obs,last_fpt_obs)

  # -------------------------
  # Targets

  # Emissions obersvées sur l'année de référence de la SNBC (2015)

  base_impacts_snbc <- obs_data_raw %>%
    filter(
      year == "2015"
    ) %>%
    rename(
      base_snbc_year = year,
      base_snbc_impact = value
    ) %>%
    select(country, industry, base_snbc_year, base_snbc_impact)

  # Coefficients de réduction par secten (SNBC) 2015-2029

  target_snbc_coefs <- snbc_data %>%
    pivot_wider(names_from = "annee", values_from = "budget_carbone") %>%
    mutate(
      coef_2029 = `2029` / `2015`
    ) %>%
    select(secten,coef_2029)

  # Coefficients de réduction par industry FIGARO

  target_coefs <- last_impacts_obs %>%
    merge(base_impacts_snbc) %>%
    merge(snbc_correspondence_table_secten) %>%
    merge(target_snbc_coefs) %>%
    mutate(
      # Objectif : minimum entre taux de réduction /r 2015 et dernière observation
      impact_tgt = pmin(base_snbc_impact * coef_2029, last_impact_obs),
      # Coef de réduction entre l'objectif et la dernière observation
      coef_total = ifelse(last_impact_obs > 0, impact_tgt / last_impact_obs, 1.0),
      # Coef annuel
      coef_yearly = coef_total ^ (1 / (2029 - as.integer(last_year_obs)))
    ) %>%
    select(country, industry, coef_yearly)

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build raw impact tgt -----------------------------
    left_join(last_impacts_obs, by = c("country", "industry")) %>%
    left_join(target_coefs, by = c("country", "industry")) %>%
    mutate(
      n = as.integer(year) - as.integer(last_year_obs),
      tgt_value = ifelse(country == "FR", last_impact_obs * (coef_yearly^n), NA)
    ) %>%
    # apply trend for other countries ------------------
    left_join(trd_data, by = c("country", "industry", "year")) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check decreasing fpt -----------------------------
    merge(main_aggregates_data) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, tgt_value / NVA, 0)
    ) %>%
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = pmin(fpt_tgt, last_fpt_obs),
      fpt_tgt = cummin(fpt_tgt)
    ) %>%
    ungroup() %>%
    mutate(
      tgt_value = ifelse(NVA > 0, fpt_tgt * NVA, 0)
    ) %>%
    # select -------------------------------------------
    rename(
      value = tgt_value
    ) %>%
    select(country, industry, year, value)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(targets_data) != size) {
    error_data <<- targets_data
    stop("ERROR - Wrong size for tgt accounts (GHG)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (GHG)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "ghg_tgt",
      value       = round(value, digits = 0),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_ghg.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
