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

build_target_ghg <- function(
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

  eurostat_correspondence_table_geo <- read_delim(
      "tgt_accounts/ghg/snbc.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    select(secten, annee, budget_carbone)

  # -------------------------------------------------------------------
  # FIGARO Economic data

  main_aggregates_data_raw <- map_dfr(
    years,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    mutate(
      rate = ifelse(PRD == 0, 0, NVA / PRD)
    ) %>%
    select(year, country, industry, PRD, NVA, rate)

  # -------------------------------------------------------------------
  # OBS Accounts

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_ghg.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_ghg.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    )

  # -------------------------------------------------------------------

  last_year_obs = max(as.integer(impacts_obs_data$year), na.rm = TRUE)

  years <- last_year_obs : 2030
  n_years <- 2030 - years[1]

  # -------------------------
  # Start point (base)

  last_impacts_obs <- obs_data_raw %>%
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

  base_impacts_snbc <- impacts_obs_data %>%
    filter(year == '2015') %>%
    rename(base_snbc_year = year,
           base_snbc_impact = value) %>%
    select(country, industry, base_snbc_year, base_snbc_impact)

  target_snbc_coefs <- snbc_data %>%
    pivot_wider(names_from = "annee", values_from = "budget_carbone") %>%
    mutate(
      coef_2029 = `2029` / `2015`
    ) %>%
    select(secten,coef_2029)

  target_coefs = last_impacts_obs %>%
    merge(base_impacts_snbc) %>%
    merge(metadata_industries) %>%
    merge(target_snbc_coefs) %>%
    mutate(
      impact_tgt = pmin(base_snbc_impact * coef_2029, last_impact_obs),
      coef_total = ifelse(last_impact_obs>0, impact_tgt/last_impact_obs, NA),
      coef_yearly = coef_total^(1/(2029 - as.integer(last_year_obs)))
    ) %>%
    select(country, industry, coef_yearly)

  targets_data = last_impacts_obs %>%
    # add years ----------------------------------------
    slice(rep(1:n(), each = length(years))) %>%
    mutate(year = rep(years, n()/length(years))) %>%
    # build raw impact tgt -----------------------------
    merge(target_coefs) %>%
    mutate(
      n = year - as.integer(last_year_obs),
      impact_tgt = ifelse(country == 'FR', last_impact_obs * (coef_yearly^n), NA)
    ) %>%
    # apply trend for other countries ------------------
    merge(impacts_trd_data) %>%
    mutate(
      impact_tgt = ifelse(country == 'FR', impact_tgt, impacts_trd)
    ) %>%
    # build raw fpt tgt --------------------------------
    merge(main_aggregates_data) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, impact_tgt / NVA, NA)
    ) %>%
    # check decreasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = pmin(fpt_tgt, last_fpt_obs),
      fpt_tgt = cummin(fpt_tgt),
      value = fpt_tgt * NVA
    ) %>%
    ungroup() %>%
    # select -------------------------------------------
    select(country, industry, year, value)

  # Check
  size <- nrow(tgt_years)*nrow(figaro_industries)*nrow(figaro_countries)
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
