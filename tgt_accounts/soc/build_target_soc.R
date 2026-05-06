# La Société Nouvelle

#' TARGETS BUILDER - SOC
#'
#' Note :
#'   Update target function for indic SOC
#'
#' Targets :
#'   - FRA : 100% in 2050
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_target_soc <- function(
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

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_soc.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_soc.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    )

  # -------------------------------------------------------------------

  last_year_obs = max(as.integer(fpt_obs$year), na.rm = TRUE)

  years <- (last_year_obs + 1) : 2030
  n_years <- 2050 - years[1]

  # -------------------------
  # Start point (base)

  base_targets <- fpt_obs %>%
    filter(year == last_year_obs) %>%
    rename(base_year = year,
           base_fpt = value) %>%
    select(country, industry, base_year, base_fpt)

  # -------------------------
  # Targets coefs

  target_coefs <- base_targets %>%
    mutate(
      target_2050 = ifelse(country == 'FR', 100.0, NA), # taux de contribution de 100%
      coef_yearly = ifelse(is.na(target_2050) | base_fpt == 100.0, 1.0, (target_2050 / base_fpt)^(1 / n_years)) # pas d'augmentation si taux de contribution atteint
    ) %>%
    select(country, industry, target_2050, coef_yearly)

  # -------------------------
  # Targets fpt

  targets_data <- base_targets %>%
    merge(target_coefs) %>%
    # add years ----------------------------------------
    slice(rep(1:n(), each = length(years))) %>%
    mutate(year = rep(years, n()/length(years))) %>%
    # build raw fpt tgt --------------------------------
    mutate(
      n = year - as.integer(base_year),
      fpt_tgt = base_fpt * (coef_yearly^n)
    ) %>%
    # build impact tgt ---------------------------------
    merge(main_aggregates_data) %>%
    mutate(
      impact_tgt = round(fpt_tgt * NVA, 1)
    ) %>%
    # apply trend for other countries ------------------
    merge(impacts_trd_data) %>%
    mutate(
      impact_tgt = ifelse(country == 'FR', impact_tgt, impacts_trd)
    ) %>%
    # check increasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, impact_tgt / NVA, 0),
      fpt_tgt = pmax(fpt_tgt, base_fpt),
      fpt_tgt = cummax(fpt_tgt),
      impact_tgt = fpt_tgt * NVA
    ) %>%
    ungroup() %>%
    # select -------------------------------------------
    select(country, industry, year, impact_tgt)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(targets_data) != size) {
    error_data <<- targets_data
    stop("ERROR - Wrong size for tgt accounts (SOC)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (SOC)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "soc_tgt",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_soc.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
