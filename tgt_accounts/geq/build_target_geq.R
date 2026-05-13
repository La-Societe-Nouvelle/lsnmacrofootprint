# La Société Nouvelle

#' TARGETS BUILDER - GEQ
#'
#' Note :
#'   Update target function for indic GEQ
#'
#' Targets :
#'   - FRA : < 1.0 % in 2050
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_target_geq <- function(
  years = 2010:2030,
  verbose = FALSE
) {
  # -------------------------------------------------------------------
  # Utils

  # ...

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
  # OBS Accounts

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_geq.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_geq.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    )

  # -------------------------------------------------------------------
  # TGT Accounts

  last_year_obs = max(as.integer(obs_data_raw$year), na.rm = TRUE)

  tgt_years <- (last_year_obs + 1) : 2030
  n_years <- 2050 - tgt_years[1]

  years <- tibble(year = as.character(tgt_years))

  # Start point
  base_targets <- obs_data_raw %>%
    filter(year == last_year_obs) %>%
    rename(
      base_year = year,
      base_fpt = value
    ) %>%
    select(country,industry,base_year,base_fpt)

  # Coef (reduction gender gap)
  target_coefs <- base_targets %>%
    mutate(
      target_2050 = ifelse(country == "FR", 1.0, base_fpt), # écart < 1.0 %
      coef_yearly = ifelse(base_fpt <= 1.0, 1.0, (target_2050 / base_fpt)^(1 / n_years)) # pas de réduction si écart inférieur à 1.0 %
    ) %>%
    select(country, industry, target_2050, coef_yearly)

  # Fpt targets for each year
  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      base_targets
    ) %>%
    # build raw fpt tgt --------------------------------
    merge(target_coefs) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      tgt_value = base_fpt * (coef_yearly^n)
    ) %>%
    # use trend for other countries --------------------
    merge(trd_data) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check decreasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      tgt_value = pmin(tgt_value, base_fpt), # value < base
      tgt_value = cummin(tgt_value) # value < prev year value
    ) %>%
    ungroup() %>%
    mutate(
      value = tgt_value
    ) %>%
    # select -------------------------------------------
    select(country, industry, year, value)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(targets_data) != size) {
    error_data <<- targets_data
    stop("ERROR - Wrong size for tgt accounts (GEQ)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (GEQ)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "geq_tgt",
      value       = round(value, digits = 1),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_geq.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
