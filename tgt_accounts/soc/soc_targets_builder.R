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

build_soc_tgt_accounts <- function(
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
  # OBS Accounts

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_soc.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_soc.csv")

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
  n_years <- 2050 - tgt_years[1]

  years <- tibble(year = as.character(tgt_years))

  # -------------------------------------------------------------------
  # FIGARO Economic data

  main_aggregates_years <- c(years$year, as.character(last_year_obs))

  main_aggregates_data_raw <- map_dfr(
    main_aggregates_years,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, NVA)

  # -------------------------------------------------------------------
  # Building SOC Targets accounts data

  # -------------------------
  # Start point (base)

  base_targets <- obs_data %>%
    filter(year == last_year_obs) %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA * 100, 0)
    ) %>%
    rename(
      base_year = year,
      base_fpt = fpt
    ) %>%
    select(country, industry, base_year, base_fpt)

  # -------------------------
  # Targets coefs

  target_coefs_fr <- base_targets %>%
    filter(country == "FR") %>%
    mutate(
      target_2050 = 100.0,
      coef_yearly = (target_2050 / base_fpt)^(1 / n_years)
    ) %>%
    select(country, industry, target_2050, coef_yearly)

  targets_raw_data_fr <- base_targets %>%
    merge(target_coefs_fr) %>%
    crossing(years) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      fpt_tgt = base_fpt * (coef_yearly^n)
    ) %>%
    merge(main_aggregates_data) %>%
    mutate(
      tgt_value = round(fpt_tgt / 100 * NVA, digits = 1)
    ) %>%
    select(country, industry, year, tgt_value)

  # -------------------------
  # Targets fpt

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build accounts tgt data --------------------------
    left_join(targets_raw_data_fr) %>%
    left_join(trd_data) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check increasing fpt -----------------------------
    left_join(base_targets) %>%
    left_join(main_aggregates_data) %>%
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, tgt_value / NVA * 100, 0),
      fpt_tgt = pmax(fpt_tgt, base_fpt),
      fpt_tgt = cummax(fpt_tgt),
      tgt_value = fpt_tgt / 100 * NVA
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
      value       = round(value, digits = 3),
      flag        = "",
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
