# La Société Nouvelle

#' TARGETS BUILDER - KNW
#'
#' Note :
#'   Update target function for indic KNW
#'
#' Targets :
#'   - FRA : +0.8 pts increase by 2030, from 2020
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_knw_tgt_accounts <- function(
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

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_knw.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_knw.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    )

  # -------------------------------------------------------------------

  last_year_obs <- max(as.integer(obs_data$year), na.rm = TRUE)

  tgt_years <- (last_year_obs + 1) : 2030
  n_years <- 2030 - tgt_years[1]

  years <- tibble(year = as.character(tgt_years))

  # -------------------------------------------------------------------
  # FIGARO Economic data

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, NVA)

  # -------------------------------------------------------------------

  # -------------------------
  # Starting point

  base_targets <- obs_data %>%
    filter(year == as.character(last_year_obs)) %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA * 100, 0)
    ) %>%
    rename(
      base_year = year,
      base_fpt = fpt
    ) %>%
    select(country,industry,base_year,base_fpt)

  # -------------------------
  # Targets for 2030

  target_coefs <- obs_data_raw %>%
    filter(
      year == "2020",
      country == "FR"
    ) %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA * 100, 0)
    ) %>%
    select(country,industry,fpt) %>%
    merge(main_aggregates_data) %>%
    filter(year %in% c("2020", "2030")) %>%
    pivot_wider(
      names_from = year,
      values_from = NVA,
      names_glue = "nva_{year}"
    ) %>%
    group_by(country) %>%
    summarise(
      fpt_total_2020 = sum(fpt * nva_2020) / sum(nva_2020),
      fpt_total_2030 = sum(fpt * nva_2030) / sum(nva_2030),
      delta = 0.8 - (fpt_total_2030 - fpt_total_2020),
      coef = (fpt_total_2020 + 0.8) / fpt_total_2030
    ) %>%
    mutate(
      coef_yearly = ifelse(country == "FR", coef^(1 / 10), 1.0) # coef annuel sur 10 ans
    ) %>%
    select(country,coef_yearly)

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build raw fpt tgt --------------------------------
    left_join(base_targets) %>%
    left_join(target_coefs) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      fpt_tgt = ifelse(country == "FR", pmin(base_fpt * (coef_yearly^n), 100.0), NA)
    ) %>%
    # build raw impact_tgt -----------------------------
    left_join(main_aggregates_data) %>%
    mutate(
      tgt_value = round(fpt_tgt * NVA / 100, 1)
    ) %>%
    # use trend for other countries --------------------
    left_join(trd_data) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check increasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, tgt_value / NVA * 100, 0),
      fpt_tgt = pmax(fpt_tgt, base_fpt),
      fpt_tgt = cummax(fpt_tgt),
      tgt_value = fpt_tgt * NVA / 100
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
    stop("ERROR - Wrong size for tgt accounts (KNW)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    print(targets_data %>% filter(is.na(value)) %>% as_tibble())
    stop("ERROR - NA values in tgt accounts (KNW)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "knw_tgt",
      value       = round(value, digits = 3),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_knw.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
