# La Société Nouvelle

#' TARGETS BUILDER - WAT
#'
#' Note :
#'   Update target function for indic WAT
#'
#' Targets :
#'   - FRA : stability of raw water consumption
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_target_wat <- function(
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

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_wat.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_wat.csv")

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

  base_impacts <- impacts_obs_data %>%
    filter(year == last_year_obs) %>%
    merge(main_aggregates_data) %>%
    mutate(
      base_year = year,
      base_impact = value,
      base_fpt = ifelse(NVA > 0, value / NVA, 0)
    ) %>%
    select(country,industry,base_year,base_impact,base_fpt)

  # -------------------------
  # Targets impacts

  targets_data <- base_impacts %>%
    # add years ----------------------------------------
    slice(rep(1:n(), each = length(years))) %>%
    mutate(year = rep(years, n()/length(years))) %>%
    # build raw impact tgt -----------------------------
    mutate(
      impact_tgt = ifelse(country == 'FR', base_impact, NA)
    ) %>%
    # apply trend for other countries ------------------
    merge(impacts_trd_data) %>%
    mutate(
      impact_tgt = ifelse(country == 'FR', impact_tgt, impacts_trd)
    ) %>%
    # build raw fpt tgt --------------------------------
    merge(main_aggregates_data) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, impact_tgt / NVA, 0)
    ) %>%
    # check decreasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = pmin(fpt_tgt, base_fpt),
      fpt_tgt = cummin(fpt_tgt),
      impact_tgt = fpt_tgt * NVA
    ) %>%
    ungroup() %>%
    # select -------------------------------------------
    select(country,industry,year,impact_tgt)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(targets_data) != size) {
    error_data <<- targets_data
    stop("ERROR - Wrong size for tgt accounts (WAT)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (WAT)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "wat_tgt",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_wat.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
