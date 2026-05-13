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

  if (verbose) cat("Loading metadata...\n")

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

  if (verbose) cat("Metadata loaded\n")

  # -------------------------------------------------------------------
  # OBS Accounts

  if (verbose) cat("Loading obs accounts data...\n")

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_wat.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  if (verbose) cat("Loading trd accounts data...\n")

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_wat.csv")

  trd_data_raw <- read.csv(trd_accounts_path)

  trd_data <- trd_data_raw %>%
    rename(
      trd_value = value,
      trd_flag = flag
    )

  if (verbose) cat("trd accounts data loaded\n")

  # -------------------------------------------------------------------

  last_year_obs <- max(as.integer(obs_data_raw$year), na.rm = TRUE)

  tgt_years <- last_year_obs : 2030
  n_years <- 2030 - tgt_years[1]

  years <- tibble(year = as.character(tgt_years))

  # -------------------------------------------------------------------
  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------

  # -------------------------
  # Start point (base)

  base_impacts <- obs_data_raw %>%
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

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    filter(year != last_year_obs) %>%
    # build raw fpt tgt --------------------------------
    merge(base_impacts) %>%
    mutate(
      impact_tgt = ifelse(country == "FR", base_impact, NA)
    ) %>%
    # apply trend for other countries ------------------
    merge(trd_data) %>%
    mutate(
      impact_tgt = ifelse(country == "FR", impact_tgt, trd_value)
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
    rename(
      value = impact_tgt
    ) %>%
    select(country,industry,year,value)

  # Check
  size <- (nrow(years) - 1)*nrow(figaro_industries)*nrow(figaro_countries)
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
      flag        = "",
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
