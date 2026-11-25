# La Société Nouvelle

#' TARGETS BUILDER - GHG
#'
#' Note :
#'   Update target function for indic GHG
#'
#' Targets :
#'   - FRA : SNBC 3, last observation scaled by its sector trajectory
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, unit, flag, lastupdate

build_ghg_tgt_accounts <- function(
  verbose = FALSE,
  write_output = TRUE,
  horizon = 2040
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

  # SNBC 3 trajectories by sector (MtCO2e), net of fossil CCS, see snbc3_extract.R
  snbc3_correspondence_table <- read_delim(
      "tgt_accounts/ghg/snbc3_correspondence_table.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, snbc_sector)

  snbc3_data <- read_delim(
      "tgt_accounts/ghg/snbc3_detail.csv",
      delim = ";",
      show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")
    ) %>%
    inner_join(
      read_delim(
        "tgt_accounts/ghg/snbc3_sector_lines.csv",
        delim = ";",
        show_col_types = FALSE,
        locale = locale(encoding = "UTF-8")
      ),
      by = c("block", "line")
    ) %>%
    group_by(snbc_sector, year) %>%
    summarise(emissions = sum(value), .groups = "drop") %>%
    mutate(year = as.character(year))

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

  tgt_years <- (last_year_obs + 1) : horizon

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

  # -------------------------
  # Start point (base)

  last_impacts_obs <- obs_data %>%
    filter(year == last_year_obs) %>%
    merge(main_aggregates_data) %>%
    mutate(
      last_impact_obs = value,
      last_fpt_obs = ifelse(NVA > 0, value / NVA, 0)
    ) %>%
    select(country, industry, last_impact_obs, last_fpt_obs)

  # -------------------------
  # Targets

  # SNBC 3 trajectory of each sector relative to the last observed year
  snbc3_base <- snbc3_data %>%
    filter(year == as.character(last_year_obs)) %>%
    select(snbc_sector, base_emissions = emissions)

  if (nrow(snbc3_base) != n_distinct(snbc3_correspondence_table$snbc_sector)) {
    stop("ERROR - SNBC 3 trajectories missing for year ", last_year_obs)
  }

  target_coefs <- snbc3_correspondence_table %>%
    inner_join(snbc3_data, by = "snbc_sector", relationship = "many-to-many") %>%
    inner_join(snbc3_base, by = "snbc_sector") %>%
    filter(year %in% years$year) %>%
    mutate(
      coef_snbc = if_else(base_emissions > 0, emissions / base_emissions, 1)
    ) %>%
    select(industry, year, coef_snbc)

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # SNBC 3 for France, trend elsewhere
    left_join(last_impacts_obs, by = c("country", "industry")) %>%
    left_join(target_coefs, by = c("industry", "year")) %>%
    left_join(trd_data, by = c("country", "industry", "year")) %>%
    # check decreasing fpt, other countries ------------
    left_join(main_aggregates_data, by = c("country", "industry", "year")) %>%
    mutate(fpt_tgt = ifelse(NVA > 0, trd_value / NVA, 0)) %>%
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = pmin(fpt_tgt, last_fpt_obs),
      fpt_tgt = cummin(fpt_tgt)
    ) %>%
    ungroup() %>%
    mutate(
      value = ifelse(country == "FR", last_impact_obs * coef_snbc, ifelse(NVA > 0, fpt_tgt * NVA, 0))
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
      unit        = unique(obs_data_raw$unit),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_tgt_ghg.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
