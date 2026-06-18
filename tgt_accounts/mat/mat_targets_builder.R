# La Société Nouvelle

#' TARGETS BUILDER - MAT
#'
#' Note :
#'   Update target function for indic MAT
#'
#' Targets :
#'   - FRA : 30% increase in GDP per domestic material consumption ratio between 2010 and 2030
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_mat_tgt_accounts <- function(
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

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_mat.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_mat.csv")

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

  main_aggregates_years <- c(years$year, last_year_obs, "2010")

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
    filter(year == last_year_obs) %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA, 0)
    ) %>%
    rename(base_year = year,
           base_fpt = fpt) %>%
    select(country,industry,base_year,base_fpt)

  # -------------------------
  # Targets for 2030

  targets_2030 <- obs_data %>%
    filter(year == "2010") %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA, 0)
    ) %>%
    mutate(
      tgt_2030 = ifelse(country == "FR", fpt / 1.3, NA)
    ) %>%
    select(country, industry, tgt_2030)

  target_coefs <- base_targets %>%
    merge(targets_2030) %>%
    mutate(
      coef_yearly = ifelse(is.na(tgt_2030) | base_fpt <= tgt_2030, 1.0, (tgt_2030 / base_fpt)^(1 / n_years)) # pas de réduction si objectif atteint
    ) %>%
    select(country, industry, tgt_2030, coef_yearly)

  # -------------------------
  # Targets

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build raw fpt tgt --------------------------------
    left_join(base_targets) %>%
    left_join(target_coefs) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      fpt_tgt = base_fpt * (coef_yearly^n)
    ) %>%
    # build raw impact tgt -----------------------------
    left_join(main_aggregates_data) %>%
    mutate(
      tgt_value = round(fpt_tgt * NVA, 1)
    ) %>%
    # apply trend for other countries ------------------
    left_join(trd_data, by = c("country", "industry", "year")) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check decreasing fpt -----------------------------
    arrange(year) %>%
    group_by(country, industry) %>%
    mutate(
      fpt_tgt = ifelse(NVA > 0, tgt_value / NVA, 0),
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
    stop("ERROR - Wrong size for tgt accounts (MAT)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (MAT)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "mat_tgt",
      value       = round(value, digits = 0),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_mat.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
