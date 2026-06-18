# La Société Nouvelle

#' TARGETS BUILDER - WAS
#'
#' Note :
#'   Update target function for indic WAS
#'
#' Targets :
#'   - FRA : French National Waste Prevention Plan (PNPD).
#'       -> Reducing waste from economic activities per unit of value added by 5%, particularly in construction and public works, by 2030 compared to 2010
#'       Source: https://www.legifrance.gouv.fr/codes/article_lc/LEGIARTI000043974936/ AND "plan national de prévention des déchets (2021-2027)
#'   - other countries : trend
#'
#' output columns: serie_id, country, industry, year, value, flag, lastupdate

build_was_tgt_accounts <- function(
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

  obs_accounts_path  <- file.path(output_dir, "accounts_obs_was.csv")

  obs_data_raw <- read.csv(obs_accounts_path)

  obs_data <- obs_data_raw %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("obs data loaded\n")

  # -------------------------------------------------------------------
  # TRD Accounts

  trd_accounts_path  <- file.path(output_dir, "accounts_trd_was.csv")

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
    select(country, industry, base_year, base_fpt)

  # -------------------------
  # Targets for 2030

  targets_2030_fr <- obs_data %>%
    filter(
      country == "FR",
      year == "2010"
    ) %>%
    merge(main_aggregates_data) %>%
    mutate(
      fpt = if_else(NVA > 0, value / NVA, 0)
    ) %>%
    mutate(
      tgt_2030 = value * 0.95
    ) %>%
    select(country, industry, tgt_2030)

  target_coefs_fr <- base_targets %>%
    merge(targets_2030_fr) %>%
    mutate(
      coef_yearly = ifelse(base_fpt <= tgt_2030, 1.0, (tgt_2030 / base_fpt)^(1 / n_years)) # pas d'augmentation si taux de contribution atteint
    ) %>%
    select(country, industry, tgt_2030, coef_yearly)

  targets_raw_data_fr <- base_targets %>%
    merge(target_coefs_fr) %>%
    crossing(years) %>%
    mutate(
      n = as.integer(year) - as.integer(base_year),
      fpt_tgt = base_fpt * (coef_yearly^n)
    ) %>%
    merge(main_aggregates_data) %>%
    mutate(
      tgt_value = fpt_tgt * NVA
    ) %>%
    select(country, industry, year, tgt_value)

  # -------------------------
  # Targets

  targets_data <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    # build accounts tgt data --------------------------
    left_join(targets_raw_data_fr) %>%
    left_join(trd_data) %>%
    mutate(
      tgt_value = ifelse(country == "FR", tgt_value, trd_value)
    ) %>%
    # check decreasing fpt -----------------------------
    left_join(base_targets) %>%
    left_join(main_aggregates_data) %>%
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
    stop("ERROR - Wrong size for tgt accounts (WAS)")
  } else if (any(is.na(targets_data$value))) {
    error_data <<- targets_data
    stop("ERROR - NA values in tgt accounts (WAS)")
  }

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- targets_data %>%
    mutate(
      serie_id    = "was_tgt",
      value       = round(value, digits = 0),
      flag        = "",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_tgt_was.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
