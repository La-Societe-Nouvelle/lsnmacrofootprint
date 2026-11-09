# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Environmental FIGARO accounts builder for raw material extraction (MAT)
#'
#' Main sources :
#'   - UNEP material‑flow data
#'
#' Output data
#'   Accounts are in tonnes
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_mat_obs_accounts()

build_mat_obs_accounts <- function(
  years = 2010:2024,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build MAT accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/mat/mat_accounts_sources.R", encoding = "UTF-8-BOM")
  source("utils/utils_imputations.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  years <- tibble(year = as.character(years))

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(
      industry = code
    ) %>%
    select(industry, sector)

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
  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, PRD)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # UNEP data

  unep_data <- load_unep_mat_source(years$year, use_temp_data, verbose)

  # -------------------------------------------------------------------
  # Building MAT impact vector & Fill missing values by similarity

  if (verbose) cat("Building FIGARO accounts...\n")

  # direct extraction by sector
  sector_extraction <- unep_data %>%
    mutate(
      sector = case_when(
        category %in% c("Biomass") ~ "A",
        category %in% c("Fossil fuels", "Metal ores", "Non-metallic minerals") ~ "B"
      )
    ) %>%
    group_by(year, country, sector) %>%
    summarise(
      value = sum(value, na.rm = TRUE), .groups = "drop"
    ) %>%
    filter(value >= 0) %>%
    select(year, country, sector, value)

  # use production to split impacts between FIGARO industries
  production_distribution_sectors <- main_aggregates_data %>%
    merge(figaro_industries) %>%
    group_by(year, country, sector) %>%
    mutate(
      share_sector = PRD / sum(PRD, na.rm = TRUE),
    ) %>%
    ungroup() %>%
    select(year, country, industry, share_sector)

  # -------------------------
  # Accounts data

  raw_mat_accounts <- sector_extraction %>%
    merge(figaro_industries) %>% # by sector
    merge(production_distribution_sectors) %>% # by year, country, industry
    mutate(
      value = value * share_sector,
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  figaro_mat_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      raw_mat_accounts,
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      value = if_else(sector %in% c("A","B"), value, 0)
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_mat_accounts <- figaro_mat_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "MAT", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_mat_accounts) != size) {
    error_data <<- figaro_mat_accounts
    stop("ERROR - Wrong size for obs accounts (MAT)")
  } else if (any(is.na(figaro_mat_accounts$value))) {
    error_data <<- figaro_mat_accounts
    stop("ERROR - NA values in obs accounts (MAT)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_mat_accounts %>%
    mutate(
      serie_id    = "mat_obs",
      value       = round(value, digits = 0),
      unit        = "T",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_mat.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
