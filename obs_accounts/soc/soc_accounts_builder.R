# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
# Non-financial accounts builder for social purpose organisations (SOC)
#
# Main sources :
#   - URSSAF
#
# Output data
#   Accounts are in millions euros (CPMEUR)
#
# Missing values filled by proxy using industry and country similarity.
#
# build_soc_obs_accounts()

# /!\ N80T82 -> link to 81/82 not 80 / R90T92 link to 90 & 91, not 92

build_soc_obs_accounts <- function(
  years = 2010:2024,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build SOC accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/soc/soc_accounts_sources.R", encoding = "UTF-8-BOM")

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
    select(industry)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      country = code
    ) %>%
    select(country)

  table_passage_urssaf_data <- read_delim(
      "obs_accounts/soc/table_passage_urssaf.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      industry = figaro_industry
    ) %>%
    select(industry, secteur_na88)

  # -------------------------------------------------------------------
  if (verbose) cat("Metadata loaded\n")

  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # URSSAF data

  urssaf_data <- load_urssaf_soc_source(years$year, use_temp_data, verbose)

  # -------------------------------------------------------------------
  # FIGARO accounts

  soc_accounts_fr <- urssaf_data %>%
    merge(table_passage_urssaf_data) %>%
    group_by(annee, industry) %>%
    summarise(
      masse_salariale = sum(masse_salariale, na.rm = TRUE),
      masse_salariale_ess = sum(masse_salariale_ess, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      year = as.character(annee),
      rate_ess = if_else(masse_salariale > 0, masse_salariale_ess / masse_salariale, 0) # coef entre 0 et 1
    ) %>%
    arrange(year, industry)

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_soc_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      main_aggregates_data,
      by = c("year", "country", "industry")
    ) %>%
    left_join(soc_accounts_fr) %>%
    mutate(
      value = if_else(country == "FR" & NVA > 0, NVA * rate_ess, 0),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_soc_accounts) != size) {
    error_data <<- figaro_soc_accounts
    stop("ERROR - Wrong size for obs accounts (SOC)")
  } else if (any(is.na(figaro_soc_accounts$value))) {
    error_data <<- figaro_soc_accounts
    stop("ERROR - NA values in obs accounts (SOC)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_soc_accounts %>%
    mutate(
      serie_id    = "soc_obs",
      value       = round(value, digits = 3),
      unit        = "MEUR",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_soc.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
