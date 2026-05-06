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
  years = 2010:2020,
  do_clean_outliers = TRUE,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build SOC accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")

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

  base_url_urssaf_data = "https://open.urssaf.fr/api/explore/v2.1/catalog/datasets/nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"

  urssaf_file_name <- "nombre-etab-effectifs-salaries-et-masse-salariale-ess-france-x-na88.csv"
  urssaf_file_path <- file.path(download_dir, urssaf_file_name)

  if (!file.exists(urssaf_file_path) | !use_temp_data)
  {
    # URSSAF data (2023) : load and compute SOC share based on salaried mass
    urssaf_raw_data <- read.csv(
      base_url_urssaf_data,
      sep = ";",
      check.names = TRUE
    )

    write.csv(urssaf_raw_data, urssaf_file_path, row.names = FALSE)
  }

  urssaf_raw_data <- read.csv(urssaf_file_path)

  urssaf_raw_data <- urssaf_raw_data %>%
    rename(
      champ_ess = X.Champ.ESS..,
      famille_ess = Famille.ESS,
      grand_secteur_activite = Grand.secteur.d.activité,
      secteur_na88 = Secteur.NA88,
      annee = Année,
      nombre_etablissements = Nombre.d.établissements,
      effectifs_salaries_moyens = Effectifs.salariés.moyens,
      masse_salariale_brute = Masse.salariale..brute.
    ) %>%
    mutate(
      secteur_na88 = sub(" .*", "", secteur_na88),
      champ_ess = champ_ess == "OUI"
    ) %>%
    select(annee, secteur_na88, champ_ess, masse_salariale_brute)

  urssaf_data <- urssaf_raw_data %>%
    filter(annee %in% years$year) %>%
    group_by(annee, secteur_na88) %>%
    summarise(
      masse_salariale = sum(masse_salariale_brute, na.rm = TRUE),
      masse_salariale_ess = sum(if_else(champ_ess, masse_salariale_brute, 0), na.rm = TRUE),
      .groups = "drop"
    )

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
      rate_ess = if_else(masse_salariale > 0, masse_salariale_ess / masse_salariale, 6) # coef entre 0 et 1
    ) %>%
    arrange(year, industry)

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_soc_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(main_aggregates_data) %>%
    left_join(soc_accounts_fr) %>%
    mutate(
      value = if_else(country == "FR", NVA * rate_ess, 0),
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

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <- figaro_soc_accounts %>%
    mutate(
      serie_id    = "soc_obs",
      value       = round(value, digits = 0), # *100 for percentage
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  # -------------------------------------------------------------------
  if (verbose) print(formatted_data %>% as_tibble())

  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_soc.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
