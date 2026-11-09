# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for craft production (ART)
#'
#' Main sources :
#'   - FIGARO main aggregates data
#'   - SIRENE data (historic)
#'
#' Nomenclature NAFA : "https://apiopendata.artisanat.fr/nafa"
#'
#' Output data
#'   Accounts are in millions of euros (current price)
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_art_obs_accounts()

build_art_obs_accounts <- function(
  years = 2010:2024,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build ART accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/art/art_accounts_sources.R", encoding = "UTF-8-BOM")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  years <- tibble(year = as.character(years))

  insee_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    mutate(
      code_ape = gsub("\\.", "", code)
    ) %>%
    select(code_ape, industry)

  figaro_industries = read_delim(
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

  nomenclature_nafa <- read_delim(
      "obs_accounts/art/nomenclature_artisanat.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(
      str_ends(code_nafa, "P")
    ) %>%
    rename(code_ape = code_naf) %>%
    select(code_ape, code_nafa)

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
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # SIRENE data - StockEtablissement

  if (verbose) cat("Loading SIRENE data - StockEtablissement...\n")

  etablissements_artisanaux <- load_sirene_art_stock_source(years$year, use_temp_data, verbose)

  if (verbose) cat("SIRENE data - StockEtablissement loaded\n")

  # -------------------------------------------------------------------
  # SIRENE data - StockEtablissementHistorique

  if (verbose) cat("Loading SIRENE data - StockEtablissementHistorique...\n")

  etablissements <- load_sirene_art_history_source(years$year, use_temp_data, verbose)

  if (verbose) cat("SIRENE data - StockEtablissementHistorique loaded\n")

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  # Part des établissements artisanaux par code APE (situation actuelle)
  crafts_rates_fr_last_year <- etablissements_artisanaux %>%
    mutate(
      crafts_rate_fr = n_ets_artisanaux / n_ets
    ) %>%
    select(activitePrincipaleEtablissement, crafts_rate_fr)

  print(crafts_rates_fr_last_year %>% as_tibble())

  # Part des établissements artisanaux par industry et par an (prolongement de la situation actuelle)
  crafts_rates_fr <- etablissements %>%
    left_join(crafts_rates_fr_last_year) %>% # by activitePrincipaleEtablissement
    mutate(
      code_ape = gsub("\\.", "", activitePrincipaleEtablissement),
      crafts_rate_fr = if_else(!is.na(crafts_rate_fr), crafts_rate_fr, 0)
    ) %>%
    merge(insee_nace_niv5) %>%
    group_by(year,industry) %>%
    summarise(
      crafts_rate_fr = sum(n_ets * crafts_rate_fr, na.rm = TRUE) / sum(n_ets, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      country = "FR"
    ) %>%
    select(year, industry, country, crafts_rate_fr)

  # -------------------------
  # FIGARO Accounts

  figaro_art_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      main_aggregates_data,
      by = c("year", "country", "industry")
    ) %>%
    left_join(
      crafts_rates_fr,
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      value = case_when(
        country == "FR" & NVA > 0 ~ round(NVA * crafts_rate_fr, digits = 3),
        TRUE                      ~ 0
      ),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_art_accounts) != size) {
    error_data <<- figaro_art_accounts
    stop("ERROR - Wrong size for obs accounts (MAT)")
  } else if (any(is.na(figaro_art_accounts$value))) {
    error_data <<- figaro_art_accounts
    stop("ERROR - NA values in obs accounts (MAT)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_art_accounts %>%
    mutate(
      serie_id    = "art_obs",
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
    accounts_data_path  <- file.path(output_dir, "accounts_obs_art.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}

# ----------------------------------------------------------------------------------------------------
# NAFA download

# nomenclature_nafa <- fromJSON("https://apiopendata.artisanat.fr/nafa") %>%
#   select(code_naf, visu_nafa, libelle_nafa) %>%
#   distinct() %>%
#   rename(code_nafa = visu_nafa) %>%
#   arrange(code_naf, visu_nafa)
#
# write.table(
#   nomenclature_nafa,
#   file = file.path("obs_accounts", "art", "nomenclature_artisanat.csv"),
#   sep = ";",
#   row.names = FALSE,
#   col.names = TRUE,
#   quote = TRUE,
#   fileEncoding = "UTF-8"
# )
