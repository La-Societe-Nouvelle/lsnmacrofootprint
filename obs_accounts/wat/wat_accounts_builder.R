# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Environmental accounts builder for water consumption (WAT)
#'
#' Main sources :
#'   - Water use and supply (OECD)
#'   - Network losses (SISPEA, rapport 2024 : rendement 79,2 %)
#'       Source : https://www.eaufrance.fr/sites/default/files/documents/rapport_sispea_2024_vf.pdf
#'
#' Consumption = water not returned unaltered to its source : own withdrawals
#' are consumed (coefficient 1), except power plant cooling (D35).
#' coefficients_consommation.csv (France Strategie, 2024) is kept as an archive
#' and no longer read.
#'
#' Water abstracted for public distribution (OECD section E) is allocated :
#'   - network losses -> E36
#'   - share supplied to economic activities -> industries, pro rata their
#'     purchases of domestic E36 (FIGARO intermediate inputs, E36 excluded)
#'   - remainder (households) -> outside industry accounts
#'
#' Countries absent from OECD data over the whole period use FAO AQUASTAT (via World Bank API)
#' withdrawals by use (agriculture -> A, industry x non-energy share -> B, C, F,
#' municipal -> distribution network) and Ember thermal generation for D35.
#'
#' Output data
#'   Accounts are in thousands of m3 (to match MEUR)
#'
#' Missing values filled by proxy using industry and country similarity.#
#'
#' build_wat_obs_accounts()

build_wat_obs_accounts <- function(
  years = 2010:2022, # OECD available since 1990
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build WAT accounts for observed data")

  # Reseau de distribution (parts des prelevements de la section E)
  network_loss_rate <- 0.208 # pertes : SISPEA, rapport 2024 (rendement 79,2 %)
  network_activities_share <- 0.20 # activites economiques : France Strategie (2024), note n° 136

  # Refroidissement des centrales (D35) : ~12 % des 4,1 Md m3 consommes en France
  # (SDES/MTE, 2010-2019) rapportes aux prelevements ; 97 % restitues (EDF).
  # Valeur francaise appliquee a tous les pays. Autres prelevements propres : 1.
  d35_consumption_coef <- 0.03

  # Pays-annees sans donnee OCDE (AQUASTAT + Ember)
  # - consommation D35 par MWh thermique : valeur francaise (0,03 x 34 m3
  #   preleves / MWh thermique, OCDE / Ember 2015-2019)
  # - part hors energie (B, C, F) des prelevements industriels AQUASTAT, qui
  #   incluent le refroidissement des centrales : mediane des pays OCDE (2015-2019)
  d35_consumption_per_mwh <- 1.0 # m3 / MWh
  aquastat_non_energy_industry_share <- 0.39

  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/wat/wat_accounts_sources.R", encoding = "UTF-8-BOM")
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

  table_passage_ocde_data <- read_delim(
      "obs_accounts/wat/table_passage_ocde.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      industry = figaro_industry
    ) %>%
    select(industry, oecd_industry)

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

  # Achats de E36 national par les branches (hors E36 : ventes entre distributeurs)
  e36_purchases_data <- map_dfr(years$year, function(year_i) {
    load_local_figaro_intermediate_inputs(year_i) %>%
      filter(
        resource_industry == "E36",
        use_country == resource_country,
        use_industry != "E36"
      ) %>%
      transmute(
        year = as.character(year_i),
        country = use_country,
        industry = use_industry,
        e36_purchases = pmax(value, 0)
      )
  })

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # Fetch OECD data

  if (verbose) cat("Loading OECD data...\n")

  # Water accounts - supply and use

  oecd_data <- load_oecd_wat_source(years$year, use_temp_data, verbose) %>%
    filter(year %in% years$year, !is.na(water_withdrawal))

  if (verbose) cat("OECD data loaded\n")

  # AQUASTAT et Ember, pour les pays absents de l'OCDE sur toute la periode
  # (les series OCDE partielles sont completees par interpolation/extrapolation)

  oecd_countries <- oecd_data %>%
    distinct(country)

  aquastat_data <- load_aquastat_wat_source(years$year, use_temp_data, verbose) %>%
    anti_join(oecd_countries, by = "country")

  thermal_data <- load_ember_thermal_wat_source(years$year, use_temp_data, verbose)

  # -------------------------------------------------------------------
  # Building WAT impact vector

  if (verbose) cat("Building FIGARO accounts...\n")

  # use VA to split impacts between FIGARO industries
  va_distribution_oecd_industries <- table_passage_ocde_data %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, oecd_industry) %>%
    mutate(
      share_oecd_industry = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, oecd_industry, share_oecd_industry)

  # -------------------------
  # Prelevements propres (hors distribution publique) : consommes, sauf la part
  # restituee sans alteration par le refroidissement des centrales (D35)

  own_wat_accounts <- oecd_data %>%
    filter(oecd_industry != "E") %>%
    merge(va_distribution_oecd_industries) %>%
    mutate(
      coef_consumption = if_else(industry == "D35", d35_consumption_coef, 1),
      own_value = water_withdrawal * share_oecd_industry * coef_consumption
    ) %>%
    filter(own_value >= 0) %>%
    select(year, country, industry, own_value)

  # -------------------------
  # Prelevements propres des pays-annees AQUASTAT
  # agriculture -> A ; industrie x part hors energie -> B, C, F ; D35 a partir
  # de la production thermique ; services (GTU) : 0, les trois usages AQUASTAT
  # couvrant l'ensemble des prelevements.

  table_passage_aquastat_data <- table_passage_ocde_data %>%
    mutate(oecd_industry = if_else(oecd_industry %in% c("B", "C", "F"), "BCF", oecd_industry))

  va_distribution_aquastat <- table_passage_aquastat_data %>%
    filter(oecd_industry %in% c("A", "BCF")) %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, oecd_industry) %>%
    mutate(share_oecd_industry = NVA / sum(NVA, na.rm = TRUE)) %>%
    ungroup() %>%
    select(year, country, industry, oecd_industry, share_oecd_industry)

  own_aquastat_accounts <- aquastat_data %>%
    transmute(year, country, A = agriculture, BCF = industry * aquastat_non_energy_industry_share) %>%
    pivot_longer(c(A, BCF), names_to = "oecd_industry", values_to = "water_withdrawal") %>%
    merge(va_distribution_aquastat) %>%
    transmute(year, country, industry, own_value = water_withdrawal * share_oecd_industry) %>%
    filter(own_value >= 0) %>%
    bind_rows(
      aquastat_data %>%
        inner_join(thermal_data, by = c("year", "country")) %>%
        transmute(year, country, industry = "D35", own_value = thermal_twh * 1e3 * d35_consumption_per_mwh), # TWh x m3/MWh -> milliers de m3
      aquastat_data %>%
        distinct(year, country) %>%
        crossing(table_passage_ocde_data %>% filter(oecd_industry == "GTU") %>% select(industry)) %>%
        mutate(own_value = 0)
    )

  own_wat_accounts <- bind_rows(own_wat_accounts, own_aquastat_accounts)

  # -------------------------
  # Reseau de distribution (section E OCDE ou usages municipaux AQUASTAT) :
  # pertes -> E36, part des activites repartie au prorata des achats de E36
  # national, reste (menages) hors comptes

  network_withdrawal_data <- oecd_data %>%
    filter(oecd_industry == "E") %>%
    select(year, country, network_withdrawal = water_withdrawal) %>%
    bind_rows(
      aquastat_data %>%
        filter(!is.na(municipal)) %>%
        transmute(year, country, network_withdrawal = municipal)
    )

  network_wat_accounts <- e36_purchases_data %>%
    group_by(year, country) %>%
    mutate(
      total_e36_purchases = sum(e36_purchases),
      share_e36_purchases = if_else(total_e36_purchases > 0, e36_purchases / total_e36_purchases, 0)
    ) %>%
    ungroup() %>%
    inner_join(network_withdrawal_data, by = c("year", "country")) %>%
    transmute(
      year, country, industry,
      network_value = network_withdrawal * network_activities_share * share_e36_purchases
    ) %>%
    bind_rows(
      network_withdrawal_data %>%
        transmute(year, country, industry = "E36", network_value = network_withdrawal * network_loss_rate)
    )

  # -------------------------
  # Accounts data
  # Valeur manquante si les prelevements propres de la section ou les
  # prelevements de distribution du pays manquent (completee ensuite).

  figaro_wat_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(table_passage_ocde_data, by = "industry") %>%
    left_join(own_wat_accounts, by = c("year", "country", "industry")) %>%
    left_join(network_wat_accounts, by = c("year", "country", "industry")) %>%
    left_join(
      network_withdrawal_data %>% transmute(year, country, has_network_data = TRUE),
      by = c("year", "country")
    ) %>%
    left_join(
      aquastat_data %>% transmute(year, country, from_aquastat = TRUE),
      by = c("year", "country")
    ) %>%
    mutate(
      own_value = if_else(is.na(oecd_industry) | oecd_industry == "E", 0, own_value),
      network_value = if_else(is.na(has_network_data), NA_real_, coalesce(network_value, 0)),
      value = own_value + network_value,
      flag = if_else(is.na(from_aquastat), "", "e") # AQUASTAT : estimation par cles
    ) %>%
    select(year, country, industry, value, flag)

  if (verbose) cat("Completing with similarity...\n")

  # Complete with similarity
  figaro_wat_accounts <- figaro_wat_accounts_raw %>%
    complete_series(min_value = 0) %>%
    proxy_missing_value_by_similarity(., "WAT", min_value = 0) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_wat_accounts) != size) {
    error_data <<- figaro_wat_accounts
    stop("ERROR - Wrong size for obs accounts (WAT)")
  } else if (any(is.na(figaro_wat_accounts$value))) {
    error_data <<- figaro_wat_accounts
    stop("ERROR - NA values in obs accounts (WAT)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_wat_accounts %>%
    mutate(
      serie_id    = "wat_obs",
      value       = round(value, digits = 0),
      unit        = "THS_M3",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  if (write_output) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    accounts_data_path  <- file.path(output_dir, "accounts_obs_wat.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
