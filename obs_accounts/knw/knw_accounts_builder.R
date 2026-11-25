# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for training/research contribution (KNW)
#'
#' Main sources :
#'   -
#'
#' Output data
#'   Accounts are in millions of euros (CP_MEUR)
#'
#' ANBERD: 2010-2021 (2022 limited data)
#' TRNG: 2015-2020
#' STAN_2025: 2015-2022
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_knw_obs_accounts()

build_knw_obs_accounts <- function(
  years = 2015:2020,
  use_temp_data = TRUE,
  verbose = FALSE,
  write_output = TRUE
) {
  if (verbose) message("Build KNW accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/knw/knw_accounts_sources.R", encoding = "UTF-8-BOM")
  source("utils/utils_imputations.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_monetary_conversion.R")
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

  anberd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/anberd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, anberd_activity)

  eurostat_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  eurostat_berd_correspondence_table_activity <- read_delim(
      "obs_accounts/knw/eurostat_berd_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2, nace_r2_section)

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
  # ANBERD data (R&D Expenditure)

  if (verbose) cat("Loading ANBERD data...\n")

  # Monnaie nationale -> EUR au taux de change de marche de chaque pays,
  # comme les agregats FIGARO (pays FIGARO uniquement).
  anberd_data_ncu <- load_anberd_knw_source(years$year, use_temp_data, verbose) %>%
    filter(country %in% figaro_countries$country)

  ncu_eur <- from_national_currencies_to_euro(
      years$year,
      unique(anberd_data_ncu$REF_AREA),
      cache_key = "OECD_ANBERD_CURRENCIES",
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    rename(REF_AREA = area, ncu_eur = coef)
  # ncu_eur = coef 1 unite de monnaie nationale x ncu_eur -> 1 €

  anberd_data <- anberd_data_ncu %>%
    merge(ncu_eur, by = c("year", "REF_AREA")) %>%
    mutate(
      value = round(value * ncu_eur, digits = 0),
      unit = "MEUR"
    ) %>%
    select(year, country, anberd_activity, value, unit) %>%
    arrange(year, country, anberd_activity)

  if (verbose) cat("ANBERD data loaded\n")

  # -------------------------------------------------------------------
  # EUROSTAT data (TRNG CVT)

  if (verbose) cat("Loading EUROSTAT data...\n")

  trng_cvt_data <- load_training_knw_source(years$year, use_temp_data, verbose)

  if (verbose) print("EUROSTAT data loaded\n")

  # -------------------------------------------------------------------
  # OCDE STAN data

  if (verbose) cat("Loading OECD STAN data...")

  stan_data <- load_stan_knw_source(years$year, use_temp_data, verbose)

  if (verbose) cat("OECD STAN data loaded\n")

  # -------------------------------------------------------------------
  # EUROSTAT data (BERD by type of expenditure)

  if (verbose) cat("Loading EUROSTAT BERD labour costs...\n")

  berd_labour_cost_data <- load_berd_labour_cost_knw_source(years$year, use_temp_data, verbose)

  if (verbose) cat("EUROSTAT BERD labour costs loaded\n")

  # -------------------------
  # Share of labour costs in R&D expenditure (Eurostat BERD)
  # - interpolation lineaire entre les annees publiees, derniere (ou premiere)
  #   valeur reconduite hors de la periode couverte ;
  # - priorite : activite Eurostat la plus fine, puis section, puis total national ;
  # - a defaut, mediane annuelle des totaux nationaux disponibles (flag "e").

  target_years <- as.integer(years$year)

  berd_labour_cost_shares <- berd_labour_cost_data %>%
    mutate(year = as.integer(year)) %>%
    group_by(country, nace_r2) %>%
    group_modify(\(serie, key) {
      serie <- serie[order(serie$year), ]
      values <- if (nrow(serie) == 1L) {
        rep(serie$labour_cost_share, length(target_years))
      } else {
        approx(serie$year, serie$labour_cost_share, xout = target_years, rule = 2)$y
      }
      tibble(year = as.character(target_years), labour_cost_share = values)
    }) %>%
    ungroup()

  national_labour_cost_shares <- berd_labour_cost_shares %>%
    filter(nace_r2 == "TOTAL") %>%
    select(year, country, share_total = labour_cost_share)

  median_labour_cost_shares <- national_labour_cost_shares %>%
    group_by(year) %>%
    summarise(share_median = median(share_total, na.rm = TRUE), .groups = "drop")

  rd_labour_cost_shares <- figaro_countries %>%
    crossing(eurostat_berd_correspondence_table_activity) %>%
    crossing(years) %>%
    left_join(
      berd_labour_cost_shares %>% rename(share_fine = labour_cost_share),
      by = c("year", "country", "nace_r2")
    ) %>%
    left_join(
      berd_labour_cost_shares %>% rename(nace_r2_section = nace_r2, share_section = labour_cost_share),
      by = c("year", "country", "nace_r2_section")
    ) %>%
    left_join(national_labour_cost_shares, by = c("year", "country")) %>%
    left_join(median_labour_cost_shares, by = "year") %>%
    mutate(
      labour_cost_share = coalesce(share_fine, share_section, share_total, share_median),
      labour_cost_flag = if_else(
        is.na(share_fine) & is.na(share_section) & is.na(share_total), "e", ""
      )
    ) %>%
    select(year, country, industry, labour_cost_share, labour_cost_flag)

  # -------------------------------------------------------------------
  # Building KNW impact vector

  if (verbose) cat("Building FIGARO accounts...\n")

  # /!\ values in percentage (rate) to build accounts

  # -------------------------
  # Research contribution

  # R&D accounts (ANBERD)

  # use VA to split contributions between FIGARO industries
  va_distribution_anberd_activities <- anberd_correspondence_table_activity %>%
    merge(main_aggregates_data) %>%
    group_by(year, country, anberd_activity) %>%
    mutate(
      share_anberd_activity = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, anberd_activity, share_anberd_activity)

  # Seuls les frais de personnel de R&D sont retenus (part de la VA consacree
  # a la R&D) : autres depenses courantes et investissements exclus.
  research_contributions_raw <- anberd_data %>%
    merge(va_distribution_anberd_activities) %>%
    merge(main_aggregates_data) %>%
    merge(rd_labour_cost_shares, by = c("year", "country", "industry")) %>%
    mutate(
      value = round(value * labour_cost_share * share_anberd_activity / NVA, digits = 6), # contribution (rate)
      flag = labour_cost_flag # "e" si part des frais de personnel estimee (mediane)
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  research_contributions <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      research_contributions_raw,
      by = c("year", "country", "industry")
    ) %>%
    complete_series(min_value = 0, max_value = 100) %>%
    proxy_missing_value_by_similarity(., "KNW", min_value = 0, max_value = 100) %>%
    rename(
      research_value = value,
      research_flag = flag
    ) %>%
    select(year, country, industry, research_value, research_flag) %>%
    arrange(year, country, industry)

  # -------------------------
  # Training contribution

  # Share of total labor cost in net value added (STAN)

  shares_labor_cost <- stan_data %>%
    pivot_wider(
      names_from = aggregate,
      values_from = value
    ) %>%
    filter(!is.na(B1G) & !is.na(D11)) %>%
    mutate(
      share_labor_cost = ifelse(B1G > 0, pmax(pmin(D11 / B1G, 1), 0), 0)
    ) %>%
    select(year, country, industry, share_labor_cost)

  training_contributions_raw <- trng_cvt_data %>%
    merge(eurostat_correspondence_table_activity) %>% # by nace_r2
    merge(shares_labor_cost) %>% # by year, country, industry
    mutate(
      value = round(share_labor_cost * trng_cvt, digits = 6),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  training_contributions <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      training_contributions_raw,
      by = c("year", "country", "industry")
    ) %>%
    complete_series(min_value = 0, max_value = 100, verbose = verbose) %>%
    proxy_missing_value_by_similarity(., "KNW", min_value = 0, max_value = 100) %>%
    rename(
      training_value = value,
      training_flag = flag
    ) %>%
    select(year, country, industry, training_value, training_flag) %>%
    arrange(year, country, industry)

  # -------------------------
  # FIGARO accounts

  figaro_knw_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(main_aggregates_data) %>%
    left_join(research_contributions) %>%
    left_join(training_contributions) %>%
    mutate(
      contribution_rate = if_else(
        industry %in% c("M72", "P85"),
        1.0,
        pmax(pmin(research_value + training_value, 1.0), 0.0)
      ),
      flag = case_when(
        industry %in% c("M72", "P85") ~ "",
        research_flag == "e" ~ "e",
        training_flag == "e" ~ "e",
        TRUE ~ ""
      )
    ) %>%
    mutate(
      value = if_else(NVA > 0, round(NVA * contribution_rate, digits = 3), 0.0),
      flag = flag
    ) %>%
    select(year, country, industry, value, flag)

  # Check max
  figaro_knw_accounts <- figaro_knw_accounts %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / NVA * 100, 0)) %>%
    # Check upper limit
    mutate(value = pmin(value, 100.0)) %>%
    merge(main_aggregates_data) %>%
    mutate(value = if_else(NVA > 0, value / 100 * NVA, 0)) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_knw_accounts) != size) {
    error_data <<- figaro_knw_accounts
    stop("ERROR - Wrong size for obs accounts (KNW)")
  } else if (any(is.na(figaro_knw_accounts$value))) {
    error_data <<- figaro_knw_accounts
    stop("ERROR - NA values in obs accounts (KNW)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <- figaro_knw_accounts %>%
    mutate(
      serie_id    = "knw_obs",
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
    accounts_data_path  <- file.path(output_dir, "accounts_obs_knw.csv")
    write.csv(formatted_data, accounts_data_path, row.names = FALSE)
  }

  # Return
  return(formatted_data)
}
