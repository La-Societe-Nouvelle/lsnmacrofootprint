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
  years = 2010:2024, # UNEP/IRP Global Material Flows DB confirmed complete to 2024
  detect_latest_year = FALSE, # if TRUE, probe UNEP mfa4 beyond max(years) and extend if complete (see utils_source_years.R)
  do_clean_outliers = FALSE, # obs series treated as reliable by default; trd/tgt series keep outlier cleaning on
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build MAT accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_source_years.R")

  # -------------------------------------------------------------------
  # Optional: detect the latest usable UNEP mfa4 year beyond max(years)
  #
  # UNEP publishes one wide CSV (one column per year, no year filter in the
  # URL) rather than a per-year endpoint - so the "probe" downloads it once
  # (memoised across candidate years within this call) and checks whether the
  # next year's column exists and is populated.

  if (detect_latest_year) {
    unep_probe_data <- NULL

    fetch_unep_year <- function(year_i) {
      if (is.null(unep_probe_data)) {
        raw <- tryCatch(
          read.csv("https://unep-irp.fineprint.global/mfa4/export?flowTypes[0]=DE"),
          error = function(e) NULL
        )
        if (is.null(raw)) return(NULL)
        unep_probe_data <<- raw
      }

      year_col <- paste0("X", year_i) # read.csv() prefixes numeric-only column names with "X"
      if (!year_col %in% colnames(unep_probe_data)) return(NULL)

      unep_probe_data %>%
        filter(
          Flow.code == "DE",
          Flow.unit == "t",
          Category %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
        ) %>%
        transmute(
          country = countrycode(Country, "country.name", "iso2c", nomatch = NULL),
          sector  = Category,
          value   = .data[[year_col]]
        ) %>%
        filter(!is.na(value))
    }

    detected_max_year <- detect_max_usable_year(
      source_name     = "MAT_UNEP_MFA4",
      fetch_year_fn   = fetch_unep_year,
      group_col       = "country",
      value_col       = "value",
      sector_col      = "sector",
      known_good_year = max(years),
      verbose         = verbose
    )

    if (detected_max_year > max(years)) {
      if (verbose) message("MAT: extending years to detected max usable UNEP mfa4 year ", detected_max_year)
      years <- min(years):detected_max_year
    }
  }

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
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # UNEP data

  base_url_unep_data = "https://unep-irp.fineprint.global/mfa4/export?"
  url_unep_data = paste0(base_url_unep_data,
    "flowTypes[0]=","DE" # DE Domestic Extraction
  )

  unep_file_path  <- file.path(download_dir, "mfa4.csv")

  if (!file.exists(unep_file_path) | !use_temp_data)
  {
    if (verbose) print("Téléchargement des données UNEP...")

    unep_raw_data <- read.csv(url_unep_data)

    write.csv(unep_raw_data, unep_file_path, row.names = FALSE)
  }

  unep_raw_data <- read.csv(unep_file_path)

  unep_data <- unep_raw_data %>%
    pivot_longer(
      6:ncol(.),
      names_to = "year",
      values_to = "value"
    ) %>%
    filter(
      Flow.code == "DE",
      Flow.unit == "t",
      Category %in% c("Biomass", "Fossil fuels", "Metal ores", "Non-metallic minerals")
    ) %>%
    mutate(
      year = sub("^X", "", year),
      country = countrycode(Country, 'country.name', 'iso2c', nomatch = NULL),
      flow = Flow.code,
      category = Category,
      unit = "T"
    ) %>%
    filter(year %in% years$year) %>%
    select(year, country, flow, category, value, unit)

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
    select(year, country, sector, value)

  # use VA to split impacts between FIGARO industries
  va_distribution_sectors <- main_aggregates_data %>%
    merge(figaro_industries) %>%
    group_by(year, country, sector) %>%
    mutate(
      share_sector = NVA / sum(NVA, na.rm = TRUE), # /!\ if VA < 0 !
    ) %>%
    ungroup() %>%
    select(year, country, industry, share_sector)

  # -------------------------
  # Accounts data

  raw_mat_accounts <- sector_extraction %>%
    merge(figaro_industries) %>% # by sector
    merge(va_distribution_sectors) %>% # by year, country, industry
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
    proxy_missing_value_by_similarity(., "MAT") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  if (do_clean_outliers) {
    figaro_mat_accounts <- figaro_mat_accounts %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
      clean_outliers(., serie_pkey = c("country", "industry")) %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
      select(year, country, industry, value, flag)
  }

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

  accounts_data_path  <- file.path(output_dir, "accounts_obs_mat.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
