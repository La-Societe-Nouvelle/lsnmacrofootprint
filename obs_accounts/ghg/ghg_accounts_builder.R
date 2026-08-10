# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for ghg emissions (GHG)
#'
#' Main sources :
#'   - Greenhouse gas emission footprints (in CO2 equivalent, FIGARO application) - EUROSTAT
#'   - Air Emissions Accounts - OECD
#'
#' Output data
#'   Accounts are in tonnes of CO2e
#'
#' Missing values filled by proxy using industry and country similarity.
#'
#' build_ghg_obs_accounts()

build_ghg_obs_accounts <- function(
  years = 2010:2024, # env_ac_ghgfp stops at 2023 - 2024 covered by extend_ghg_accounts_beyond_eurostat() (EDGAR_2025_GHG confirmed complete to 2024)
  detect_latest_year = FALSE, # confirmatory only, see note below - does not change years
  do_clean_outliers = FALSE, # obs series treated as reliable by default; trd/tgt series keep outlier cleaning on
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build GHG accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_proxy_by_similarity.R")
  source("utils/utils_outliers.R")
  source("utils/utils_source_years.R")
  source("obs_accounts/ghg/ghg_accounts_extensions.R")

  # -------------------------------------------------------------------
  # Optional: confirm env_ac_ghgfp's own ceiling (informational only)
  #
  # Unlike the other builders, this does NOT extend `years` - env_ac_ghgfp
  # structurally lags by n-2, and years beyond that are already handled by
  # extend_ghg_accounts_beyond_eurostat() (EDGAR/AEA/ONS/OFS/OECD transport),
  # a completely separate code path that this mechanism doesn't wrap: EDGAR
  # ships as a multi-hundred-MB zip per gas, so a cheap single-year probe
  # like the ones used elsewhere isn't possible for it. This block only logs
  # whether Eurostat's own direct coverage still matches what the code
  # assumes, to catch it silently changing.

  if (detect_latest_year) {
    fetch_ghgfp_year <- function(year_i) {
      url_probe <- paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/env_ac_ghgfp/1.0/*.*.*.*.*?",
        "c[freq]=", "A",
        "&c[na_item]=", "TOTAL",
        "&c[c_dest]=", "WORLD",
        "&c[TIME_PERIOD]=", year_i,
        "&format=", "csvdata"
      )
      raw <- tryCatch(read.csv(url_probe), error = function(e) NULL)
      if (is.null(raw) || nrow(raw) == 0) return(NULL)
      raw %>% select(c_orig, nace_r2, OBS_VALUE)
    }

    known_good_ghgfp_year <- max(years) - 1 # years default already assumes env_ac_ghgfp lags by >=1 vs the extended ceiling

    detected_ghgfp_year <- detect_max_usable_year(
      source_name     = "GHG_EUROSTAT_GHGFP",
      fetch_year_fn   = fetch_ghgfp_year,
      group_col       = "c_orig",
      value_col       = "OBS_VALUE",
      sector_col      = "nace_r2",
      known_good_year = known_good_ghgfp_year,
      verbose         = verbose
    )

    if (verbose) message(
      "GHG: env_ac_ghgfp confirmed usable up to ", detected_ghgfp_year,
      " - years beyond this rely on extend_ghg_accounts_beyond_eurostat(), not on this check"
    )
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

  eurostat_correspondence_table_nace_r2 <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_nace_r2.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, nace_r2)

  eurostat_correspondence_table_c_orig <- read_delim(
      "obs_accounts/ghg/eurostat_correspondence_table_c_orig.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = figaro_country) %>%
    select(country, c_orig)

  oecd_correspondence_table_activity <- read_delim(
      "obs_accounts/ghg/oecd_correspondence_table_activity.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(industry = figaro_industry) %>%
    select(industry, oecd_activity)

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
  # Eurostat data - Greenhouse gas emission footprints (in CO2 equivalent, FIGARO application)

  eurostat_file_path  <- file.path(download_dir, "env_ac_ghgfp.csv")

  if (!file.exists(eurostat_file_path) | !use_temp_data)
  {
    eurostat_raw_data <- get_eurostat(
      "env_ac_ghgfp",
      filters = list(time = years$year, na_item = "TOTAL", c_dest = "WORLD"),
      cache   = FALSE
    )

    write.csv(eurostat_raw_data, eurostat_file_path, row.names = FALSE)
  }

  eurostat_raw_data <- read.csv(eurostat_file_path)

  eurostat_data <- eurostat_raw_data %>%
    filter(
      freq == "A",
      unit == "THS_T",
      na_item == "TOTAL",
      c_dest == "WORLD"
    ) %>%
    merge(eurostat_correspondence_table_nace_r2) %>%
    merge(eurostat_correspondence_table_c_orig) %>%
    mutate(
      year = substring(as.character(time), 1, 4),
      eurostat_ghg_emissions = values * 1e3,
      unit = "TCO2E"
    ) %>%
    select(year, country, industry, eurostat_ghg_emissions, unit)

  # -------------------------------------------------------------------
  # Extension beyond Eurostat's own coverage (env_ac_ghgfp only goes up to n-2, e.g. 2023 as of
  #  2026) - replicates Eurostat's own FIGARO GHG-footprint methodology directly from EDGAR and
  #  national AEA sources for those extra year(s) - see ghg_accounts_extensions.R

  years_beyond_eurostat <- setdiff(years$year, unique(eurostat_data$year))

  if (length(years_beyond_eurostat) > 0) {

    if (verbose) message("Extending GHG accounts beyond Eurostat's coverage for year(s) ",
                         paste(years_beyond_eurostat, collapse = ", "))

    ghg_estimate_extension <- extend_ghg_accounts_beyond_eurostat(years_beyond_eurostat, verbose)

    eurostat_data <- bind_rows(eurostat_data, ghg_estimate_extension)
  }

  # -------------------------------------------------------------------
  # Carry forward NACE "U" (extraterritorial organisations) intensity where
  # the extension is missing it - the CRF/IPCC categories used by
  # extend_ghg_accounts_beyond_eurostat() have no equivalent for "U"
  # (embassies/international bodies aren't an IPCC emission category), so
  # extension years never produce a value for it even for countries Eurostat
  # itself reports "U" for directly in other years.
  #   U[country, year] = (U[country, prior_year] / NVA_U[country, prior_year]) * NVA_U[country, year]

  years_with_u <- eurostat_data %>% filter(industry == "U") %>% distinct(year, country)
  years_without_u <- eurostat_data %>%
    filter(industry != "U") %>%
    distinct(year, country) %>%
    anti_join(years_with_u, by = c("year", "country"))

  if (nrow(years_without_u) > 0) {

    u_values <- eurostat_data %>%
      filter(industry == "U") %>%
      select(country, prior_year = year, prior_value = eurostat_ghg_emissions)

    u_prior_match <- years_without_u %>%
      merge(u_values, by = "country") %>% # cross all known "U" years per country
      filter(as.integer(prior_year) < as.integer(year)) %>%
      group_by(country, year) %>%
      slice_max(as.integer(prior_year), n = 1, with_ties = FALSE) %>%
      ungroup()

    if (nrow(u_prior_match) > 0) {

      if (verbose) message("Carrying forward NACE 'U' intensity for ",
                           nrow(u_prior_match), " country-year gap(s)")

      nva_u <- main_aggregates_data %>%
        filter(industry == "U") %>%
        mutate(year = as.character(year)) %>%
        select(country, year, NVA)

      u_carry_forward <- u_prior_match %>%
        merge(nva_u %>% rename(prior_year = year, NVA_prior = NVA), by = c("country", "prior_year")) %>%
        merge(nva_u, by = c("country", "year")) %>%
        mutate(
          intensity = if_else(NVA_prior > 0, prior_value / NVA_prior, 0),
          eurostat_ghg_emissions = intensity * NVA,
          industry = "U",
          unit = "TCO2E"
        ) %>%
        select(year, country, industry, eurostat_ghg_emissions, unit)

      eurostat_data <- bind_rows(eurostat_data, u_carry_forward)
    }
  }

  # -------------------------------------------------------------------
  # Disaggregate AL/ME/MK out of FIGW1 ("rest of world" residual)
  #
  # Eurostat's FIGARO GHG footprint modelling doesn't cover these EU
  # candidate countries individually - env_ac_ghgfp's c_orig is NA for all
  # three (see eurostat_correspondence_table_c_orig.csv), meaning their
  # emissions are folded into FIGW1's "WRL_REST" residual along with every
  # other unmodelled country. They do have their own FIGARO NVA figures
  # though (national accounts are collected more broadly than the full
  # multi-regional footprint model), so each country's share of FIGW1's
  # residual footprint is estimated from its share of FIGW1's residual
  # production, by (industry, year):
  #   country_ghg[i,y] = FIGW1_ghg[i,y] * country_NVA[i,y] / FIGW1_NVA[i,y]
  # FIGW1's own footprint is reduced by the same amount so the "rest of
  # world" total is unchanged - just reallocated, not double-counted.

  candidate_countries_in_figw1 <- c("AL", "ME", "MK")

  figw1_ghg <- eurostat_data %>%
    filter(country == "FIGW1") %>%
    select(year, industry, figw1_ghg = eurostat_ghg_emissions)

  figw1_nva <- main_aggregates_data %>%
    filter(country == "FIGW1") %>%
    mutate(year = as.character(year)) %>%
    select(year, industry, figw1_nva = NVA)

  candidates_nva <- main_aggregates_data %>%
    filter(country %in% candidate_countries_in_figw1) %>%
    mutate(year = as.character(year)) %>%
    select(year, country, industry, NVA)

  if (nrow(figw1_ghg) > 0) {

    if (verbose) message("Extracting ", paste(candidate_countries_in_figw1, collapse = ", "),
                         " from FIGW1 proportionally to their NVA share")

    figw1_extraction <- candidates_nva %>%
      merge(figw1_ghg, by = c("year", "industry")) %>%
      merge(figw1_nva, by = c("year", "industry")) %>%
      mutate(
        eurostat_ghg_emissions = if_else(figw1_nva > 0, figw1_ghg * NVA / figw1_nva, 0),
        unit = "TCO2E"
      ) %>%
      select(year, country, industry, eurostat_ghg_emissions, unit)

    figw1_reduction <- figw1_extraction %>%
      group_by(year, industry) %>%
      summarise(extracted = sum(eurostat_ghg_emissions, na.rm = TRUE), .groups = "drop")

    eurostat_data <- eurostat_data %>%
      merge(figw1_reduction, by = c("year", "industry"), all.x = TRUE) %>%
      mutate(
        extracted = coalesce(extracted, 0),
        eurostat_ghg_emissions = if_else(
          country == "FIGW1",
          eurostat_ghg_emissions - extracted,
          eurostat_ghg_emissions
        )
      ) %>%
      select(-extracted) %>%
      bind_rows(figw1_extraction)
  }

  # -------------------------------------------------------------------
  # OECD data

  base_url_oecd_data = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.2/..EMISSIONS.T_CO2E+T...GHG...?"
  url_oecd_data = paste0(base_url_oecd_data,
    "startPeriod=",min(years$year),
    "&endPeriod=",max(years$year),
    "&dimensionAtObservation=","AllDimensions",
    "&format=","csvfilewithlabels"
  )

  oecd_raw_data <- read.csv(url_oecd_data)

  oecd_data <- oecd_raw_data %>%
    filter(
      MEASURE == "EMISSIONS",
      POLLUTANT == "GHG",
      METHODOLOGY == "EMISSIONS_SEEA",
      ACTION == "I",
      FREQ == "A",
      UNIT_MEASURE == "T_CO2E", # TONNES
      SOURCE == "REPORTED",
      ACTIVITY_SCOPE == "RES",
      ADJUSTMENT == "N"
    ) %>%
    mutate(
      REF_AREA = countrycode(REF_AREA, 'iso3c', 'iso2c', nomatch = NULL)
    ) %>%
    # format
    mutate(
      year = as.character(TIME_PERIOD),
      country = REF_AREA,
      oecd_activity = ACTIVITY,
      oecd_ghg_emissions = OBS_VALUE,
    ) %>%
    merge(oecd_correspondence_table_activity) %>%
    select(year,country,industry,oecd_ghg_emissions)

  # -------------------------------------------------------------------
  # Fill remaining country gaps directly from Eurostat AEA (env_ac_ainah_r2)
  #
  # env_ac_ghgfp (FIGARO footprint) and OECD AEA both only cover EU/EFTA/OECD
  # members. Eurostat separately collects raw Air Emissions Accounts from EU
  # candidate countries though, and some of them are complete panels that
  # neither of the two main sources above ever surfaces - confirmed live for
  # Serbia (RS): complete 2010-2023 in env_ac_ainah_r2, present in neither
  # eurostat_data nor oecd_data. Reuses the same NACE r2 -> FIGARO
  # correspondence table as env_ac_ghgfp and the dataset's own "GHG"
  # (CO2-equivalent total) pollutant series, so no CRF/EDGAR-style
  # methodology replication is needed for these countries.
  #
  # NB: a plain "does the country appear at all" check is not enough - RS
  # turned out to have a handful of stray OECD rows (real data, but nowhere
  # near a full panel). Because the case_when() below picks a country's
  # *entire* value from whichever source it first matches in, that token
  # presence was silently routing all of RS to the OECD branch - which then
  # produced NA for every (year, industry) OECD had no row for, instead of
  # falling through to this AEA fill. A coverage-share floor avoids that.

  full_grid_per_country <- nrow(years) * nrow(figaro_industries)
  coverage_floor <- 0.5 * full_grid_per_country

  country_source_coverage <- figaro_countries %>%
    left_join(count(eurostat_data, country, name = "n_eurostat"), by = "country") %>%
    left_join(count(oecd_data, country, name = "n_oecd"), by = "country") %>%
    mutate(
      n_eurostat = coalesce(n_eurostat, 0L),
      n_oecd     = coalesce(n_oecd, 0L)
    )

  countries_missing_from_main_sources <- country_source_coverage %>%
    filter(n_eurostat < coverage_floor, n_oecd < coverage_floor) %>%
    pull(country)

  if (length(countries_missing_from_main_sources) > 0) {

    if (verbose) message("Trying Eurostat AEA (env_ac_ainah_r2) directly for countries missing from both main sources: ",
                         paste(countries_missing_from_main_sources, collapse = ", "))

    aea_gap_fill_raw <- tryCatch(
      get_eurostat(
        "env_ac_ainah_r2",
        filters = list(
          geo    = countries_missing_from_main_sources,
          time   = years$year,
          airpol = "GHG",
          unit   = "T"
        ),
        cache = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(aea_gap_fill_raw) && nrow(aea_gap_fill_raw) > 0) {

      aea_gap_fill <- aea_gap_fill_raw %>%
        mutate(
          year    = format(time, "%Y"),
          country = geo
        ) %>%
        merge(eurostat_correspondence_table_nace_r2) %>% # by nace_r2
        mutate(
          eurostat_ghg_emissions = values,
          unit = "TCO2E"
        ) %>%
        select(year, country, industry, eurostat_ghg_emissions, unit)

      if (verbose) message("AEA direct fill: ", n_distinct(aea_gap_fill$country), " countr(y/ies), ",
                           n_distinct(aea_gap_fill$year), " year(s)")

      eurostat_data <- bind_rows(eurostat_data, aea_gap_fill)
    }
  }

  # -------------------------------------------------------------------
  # Fill Serbia's residual gap (currently just the latest year, where OECD
  # hasn't reported yet - see the unit-join note above) using EDGAR's
  # "Serbia and Montenegro" (SCG) legacy combined zone as a year-over-year
  # growth proxy - not as an absolute value, which would double-count
  # Montenegro (already handled above via the FIGW1 extraction):
  #   RS[i, year] = RS[i, reference_year] * EDGAR_SCG_total[year] / EDGAR_SCG_total[reference_year]
  # EDGAR still reports pre-2006-split Serbia+Montenegro as a single "SCG"
  # entity even for recent years, so this can only ever be used as a trend,
  # never as a direct value for Serbia alone.

  rs_present_years <- oecd_data %>% filter(country == "RS") %>% distinct(year) %>% pull(year)
  rs_missing_years <- setdiff(years$year, rs_present_years)

  if (length(rs_missing_years) > 0 && length(rs_present_years) > 0) {

    edgar_dir       <- file.path(download_dir, "edgar")
    edgar_files     <- c("IEA_EDGAR_CO2_1970_2024", "EDGAR_CH4_1970_2024", "EDGAR_N2O_1970_2024",
                         "EDGAR_AR5g_F-gases_1990_2024")
    edgar_base_url  <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/EDGAR_2025_GHG"

    for (file_name in edgar_files) {
      filepath <- file.path(edgar_dir, paste0(file_name, ".xlsx"))
      if (!file.exists(filepath)) {
        dir.create(edgar_dir, recursive = TRUE, showWarnings = FALSE)
        tmp_zip <- tempfile(fileext = ".zip")
        curl_download(paste0(edgar_base_url, "/", file_name, ".zip"), tmp_zip)
        unzip(tmp_zip, exdir = edgar_dir, overwrite = TRUE)
        unlink(tmp_zip)
      }
    }

    fetch_edgar_scg_total <- function(year_i) {
      year_col <- paste0("Y_", year_i)

      totals <- sapply(edgar_files, function(file_name) {
        df <- read_xlsx(file.path(edgar_dir, paste0(file_name, ".xlsx")), sheet = "TOTALS BY COUNTRY", skip = 9)
        if (!year_col %in% colnames(df)) return(NA_real_)
        scg_rows <- df %>% filter(Country_code_A3 == "SCG")
        if (nrow(scg_rows) == 0) return(NA_real_)
        # CH4/N2O files report native gas mass (kt) - CO2/F-gases (already GWP_100_AR5 CO2e) pass through
        gwp_factor <- if (grepl("^EDGAR_CH4", file_name)) 28 else if (grepl("^EDGAR_N2O", file_name)) 265 else 1
        sum(scg_rows[[year_col]], na.rm = TRUE) * gwp_factor
      })

      if (any(is.na(totals))) return(NA_real_)
      sum(totals)
    }

    reference_year   <- max(rs_present_years)
    edgar_reference  <- fetch_edgar_scg_total(reference_year)

    for (target_year in rs_missing_years) {
      edgar_target <- fetch_edgar_scg_total(target_year)

      if (!is.na(edgar_reference) && !is.na(edgar_target) && edgar_reference > 0) {
        ratio <- edgar_target / edgar_reference

        if (verbose) message("Filling Serbia gap for ", target_year,
                             " via EDGAR 'Serbia and Montenegro' y/y trend (ratio = ", round(ratio, 3), ")")

        rs_carry_forward <- oecd_data %>%
          filter(country == "RS", year == reference_year) %>%
          transmute(
            year = target_year,
            country,
            industry,
            eurostat_ghg_emissions = oecd_ghg_emissions * ratio,
            unit = "TCO2E"
          )

        eurostat_data <- bind_rows(eurostat_data, rs_carry_forward)
      } else if (verbose) {
        message("Could not compute EDGAR SCG y/y ratio for ", target_year, " - leaving to proxy fill")
      }
    }
  }

  # -------------------------------------------------------------------
  # Building FIGARO accounts


  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_ghg_accounts_raw <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(
      eurostat_data,
      by = c("year", "country", "industry")
    ) %>%
    left_join(
      oecd_data,
      # NB: not joined on "unit" - that column only gets populated by the
      # eurostat_data left_join just above, so for a country absent from
      # eurostat_data entirely (e.g. Serbia) it stays NA and would silently
      # fail to match ANY oecd_data row here, no matter how much real OECD
      # coverage that country actually has. Both sources are always TCO2E.
      by = c("year", "country", "industry")
    ) %>%
    mutate(
      # coalesce(), not case_when(... country %in% X$country ...): the old
      # per-country membership check picks a source for a country's *every*
      # row as soon as it appears anywhere in that source, which silently
      # produces NA for any (year, industry) that source doesn't actually
      # cover for that country - this is exactly what made Serbia's real,
      # near-complete OECD coverage disappear the moment a single EDGAR-
      # derived eurostat_data row was added for it (see above). coalesce()
      # decides per row instead, so a partial source can never crowd out a
      # more complete one for years/industries it doesn't itself cover.
      value = coalesce(eurostat_ghg_emissions, oecd_ghg_emissions),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Complete with similarity
  figaro_ghg_accounts <- figaro_ghg_accounts_raw %>%
    proxy_missing_value_by_similarity(., "GHG") %>%
    select(year, country, industry, value, flag)

  # Clean outliers
  if (do_clean_outliers) {
    figaro_ghg_accounts <- figaro_ghg_accounts %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value / NVA, 0)) %>%
      clean_outliers(
        .,
        serie_pkey = c("country", "industry")
      ) %>%
      merge(main_aggregates_data) %>%
      mutate(value = if_else(NVA > 0, value * NVA, 0)) %>%
      select(year, country, industry, value, flag)
  }

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_ghg_accounts) != size) {
    error_data <<- figaro_ghg_accounts
    stop("ERROR - Wrong size for obs accounts (GHG)")
  } else if (any(is.na(figaro_ghg_accounts$value))) {
    error_data <<- figaro_ghg_accounts
    stop("ERROR - NA values in obs accounts (GHG)")
  }

  if (verbose) message("Accounts ready !")

  # -------------------------------------------------------------------
  # Formatting data

  formatted_data <<- figaro_ghg_accounts %>%
    mutate(
      serie_id    = "ghg_obs",
      value       = round(value, digits = 0),
      unit        = "TCO2E",
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, unit, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  if (verbose) print(formatted_data %>% as_tibble())

  # -------------------------------------------------------------------
  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_ghg.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
