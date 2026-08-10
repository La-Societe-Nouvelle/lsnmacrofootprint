# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Extension of observed GHG accounts beyond Eurostat's own coverage
#'
#' Methodology : Eurostat, "FIGARO - Greenhouse gas emission estimates - Methodological note",
#'   18 November 2024 (env_ac_ghgfp / env_ac_ainah_r2 - Eurostat, Directorate E, Unit E.2).
#'
#' Credit : this implementation is a direct port of the replication and validation work by
#'   Sylvain Larrieu (Insee) in github.com/InseeFrLab/global-ghg-emissions (see the
#'   "mirror_eurostat" option there for the full history/validation of this method). All credit
#'   for the underlying methodology and its first working implementation belongs there ; this file
#'   only adapts it to lsnmacrofootprint's own data conventions and FIGARO caches.
#'
#' Eurostat's GHG footprint (env_ac_ghgfp, used in ghg_accounts_builder.R) only covers 2010 to n-2
#'   (n being the current year - see the methodological note above). Whenever FIGARO itself has
#'   observed years that env_ac_ghgfp hasn't reached yet, this replicates Eurostat's own
#'   FIGARO GHG-footprint methodology directly from EDGAR + national AEA sources, for those extra
#'   years only :
#'   - Official Air Emission Accounts (AEA) where reported to Eurostat (env_ac_ainah_r2), used
#'     as-is for countries with a complete AEA
#'   - EDGAR (JRC), used to estimate the remaining countries, allocated from CRF categories to
#'     NACE industries using EU27 emission intensities (Eurostat's method) - for non-EU countries,
#'     EDGAR itself draws on IEA data (International Energy Agency) as modified by the JRC,
#'     licensed CC BY-NC-ND 4.0 (non-commercial use only ; contact compliance@iea.org for any
#'     other use)
#'   - UK's own AEA (ONS) and Switzerland's own AEA (BFS - not confidentiality-suppressed, unlike
#'     what Switzerland reports to Eurostat) used directly where available, instead of the EDGAR
#'     estimate
#'   - OECD air / maritime transport CO2 emissions, replacing/completing the EDGAR-based transport
#'     allocation (same as Eurostat's own method)
#'   - EU physical energy flow accounts (PEFA), used only to derive the households/industries
#'     split of road transport emissions
#'
#' Static reference tables (bridges CRF <-> NACE, households coefficients, country groupings) live
#'   in obs_accounts/ghg/resources/ - copied from InseeFrLab/global-ghg-emissions (see credit above).
#'
#' extend_ghg_accounts_beyond_eurostat(years, verbose)


# ==========================================================================================
# 1. SOURCES
# ==========================================================================================

resources_dir <- "obs_accounts/ghg/resources"

# ------------------------------------------------------------------------------------------
#' EDGAR (JRC) emissions by IPCC 2006 code and FIGARO country, for the requested years
#'
#' Downloads the 4 needed EDGAR time series (CO2, CH4, N2O, AR5g F-gases) from the JRC open-data
#'   FTP and keeps only fossil emissions.

load_edgar_emissions_extension <- function(years, verbose = FALSE) {

  edgar_base_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/EDGAR_2025_GHG"
  edgar_files    <- c("IEA_EDGAR_CO2_1970_2024", "EDGAR_CH4_1970_2024", "EDGAR_N2O_1970_2024",
                      "EDGAR_AR5g_F-gases_1990_2024")
  edgar_dir      <- file.path(download_dir, "edgar")

  edgar_countries <- read_xlsx(file.path(resources_dir, "nomenclatures.xlsx"), "edgar_countries") %>%
    select(Country_code_A3, country = country_code_figaro)

  for (file_name in edgar_files) {
    filepath <- file.path(edgar_dir, paste0(file_name, ".xlsx"))
    if (!file.exists(filepath)) {
      if (verbose) message("Downloading EDGAR - ", file_name)
      dir.create(edgar_dir, recursive = TRUE, showWarnings = FALSE)
      tmp_zip <- tempfile(fileext = ".zip")
      curl_download(paste0(edgar_base_url, "/", file_name, ".zip"), tmp_zip)
      unzip(tmp_zip, exdir = edgar_dir, overwrite = TRUE)
      unlink(tmp_zip)
    }
  }

  edgar_raw <- map_dfr(edgar_files, function(file_name) {
    read_xlsx(file.path(edgar_dir, paste0(file_name, ".xlsx")), sheet = "IPCC 2006", skip = 9)
  })

  edgar_raw %>%
    filter(fossil_bio == "fossil") %>%
    select(Country_code_A3, ipcc_code_2006 = ipcc_code_2006_for_standard_report, gas = Substance,
          starts_with("Y_")) %>%
    inner_join(edgar_countries, by = "Country_code_A3") %>%
    select(-Country_code_A3) %>%
    pivot_longer(starts_with("Y_"), names_to = "year", values_to = "value") %>%
    mutate(year = substring(year, 3, 6)) %>%
    filter(year %in% years) %>%
    # recode F-gases names, aggregate NF3 + SF6
    mutate(gas = if_else(grepl("GWP_100_AR5_", gas), paste0(substring(gas, 13), "_CO2E"), gas),
          gas = case_when(gas == "HCFC_CO2E" ~ "HFC_CO2E",
                          gas %in% c("NF3_CO2E", "SF6_CO2E") ~ "NF3_SF6_CO2E",
                          TRUE ~ gas)) %>%
    group_by(country, ipcc_code_2006, gas, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# ------------------------------------------------------------------------------------------
#' Official AEA reported to Eurostat (env_ac_ainah_r2), for the requested years
#'
#' Complete for EU27 (+ incomplete for CH/NO/TR, not reported at all for the rest) - used as-is
#'   for the "complete AEA" countries, and to compute the EU27 emission-intensity coefficients
#'   used to allocate EDGAR's CRF categories to NACE industries.

load_aea_eurostat_extension <- function(years, verbose = FALSE) {

  if (verbose) message("Downloading Eurostat AEA (env_ac_ainah_r2)")

  nace_bridge <- read_xlsx(file.path(resources_dir, "nomenclatures.xlsx"), "cpa_nace_a64") %>%
    select(industry = code_a64_figaro, nace_r2 = code_a64_eurostat_aea)

  get_eurostat("env_ac_ainah_r2",
              filters = list(time = years, unit = "THS_T",
                             airpol = c("CO2", "CH4", "N2O", "HFC_CO2E", "PFC_CO2E", "NF3_SF6_CO2E")),
              cache = FALSE) %>%
    rename(gas = airpol, country = geo) %>%
    mutate(year = substring(as.character(time), 1, 4),
          country = if_else(country == "EL", "GR", country)) %>%
    inner_join(nace_bridge, by = "nace_r2") %>%
    select(gas, year, country, industry, value = values)
}

# ------------------------------------------------------------------------------------------
#' Official AEA for the United Kingdom (ONS), for the requested years
#'
#' The UK doesn't report AEA to Eurostat - ONS's own dataset is used instead (unchanged
#'   convention from ghg_accounts_builder.R's fallback logic).

load_aea_gb_extension <- function(years, verbose = FALSE) {

  if (verbose) message("Downloading UK AEA (ONS)")

  gb_url <- paste0(
    "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/",
    "ukenvironmentalaccountsatmosphericemissionsgreenhousegasemissionsbyeconomicsectorandgasunitedkingdom/",
    "current/05atmoshpericemissionsghg.xlsx"
  )
  gb_filepath <- file.path(download_dir, "aea_gb.xlsx")
  if (!file.exists(gb_filepath)) {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    curl_download(gb_url, gb_filepath)
  }

  bridge_gb <- read_xlsx(file.path(resources_dir, "bridge_a64_to_AEA_uk.xlsx"))

  gas_sheets <- c("CO2", "CH4", "N2O", "HFC", "PFC", "NF3", "SF6")
  ch4_gwp <- 28
  n2o_gwp <- 265

  # locates the "SIC(07) group" header row of the right-hand ("by group") table in column 27 -
  #   its position has already changed once between ONS file vintages, so it's found dynamically
  #   rather than hardcoded (see global-ghg-emissions history for the incident)
  find_header_skip <- function(sheet) {
    preview <- suppressMessages(read_xlsx(gb_filepath, sheet = sheet, col_names = FALSE, n_max = 15))
    header_row <- which(preview[[27]] == "SIC(07) group")[1]
    if (is.na(header_row)) stop("Could not locate the 'SIC(07) group' header row in ONS sheet '", sheet, "'")
    header_row - 1
  }

  gb_raw <- map_dfr(gas_sheets, function(gas_sheet) {
    read_xlsx(gb_filepath, sheet = gas_sheet, skip = find_header_skip(gas_sheet))[-1, -c(1:26)] %>%
      pivot_longer(-1, names_to = "sic_07_group") %>%
      filter(sic_07_group != "Total") %>%
      transmute(year = as.character(`SIC(07) group`),
               sic_a88 = if_else(sic_07_group %in% c("100", "101"), sic_07_group, substr(sic_07_group, 1, 2)),
               value = replace_na(as.numeric(value), 0),
               gas = case_when(gas_sheet %in% c("NF3", "SF6") ~ "NF3_SF6_CO2E",
                               gas_sheet %in% c("HFC", "PFC") ~ paste0(gas_sheet, "_CO2E"),
                               TRUE ~ gas_sheet))
  }) %>%
    filter(year %in% years) %>%
    inner_join(bridge_gb %>% select(industry = emis_industry, sic_a88) %>% distinct(), by = "sic_a88") %>%
    group_by(gas, year, industry) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(value = case_when(gas == "CH4" ~ value / ch4_gwp,
                             gas == "N2O" ~ value / n2o_gwp,
                             TRUE ~ value),
          gas = if_else(gas %in% c("NF3", "SF6"), "NF3_SF6_CO2E", gas)) %>%
    mutate(country = "GB")
}

# ------------------------------------------------------------------------------------------
#' Official AEA for Switzerland (BFS PxWeb API), for the requested years
#'
#' Switzerland reports an incomplete AEA to Eurostat (C19/C20, H50/H51, K64/K65 are
#'   confidentiality-suppressed there) - BFS's own AEA has no such gaps.
#' Water and air transport are only published combined ("50-51") - split into H50/H51 downstream
#'   in estimate_ghg_from_edgar(), using this project's own EDGAR/OECD-based split ratio between
#'   the two (BFS doesn't split them).

load_aea_ch_extension <- function(years, verbose = FALSE) {

  if (verbose) message("Downloading Swiss AEA (BFS)")

  bfs_api_url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0204000000_104/px-x-0204000000_104.px"
  bridge_ch   <- read_xlsx(file.path(resources_dir, "bridge_a64_to_AEA_ch.xlsx"))

  gas_map <- tribble(
    ~bfs_gas_code,  ~gas,           ~bfs_unit,
    "02_CO2_foss",  "CO2",          "MTONS",
    "05_CH4",       "CH4",          "MTONS",
    "04_N2O",       "N2O",          "MTONS",
    "06_HFCs",      "HFC_CO2E",     "MTEQCO2",
    "07_PFCs",      "PFC_CO2E",     "MTEQCO2",
    "08_SF6",       "NF3_SF6_CO2E", "MTEQCO2" # BFS's "SF6 and NF3" already matches our combined category
  )

  query_bfs <- function(bfs_unit, bfs_gas_codes) {
    # Jahr (year) is queried unfiltered ("all") and filtered down to `years` afterwards in R - BFS
    #  rejects the whole request with a plain-text 400 (not valid JSON) if asked for a year it
    #  hasn't published yet (e.g. still no 2024 as of 2026), which would otherwise break the query
    #  for every other dimension too instead of just yielding no rows for that year
    body <- toJSON(list(
      query = list(
        list(code = "Masseinheit", selection = list(filter = "item", values = list(bfs_unit))),
        list(code = "Wirtschaft und Haushalte", selection = list(filter = "all", values = list("*"))),
        list(code = "Gas", selection = list(filter = "item", values = as.list(bfs_gas_codes))),
        list(code = "Jahr", selection = list(filter = "all", values = list("*")))
      ),
      response = list(format = "json")
    ), auto_unbox = TRUE)

    h <- new_handle()
    handle_setopt(h, post = TRUE, postfields = body)
    handle_setheaders(h, "Content-Type" = "application/json")
    resp <- curl_fetch_memory(bfs_api_url, handle = h)
    if (resp$status_code != 200) stop("BFS API request failed (HTTP ", resp$status_code, "): ", rawToChar(resp$content))
    parsed <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)

    map_dfr(parsed$data, function(row) {
      tibble(bfs_code = as.integer(row$key[[2]]), bfs_gas_code = row$key[[3]],
            year = row$key[[4]], value = suppressWarnings(as.numeric(row$values[[1]])))
    })
  }

  ch_raw <- bind_rows(
    query_bfs("MTONS", gas_map$bfs_gas_code[gas_map$bfs_unit == "MTONS"]),
    query_bfs("MTEQCO2", gas_map$bfs_gas_code[gas_map$bfs_unit == "MTEQCO2"])
  ) %>%
    inner_join(gas_map %>% select(bfs_gas_code, gas), by = "bfs_gas_code") %>%
    filter(!is.na(value), year %in% years)

  ch_direct <- ch_raw %>%
    inner_join(bridge_ch, by = "bfs_code", relationship = "many-to-many") %>%
    group_by(gas, year, industry = emis_industry) %>%
    summarise(value = sum(value), .groups = "drop")

  # K66 (Activities auxiliary to financial services) : not reported separately by BFS - computed
  #   as a residual (item 53 "64-66 total" minus item 54 "64" minus item 55 "65")
  ch_K66 <- ch_raw %>%
    filter(bfs_code %in% c(53, 54, 55)) %>%
    mutate(sign = if_else(bfs_code == 53, 1, -1)) %>%
    group_by(gas, year) %>%
    summarise(value = sum(value * sign), .groups = "drop") %>%
    mutate(industry = "K66")

  ch_H50H51_combined <- ch_raw %>%
    filter(bfs_code == 42) %>%
    transmute(gas, year, industry = "H50H51_combined", value)

  bind_rows(ch_direct, ch_K66, ch_H50H51_combined) %>%
    mutate(country = "CH")
}

# ------------------------------------------------------------------------------------------
#' OECD air / maritime transport CO2 emissions, for the requested years
#'
#' Used to replace (air) / complement (maritime, on top of EDGAR's domestic-navigation figure)
#'   the EDGAR-based CO2 estimate for NACE H50/H51 - same method as Eurostat's own note.
#' Selections (found by inspecting the raw dataflows, matching what used to be picked by hand in
#'   the OECD Data Explorer) : air -> Emissions = "Air emissions accounts: air transport"
#'   (EMISSIONS_SOURCE == RES_TOTAL), Methodology = "SEEA (residence principle)" ; maritime ->
#'   Frequency = Annual, Vessel = "All vessels".

load_oecd_transport_extension <- function(years, verbose = FALSE) {

  if (verbose) message("Downloading OECD air / maritime transport CO2 emissions")

  figaro_countries <- read_xlsx(file.path(resources_dir, "nomenclatures.xlsx"), "figaro_countries") %>%
    select(country = country_code_figaro, country_code_oecd)

  air_url <- paste0("https://sdmx.oecd.org/public/rest/data/",
                    "OECD.SDD.NAD.SEEA,DSD_AIR_TRANSPORT@DF_AIR_TRANSPORT,1.0/all",
                    "?dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
  maritime_url <- paste0("https://sdmx.oecd.org/public/rest/data/",
                         "OECD.SDD.NAD.SEEA,DSD_MARITIME_TRANSPORT@DF_MARITIME_TRANSPORT,1.0/all",
                         "?dimensionAtObservation=AllDimensions&format=csvfilewithlabels")

  air_raw <- fread(air_url)
  air <- air_raw[FREQ == "A" & EMISSIONS_SOURCE == "RES_TOTAL" & METHODOLOGY == "EMISSIONS_SEEA" &
                  FLIGHT_TYPE == "_T" & !REF_AREA %in% c("OECD", "W") & TIME_PERIOD %in% years] %>%
    as_tibble() %>%
    transmute(country_code_oecd = REF_AREA, year = as.character(TIME_PERIOD), value = OBS_VALUE / 1000) %>%
    inner_join(figaro_countries, by = "country_code_oecd")

  maritime_raw <- fread(maritime_url)
  maritime_alpha3 <- maritime_raw[FREQ == "A" & VESSEL == "ALL_VESSELS" & TIME_PERIOD %in% years] %>%
    as_tibble() %>%
    transmute(country_code_oecd = REF_AREA, year = as.character(TIME_PERIOD), value = OBS_VALUE / 1000) %>%
    left_join(figaro_countries, by = "country_code_oecd") %>%
    mutate(country = if_else(country_code_oecd == "W", "WORLD", country))

  maritime_world <- maritime_alpha3 %>% filter(country == "WORLD") %>% select(year, value_world = value)
  maritime_excl_row <- maritime_alpha3 %>% filter(!is.na(country), country != "WORLD") %>%
    group_by(year) %>% summarise(value_excl_row = sum(value), .groups = "drop")
  maritime_row <- maritime_world %>% left_join(maritime_excl_row, by = "year") %>%
    mutate(country = "FIGW1", value = value_world - value_excl_row) %>%
    select(year, country, value)
  maritime <- maritime_alpha3 %>% filter(!is.na(country), country != "WORLD") %>%
    select(year, country, value) %>%
    bind_rows(maritime_row)

  list(air = air %>% select(year, country, value), maritime = maritime)
}

# ------------------------------------------------------------------------------------------
#' EU physical energy flow accounts (PEFA), for the reference period only
#'
#' Only used to derive the households / industries split of road transport emissions (CRF
#'   1.A.3.b_noRES) - a fixed key, averaged over the same reference period as the other
#'   CRF -> NACE allocation coefficients (2014-2018, cf. Eurostat's methodological footnote 7).

load_pefa_eu_extension <- function(verbose = FALSE) {

  if (verbose) message("Downloading EU physical energy flow accounts (PEFA)")

  nace_bridge <- read_xlsx(file.path(resources_dir, "nomenclatures.xlsx"), "cpa_nace_a64") %>%
    filter(!grepl("HH", code_a64_figaro)) %>%
    select(industry = code_a64_figaro, nace_r2 = code_a64_eurostat_aea)

  get_eurostat("env_ac_pefasu",
              filters = list(stk_flow = "USE", prod_nrg = c("P14", "P17"), geo = "EU27_2020",
                             time = as.character(seq(2014, 2015))), # avoid confidential data
              cache = FALSE) %>%
    rename(nace_r2 = nace_r2) %>%
    inner_join(nace_bridge, by = "nace_r2") %>%
    group_by(industry) %>%
    summarise(nrg_consumption = sum(values, na.rm = TRUE), .groups = "drop")
}


# ==========================================================================================
# 2. ESTIMATION (Eurostat's own CRF -> NACE allocation methodology)
# ==========================================================================================

#' Replicates Eurostat's FIGARO GHG-footprint methodology from EDGAR, for the requested years
#'
#' Returns one row per gas x year x country x industry, values in native mass for CO2/CH4/N2O
#'   and in CO2-equivalent for the F-gas groups (same convention throughout) - aggregate_ghg_co2eq()
#'   converts everything to a single CO2-equivalent total afterwards.

estimate_ghg_from_edgar <- function(years, verbose = FALSE) {

  reference_years <- as.character(seq(2014, 2018))

  # FIGARO's production/output aggregate (PRD) - only ever needed for the fixed 2014-2018
  #  reference period (Eurostat's own allocation keys are never re-estimated year to year), loaded
  #  independently of whatever the caller already has in scope
  if (verbose) message("Loading FIGARO main aggregates (reference period ", reference_years[1], "-",
                       reference_years[length(reference_years)], ")")
  main_aggregates_data <- map_dfr(reference_years, load_local_figaro_main_aggregates)

  # ---- nomenclatures ----

  countries <- read_xlsx(file.path(resources_dir, "nomenclatures.xlsx"), "figaro_countries") %>%
    mutate(annex1 = as.logical(annex1))
  eu27_countries       <- countries %>% filter(EU27_esa_code == "SIS21") %>% pull(country_code_figaro)
  annex1_countries     <- countries %>% filter(annex1) %>% pull(country_code_figaro)
  complete_countries   <- countries %>% filter(eurostat_aea_available == "complete") %>% pull(country_code_figaro)
  incomplete_countries <- countries %>% filter(eurostat_aea_available == "incomplete") %>% pull(country_code_figaro)
  estimated_countries  <- countries %>% filter(eurostat_aea_available != "complete", country_code_figaro != "GB") %>%
    pull(country_code_figaro)

  # ---- sources ----

  aea_eurostat  <- load_aea_eurostat_extension(union(years, reference_years), verbose)
  aea_gb        <- load_aea_gb_extension(years, verbose)
  aea_ch        <- load_aea_ch_extension(years, verbose)
  edgar         <- load_edgar_emissions_extension(years, verbose)
  oecd          <- load_oecd_transport_extension(years, verbose)
  pefa          <- load_pefa_eu_extension(verbose)

  # ---- CRF -> NACE allocation coefficients ----

  hh_global_coeff <- read_xlsx(file.path(resources_dir, "HH_global_coeff.xlsx")) %>%
    mutate(gas = if_else(gas_type %in% c("HFC", "PFC", "NF3_SF6"), paste0(gas_type, "_CO2E"), gas_type))

  # CRF 2.F (HFC household coefficient) computed dynamically from EU27's own AEA data (Eurostat's
  #   method) rather than a fixed constant
  hfc_eu27_2F <- aea_eurostat %>%
    filter(country %in% eu27_countries, gas == "HFC_CO2E", year %in% reference_years)
  hh_coeff_2F <- sum(hfc_eu27_2F$value[hfc_eu27_2F$industry == "HH"]) / sum(hfc_eu27_2F$value)
  hh_global_coeff <- hh_global_coeff %>%
    mutate(HH_coeff = if_else(gas == "HFC_CO2E" & trimws(ipcc_code_2006) == "2.F", hh_coeff_2F, HH_coeff))

  hh_residential_coeff <- read_xlsx(file.path(resources_dir, "HH_residential_coeff_CRF_1A4.xlsx")) %>%
    filter(time_period == "2021") %>%
    mutate(HH_coeff_1A4 = unfccc_1A4b / unfccc_1A4, ipcc_code_2006 = "1.A.4") %>%
    select(gas = gas_type, country = emis_country, ipcc_code_2006, HH_coeff_1A4)

  crf_allocation <- map_dfr(c("CO2", "CH4", "N2O", "HFC", "PFC", "NF3_SF6"), function(gas_sheet) {
    read_xlsx(file.path(resources_dir, "CRF_allocation_to_NACE.xlsx"), gas_sheet) %>%
      mutate(gas = gas_sheet)
  }) %>%
    filter(code_a64_figaro != "HH") %>%
    select(gas, ipcc_code_2006, industry = code_a64_figaro) %>%
    mutate(gas = if_else(gas %in% c("HFC", "PFC", "NF3_SF6"), paste0(gas, "_CO2E"), gas))

  # ---- households vs. industries split ----

  edgar_hh_split <- edgar %>%
    filter(country %in% estimated_countries) %>%
    left_join(hh_global_coeff %>% filter(!is.na(HH_coeff)) %>% select(gas, ipcc_code_2006, HH_coeff_global = HH_coeff),
              by = c("gas", "ipcc_code_2006")) %>%
    mutate(country_1A4 = if_else(!country %in% annex1_countries, "EU27", country)) %>%
    left_join(hh_residential_coeff %>% rename(country_1A4 = country), by = c("gas", "country_1A4", "ipcc_code_2006")) %>%
    mutate(HH_coeff_global = replace_na(HH_coeff_global, 0),
          HH_coeff_1A4 = replace_na(HH_coeff_1A4, 0),
          HH_coeff = HH_coeff_global + HH_coeff_1A4,
          industries_coeff = 1 - HH_coeff)

  if (any(is.na(edgar_hh_split$industries_coeff))) stop("Bridge table CRF -> NACE / HH incomplete")

  edgar_hh <- edgar_hh_split %>% filter(HH_coeff > 0) %>%
    mutate(value = HH_coeff * value) %>%
    group_by(year, gas, country) %>% summarise(value = sum(value), .groups = "drop") %>%
    mutate(industry = "HH")

  edgar_industries <- edgar_hh_split %>% filter(industries_coeff > 0) %>%
    mutate(value = industries_coeff * value) %>%
    select(country, ipcc_code_2006, gas, year, value)

  # ---- industries allocation to NACE A64 ----

  eu27_figaro_output <- main_aggregates_data %>%
    filter(country %in% eu27_countries, year %in% reference_years, aggregate == "PRD") %>%
    group_by(industry) %>% summarise(output = sum(value), .groups = "drop")

  eu27_aea <- aea_eurostat %>% filter(country %in% eu27_countries, year %in% reference_years) %>%
    group_by(gas, industry) %>% summarise(value = sum(value), .groups = "drop")

  eu27_intensity <- full_join(eu27_figaro_output, eu27_aea, by = "industry") %>%
    group_by(gas, industry) %>% summarise(value = sum(value), output = sum(output), .groups = "drop") %>%
    mutate(emission_intensity = value / output)

  eu27_road_fuel_intensity <- eu27_figaro_output %>%
    inner_join(pefa, by = "industry") %>%
    mutate(nrg_intensity = nrg_consumption / output, ipcc_code_2006 = "1.A.3.b_noRES")

  avg_figaro_output <- main_aggregates_data %>%
    filter(year %in% reference_years, aggregate == "PRD") %>%
    group_by(country, industry) %>% summarise(output = sum(value), .groups = "drop")

  estimated_industries <- edgar_industries %>%
    rename(emission_all_industries = value) %>%
    left_join(crf_allocation, by = c("gas", "ipcc_code_2006"), relationship = "many-to-many") %>%
    filter(!is.na(industry)) %>%
    left_join(avg_figaro_output, by = c("country", "industry")) %>%
    left_join(eu27_intensity %>% select(gas, industry, EU27_intensity = emission_intensity), by = c("gas", "industry")) %>%
    left_join(eu27_road_fuel_intensity %>% select(ipcc_code_2006, industry, EU27_intensity_1A3b_noRES = nrg_intensity),
              by = c("ipcc_code_2006", "industry")) %>%
    mutate(distr_key = case_when(ipcc_code_2006 == "1.A.3.b_noRES" ~ output * EU27_intensity_1A3b_noRES,
                                 ipcc_code_2006 == "1.A.3.a" ~ 1,
                                 TRUE ~ output * EU27_intensity)) %>%
    group_by(country, ipcc_code_2006, gas, year) %>%
    mutate(value = emission_all_industries * distr_key / sum(distr_key)) %>%
    ungroup()

  estimated_a64_before_transport <- estimated_industries %>%
    group_by(year, gas, country, industry) %>% summarise(value = sum(value), .groups = "drop")

  # ---- CO2 international maritime transport (complement to domestic H50 from CRF 1.A.3.d) ----

  edgar_sea_world <- edgar %>% filter(country == "EDGAR_SEA", gas == "CO2") %>%
    select(year, global_value = value)

  h50_international <- edgar_sea_world %>%
    inner_join(oecd$maritime, by = "year") %>%
    group_by(year) %>% mutate(value = global_value * value / sum(value)) %>% ungroup()

  # ---- CO2 air transport (replaces the EDGAR-based CRF 1.A.3.a allocation, when available) ----

  h51_from_edgar <- estimated_a64_before_transport %>%
    filter(industry == "H51_from_1.A.3.a") %>%
    rename(from_edgar_value = value) %>%
    left_join(oecd$air, by = c("year", "country")) %>%
    mutate(value = coalesce(value, from_edgar_value))

  # ---- row-binding industries + maritime + air, re-aggregating to A64 ----

  estimated_a64 <- estimated_a64_before_transport %>%
    bind_rows(h50_international %>% filter(country %in% estimated_countries) %>%
              mutate(gas = "CO2", industry = "H50") %>% select(year, gas, country, industry, value)) %>%
    filter(industry != "H51_from_1.A.3.a") %>%
    bind_rows(h51_from_edgar %>% filter(country %in% estimated_countries) %>%
              mutate(industry = "H51") %>% select(year, gas, country, industry, value)) %>%
    group_by(year, gas, country, industry) %>% summarise(value = sum(value), .groups = "drop")

  estimated_aea <- bind_rows(edgar_hh, estimated_a64)

  # ---- gap-filling of incomplete AEAs (CH, NO, TR) ----
  #  years with an official figure are used directly ; years with none are chain-linked from the
  #  EDGAR-based estimate's YoY change, anchored on the nearest available real year. For
  #  Switzerland, BFS's own AEA (no confidentiality gaps) takes priority over what CH reports to
  #  Eurostat ; BFS's combined "H50+H51" figure is split using this estimate's own H50/H51 ratio.

  ch_h50_h51_split_key <- estimated_aea %>%
    filter(country == "CH", industry %in% c("H50", "H51")) %>%
    select(gas, year, industry, split_value = value) %>%
    group_by(gas, year) %>% mutate(split_share = split_value / sum(split_value)) %>% ungroup()

  ch_h50_h51_split <- aea_ch %>%
    filter(industry == "H50H51_combined") %>%
    select(gas, year, country, combined_value = value) %>%
    inner_join(ch_h50_h51_split_key, by = c("gas", "year")) %>%
    mutate(value = combined_value * split_share) %>%
    select(gas, year, country, industry, value)

  ch_official <- aea_ch %>% filter(industry != "H50H51_combined") %>% bind_rows(ch_h50_h51_split)

  official_incomplete <- aea_eurostat %>% filter(country == "CH") %>% rename(eurostat_value = value) %>%
    full_join(ch_official %>% rename(bfs_value = value), by = c("gas", "year", "country", "industry")) %>%
    mutate(value = coalesce(bfs_value, eurostat_value)) %>%
    select(gas, year, country, industry, value) %>%
    bind_rows(aea_eurostat %>% filter(country %in% setdiff(incomplete_countries, "CH"))) %>%
    rename(obs = value)

  estimate_incomplete <- estimated_aea %>% filter(country %in% incomplete_countries) %>% rename(estimate = value)

  incomplete_filled <- full_join(official_incomplete, estimate_incomplete,
                                 by = c("gas", "year", "country", "industry")) %>%
    arrange(gas, country, industry, year) %>%
    group_by(gas, country, industry) %>%
    mutate(has_any_obs = any(!is.na(obs)),
          last_obs_year = if_else(has_any_obs, suppressWarnings(max(year[!is.na(obs)])), NA_character_),
          value = case_when(!is.na(obs) ~ obs,
                            has_any_obs ~ estimate * sum(obs * (year == last_obs_year), na.rm = TRUE) /
                              sum(estimate * (year == last_obs_year), na.rm = TRUE),
                            TRUE ~ estimate)) %>%
    ungroup() %>%
    mutate(value = if_else(is.infinite(value), NA_real_, value)) %>%
    select(gas, year, country, industry, value) %>%
    mutate(value = replace_na(value, 0)) %>%
    filter(year %in% years)

  # ---- final assembly ----

  complete_aea <- aea_eurostat %>% filter(country %in% complete_countries, year %in% years) %>%
    mutate(value = if_else(industry == "U", 0, value))

  fully_estimated <- estimated_aea %>% filter(!country %in% incomplete_countries, year %in% years)

  bind_rows(complete_aea, incomplete_filled, fully_estimated,
           aea_gb %>% select(gas, year, country, industry, value))
}

#' Aggregates a per-gas GHG estimate into a single CO2-equivalent total (Eurostat's ghgfp shape)
#'
#' GWP-100 (AR5) : CH4 x28, N2O x265 (native mass everywhere upstream) ; the F-gas groups are
#'   already expressed in CO2-equivalent.

aggregate_ghg_co2eq <- function(df_estimate) {
  df_estimate %>%
    mutate(value_co2eq = case_when(gas == "CH4" ~ value * 28,
                                   gas == "N2O" ~ value * 265,
                                   TRUE ~ value)) %>%
    group_by(year, country, industry) %>%
    summarise(value = sum(value_co2eq), .groups = "drop")
}


# ==========================================================================================
# 3. MAIN ENTRY POINT
# ==========================================================================================

#' Extends observed GHG accounts beyond Eurostat's own coverage
#'
#' @param years  years to estimate (character vector, e.g. as returned by
#'               setdiff(years$year, unique(eurostat_data$year)) in ghg_accounts_builder.R)
#' @param verbose
#'
#' @return a data frame in the same shape as ghg_accounts_builder.R's `eurostat_data`
#'         (year, country, industry, eurostat_ghg_emissions, unit)

extend_ghg_accounts_beyond_eurostat <- function(years, verbose = FALSE) {

  if (verbose) message("Estimating GHG accounts for ", paste(years, collapse = ", "),
                       " (beyond Eurostat's own coverage)")

  estimate_ghg_from_edgar(years, verbose) %>%
    aggregate_ghg_co2eq() %>%
    transmute(year, country, industry, eurostat_ghg_emissions = value * 1e3, unit = "TCO2E") # kT -> T
}
