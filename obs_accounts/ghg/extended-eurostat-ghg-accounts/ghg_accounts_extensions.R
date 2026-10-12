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
#'   in obs_accounts/ghg/extended-eurostat-ghg-accounts/resources/ - copied from
#'   InseeFrLab/global-ghg-emissions (see credit above).
#'
#' estimate_ghg_from_edgar(years, use_temp_data, verbose)


# ==========================================================================================
# SOURCES
# ==========================================================================================

resources_dir <- "obs_accounts/ghg/extended-eurostat-ghg-accounts/resources"

ghg_gases <- c(
  "CO2",
  "CH4",
  "N2O",
  "HFC_CO2E",
  "PFC_CO2E",
  "NF3_SF6_CO2E"
)

crf_gas_sheets <- c("CO2", "CH4", "N2O", "HFC", "PFC", "NF3_SF6")

ghg_reference_years <- as.character(seq(2014, 2018))

# ------------------------------------------------------------------------------------------
#' EDGAR (JRC) emissions by IPCC 2006 code and FIGARO country, for the requested years
#'
#' Downloads the 4 needed EDGAR time series (CO2, CH4, N2O, AR5g F-gases) from the JRC open-data
#'   FTP and keeps only fossil emissions.

load_edgar_emissions_extension <- function(
  years, 
  use_temp_data = TRUE,
  verbose = FALSE
) {

  edgar_base_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/EDGAR_2025_GHG"
  edgar_file_names <- c(
    "IEA_EDGAR_CO2_1970_2024",
    "EDGAR_CH4_1970_2024",
    "EDGAR_N2O_1970_2024",
    "EDGAR_AR5g_F-gases_1990_2024"
  )
  edgar_dir <- file.path(download_dir, "edgar")

  # EDGAR alpha-3 country codes mapped to FIGARO country codes.
  # Columns: Country_code_A3, country.
  edgar_country_bridge <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "edgar_countries"
    ) %>%
    select(Country_code_A3, country = country_code_figaro)

  for (file_name in edgar_file_names) {
    filepath <- file.path(edgar_dir, paste0(file_name, ".xlsx"))
    if (!file.exists(filepath) | !use_temp_data) {
      if (verbose) message("Downloading EDGAR - ", file_name)
      dir.create(edgar_dir, recursive = TRUE, showWarnings = FALSE)
      tmp_zip <- tempfile(fileext = ".zip")
      curl_download(paste0(edgar_base_url, "/", file_name, ".zip"), tmp_zip)
      unzip(tmp_zip, exdir = edgar_dir, overwrite = TRUE)
      unlink(tmp_zip)
    }
  }

  # Raw EDGAR emissions by IPCC 2006 category, country, gas, and year columns.
  edgar_raw_emissions <- map_dfr(edgar_file_names, function(file_name) {
    read_xlsx(
      file.path(edgar_dir, paste0(file_name, ".xlsx")),
      sheet = "IPCC 2006",
      skip = 9
    )
  })

  # EDGAR fossil emissions in the project's common emissions format.
  # Columns: year, country, ipcc_code_2006, gas, value.
  edgar_raw_emissions %>%
    filter(fossil_bio == "fossil") %>%
    select(
      Country_code_A3,
      ipcc_code_2006 = ipcc_code_2006_for_standard_report,
      gas = Substance,
      starts_with("Y_")
    ) %>%
    inner_join(edgar_country_bridge, by = "Country_code_A3") %>%
    select(-Country_code_A3) %>%
    pivot_longer(starts_with("Y_"), names_to = "year", values_to = "value") %>%
    mutate(year = substring(year, 3, 6)) %>%
    filter(year %in% years) %>%
    mutate(
      gas = if_else(grepl("GWP_100_AR5_", gas), paste0(substring(gas, 13), "_CO2E"), gas),
      gas = case_when(
        gas == "HCFC_CO2E" ~ "HFC_CO2E",
        gas %in% c("NF3_CO2E", "SF6_CO2E") ~ "NF3_SF6_CO2E",
        TRUE ~ gas
      )
    ) %>%
    group_by(country, ipcc_code_2006, gas, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    select(year, country, ipcc_code_2006, gas, value)
}

# ------------------------------------------------------------------------------------------
#' Official AEA reported to Eurostat (env_ac_ainah_r2), for the requested years
#'
#' Complete for EU27 (+ incomplete for CH/NO/TR, not reported at all for the rest) - used as-is
#'   for the "complete AEA" countries, and to compute the EU27 emission-intensity coefficients
#'   used to allocate EDGAR's CRF categories to NACE industries.

load_aea_eurostat_extension <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading Eurostat AEA (env_ac_ainah_r2)")

  # Eurostat AEA NACE codes mapped to FIGARO A64 industries.
  # Columns: industry, nace_r2.
  aea_nace_bridge <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "cpa_nace_a64"
    ) %>%
    select(
      industry = code_a64_figaro,
      nace_r2 = code_a64_eurostat_aea
    )

  eurostat_aea_file_path <- file.path(
    download_dir,
    paste0("env_ac_ainah_r2_", min(years), "_", max(years), ".csv")
  )

  if (!file.exists(eurostat_aea_file_path) | !use_temp_data)
  {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    eurostat_aea_raw <- get_eurostat(
      "env_ac_ainah_r2",
      filters = list(
        time = years,
        unit = "THS_T",
        airpol = ghg_gases
      ),
      cache = FALSE
    ) 
    
    write.csv(eurostat_aea_raw, eurostat_aea_file_path, row.names = FALSE)
  }

  # Raw Eurostat AEA emissions.
  eurostat_aea_raw <- read.csv(eurostat_aea_file_path)

  # Official Eurostat AEA emissions in the project's common emissions format.
  # Columns: year, country, industry, gas, value.
  eurostat_aea_raw %>%
    mutate(
      year = substring(as.character(time), 1, 4),
      country = if_else(geo == "EL", "GR", geo),
      gas = airpol,
      value = values
    ) %>%
    inner_join(aea_nace_bridge, by = "nace_r2") %>%
    select(year, country, industry, gas, value)
}

# ------------------------------------------------------------------------------------------
#' Official AEA for the United Kingdom (ONS), for the requested years
#'
#' The UK doesn't report AEA to Eurostat - ONS's own dataset is used instead (unchanged
#'   convention from ghg_accounts_builder.R's fallback logic).

load_aea_gb_extension <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading UK AEA (ONS)")

  gb_ons_url <- paste0(
    "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/",
    "ukenvironmentalaccountsatmosphericemissionsgreenhousegasemissionsbyeconomicsectorandgasunitedkingdom/",
    "current/05atmoshpericemissionsghg.xlsx"
  )

  gb_ons_filepath <- file.path(download_dir, "aea_gb.xlsx")
  if (!file.exists(gb_ons_filepath) | !use_temp_data) {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    curl_download(gb_ons_url, gb_ons_filepath)
  }

  # UK SIC group codes mapped to FIGARO A64 industries.
  # Columns used: emis_industry, sic_a88.
  gb_a88_industry_bridge <- read_xlsx(file.path(resources_dir, "bridge_a64_to_AEA_uk.xlsx"))

  gb_gas_sheets <- c("CO2", "CH4", "N2O", "HFC", "PFC", "NF3", "SF6")
  gb_ch4_gwp_ar5 <- 28
  gb_n2o_gwp_ar5 <- 265

  # Locates the "SIC(07) group" header row of the right-hand ("by group") table in column 27 -
  #   its position has already changed once between ONS file vintages, so it's found dynamically
  #   rather than hardcoded (see global-ghg-emissions history for the incident)

  find_ons_group_table_skip <- function(sheet) {
    ons_sheet_preview <- suppressMessages(read_xlsx(
      gb_ons_filepath,
      sheet = sheet,
      col_names = FALSE,
      n_max = 15
    ))

    header_row_index <- which(ons_sheet_preview[[27]] == "SIC(07) group")[1]

    if (is.na(header_row_index)) {
      stop("Could not locate the 'SIC(07) group' header row in ONS sheet '", sheet, "'")
    }

    header_row_index - 1
  }

  # Raw UK ONS emissions by gas sheet, year, and SIC group.
  gb_aea_by_sic <- map_dfr(gb_gas_sheets, function(gas_sheet) {
    read_xlsx(
      gb_ons_filepath,
      sheet = gas_sheet,
      skip = find_ons_group_table_skip(gas_sheet),
      .name_repair = "unique_quiet"
    )[-1, -c(1:26)] %>%
      pivot_longer(-1, names_to = "sic_07_group") %>%
      filter(sic_07_group != "Total") %>%
      transmute(
        year = as.character(`SIC(07) group`),
        sic_a88 = if_else(sic_07_group %in% c("100", "101"), sic_07_group, substr(sic_07_group, 1, 2)),
        value = replace_na(suppressWarnings(as.numeric(value)), 0),
        gas = case_when(
          gas_sheet %in% c("NF3", "SF6") ~ "NF3_SF6_CO2E",
          gas_sheet %in% c("HFC", "PFC") ~ paste0(gas_sheet, "_CO2E"),
          TRUE ~ gas_sheet
        )
      )
  }) %>%
    filter(year %in% years) %>%
    inner_join(
      gb_a88_industry_bridge %>% select(industry = emis_industry, sic_a88) %>% distinct(),
      by = "sic_a88"
    ) %>%
    group_by(gas, year, industry) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(
      value = case_when(
        gas == "CH4" ~ value / gb_ch4_gwp_ar5,
        gas == "N2O" ~ value / gb_n2o_gwp_ar5,
        TRUE ~ value
      ),
      gas = if_else(gas %in% c("NF3", "SF6"), "NF3_SF6_CO2E", gas)
    ) %>%
    mutate(country = "GB") %>%
    select(year, country, industry, gas, value)
}

# ------------------------------------------------------------------------------------------
#' Official AEA for Switzerland (BFS PxWeb API), for the requested years
#'
#' Switzerland reports an incomplete AEA to Eurostat (C19/C20, H50/H51, K64/K65 are
#'   confidentiality-suppressed there) - BFS's own AEA has no such gaps.
#' Water and air transport are only published combined ("50-51") - split into H50/H51 downstream
#'   in estimate_ghg_from_edgar(), using this project's own EDGAR/OECD-based split ratio between
#'   the two (BFS doesn't split them).

load_aea_ch_extension <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading Swiss AEA (BFS)")

  bfs_api_url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0204000000_104/px-x-0204000000_104.px"

  # Swiss BFS activity codes mapped to FIGARO A64 industries.
  # Columns used: bfs_code, emis_industry.
  ch_bfs_industry_bridge <- read_xlsx(file.path(resources_dir, "bridge_a64_to_AEA_ch.xlsx"))

  # Swiss BFS gas codes mapped to project gas codes and query units.
  # Columns: bfs_gas_code, gas, bfs_unit.
  ch_gas_map <- tribble(
    ~bfs_gas_code,  ~gas,           ~bfs_unit,
    "02_CO2_foss",  "CO2",          "MTONS",
    "05_CH4",       "CH4",          "MTONS",
    "04_N2O",       "N2O",          "MTONS",
    "06_HFCs",      "HFC_CO2E",     "MTEQCO2",
    "07_PFCs",      "PFC_CO2E",     "MTEQCO2",
    "08_SF6",       "NF3_SF6_CO2E", "MTEQCO2" # BFS's "SF6 and NF3" already matches our combined category
  )

  query_bfs_emissions <- function(bfs_unit, bfs_gas_codes) {
    # Jahr (year) is queried unfiltered ("all") and filtered down to `years` afterwards in R - BFS
    #  rejects the whole request with a plain-text 400 (not valid JSON) if asked for a year it
    #  hasn't published yet (e.g. still no 2024 as of 2026), which would otherwise break the query
    #  for every other dimension too instead of just yielding no rows for that year
    request_body <- toJSON(
      list(
        query = list(
          list(code = "Masseinheit", selection = list(filter = "item", values = list(bfs_unit))),
          list(code = "Wirtschaft und Haushalte", selection = list(filter = "all", values = list("*"))),
          list(code = "Gas", selection = list(filter = "item", values = as.list(bfs_gas_codes))),
          list(code = "Jahr", selection = list(filter = "all", values = list("*")))
        ),
        response = list(format = "json")
      ),
      auto_unbox = TRUE
    )

    request_handle <- new_handle()
    handle_setopt(request_handle, post = TRUE, postfields = request_body)
    handle_setheaders(request_handle, "Content-Type" = "application/json")

    api_response <- curl_fetch_memory(bfs_api_url, handle = request_handle)

    if (api_response$status_code != 200) {
      stop("BFS API request failed (HTTP ", api_response$status_code, "): ", rawToChar(api_response$content))
    }

    api_payload <- fromJSON(rawToChar(api_response$content), simplifyVector = FALSE)

    map_dfr(api_payload$data, function(row) {
      tibble(
        bfs_code = as.integer(row$key[[2]]),
        bfs_gas_code = row$key[[3]],
        year = row$key[[4]],
        value = suppressWarnings(as.numeric(row$values[[1]]))
      )
    })
  }

  ch_bfs_file_path <- file.path(
    download_dir,
    paste0("aea_ch_bfs_", min(years), "_", max(years), ".csv")
  )

  if (!file.exists(ch_bfs_file_path) | !use_temp_data)
  {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    ch_bfs_raw_emissions <- bind_rows(
      query_bfs_emissions("MTONS", ch_gas_map$bfs_gas_code[ch_gas_map$bfs_unit == "MTONS"]),
      query_bfs_emissions("MTEQCO2", ch_gas_map$bfs_gas_code[ch_gas_map$bfs_unit == "MTEQCO2"])
    )

    write.csv(ch_bfs_raw_emissions, ch_bfs_file_path, row.names = FALSE)
  }

  # Raw Swiss BFS AEA emissions by BFS activity code, gas, and year.
  # Columns: bfs_code, bfs_gas_code, year, value, gas.
  ch_bfs_raw_emissions <- read.csv(ch_bfs_file_path) %>%
    mutate(year = as.character(year)) %>%
    inner_join(ch_gas_map %>% select(bfs_gas_code, gas), by = "bfs_gas_code") %>%
    filter(!is.na(value), year %in% years)

  # Swiss BFS emissions directly mapped to FIGARO industries.
  # Columns: year, industry, gas, value.
  ch_direct_emissions <- ch_bfs_raw_emissions %>%
    inner_join(ch_bfs_industry_bridge, by = "bfs_code", relationship = "many-to-many") %>%
    group_by(gas, year, industry = emis_industry) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    select(year, industry, gas, value)

  # K66 (Activities auxiliary to financial services) : not reported separately by BFS - computed
  #   as a residual (item 53 "64-66 total" minus item 54 "64" minus item 55 "65")
  ch_k66_emissions <- ch_bfs_raw_emissions %>%
    filter(bfs_code %in% c(53, 54, 55)) %>%
    mutate(sign = if_else(bfs_code == 53, 1, -1)) %>%
    group_by(gas, year) %>%
    summarise(value = sum(value * sign), .groups = "drop") %>%
    mutate(industry = "K66") %>%
    select(year, industry, gas, value)

  # Combined Swiss BFS water and air transport emissions, split later in estimate_ghg_from_edgar().
  # Columns: year, industry, gas, value.
  ch_h50_h51_combined_emissions <- ch_bfs_raw_emissions %>%
    filter(bfs_code == 42) %>%
    transmute(year, industry = "H50H51_combined", gas, value)

  bind_rows(ch_direct_emissions, ch_k66_emissions, ch_h50_h51_combined_emissions) %>%
    mutate(country = "CH") %>%
    select(year, country, industry, gas, value)
}

# ------------------------------------------------------------------------------------------
#' OECD air transport CO2 emissions, for the requested years
#'
#' Used to replace the EDGAR-based CO2 estimate for NACE H51 - same method as Eurostat's own note.
#' Selections (found by inspecting the raw dataflows, matching what used to be picked by hand in
#'   the OECD Data Explorer) : air -> Emissions = "Air emissions accounts: air transport"
#'   (EMISSIONS_SOURCE == RES_TOTAL), Methodology = "SEEA (residence principle)".

load_oecd_air_transport_extension <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading OECD air transport CO2 emissions")

  # OECD country codes mapped to FIGARO country codes.
  # Columns: country, country_code_oecd.
  oecd_country_bridge <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "figaro_countries"
    ) %>%
    select(
      country = country_code_figaro,
      country_code_oecd
    )

  oecd_air_url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/",
    "OECD.SDD.NAD.SEEA,DSD_AIR_TRANSPORT@DF_AIR_TRANSPORT,1.0/all",
    "?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
  )

  oecd_air_file_path <- file.path(download_dir, "oecd_air_transport.csv")

  if (!file.exists(oecd_air_file_path) | !use_temp_data)
  {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    curl_download(oecd_air_url, oecd_air_file_path, quiet = !verbose)
  }

  oecd_air_raw <- fread(oecd_air_file_path)

  # OECD air transport CO2 emissions in kt CO2.
  # Columns: year, country, value.
  oecd_air_raw[
      FREQ == "A" &
      EMISSIONS_SOURCE == "RES_TOTAL" &
      METHODOLOGY == "EMISSIONS_SEEA" &
      FLIGHT_TYPE == "_T" &
      !REF_AREA %in% c("OECD", "W") &
      as.character(TIME_PERIOD) %in% years
    ] %>%
    as_tibble() %>%
    transmute(
      country_code_oecd = REF_AREA,
      year = as.character(TIME_PERIOD),
      value = OBS_VALUE / 1000
    ) %>%
    inner_join(oecd_country_bridge, by = "country_code_oecd") %>%
    select(year, country, value)
}

# ------------------------------------------------------------------------------------------
#' OECD maritime transport CO2 emissions, for the requested years
#'
#' Used to complement EDGAR's domestic-navigation figure for NACE H50 - same method as Eurostat's own note.
#' Selection (found by inspecting the raw dataflows, matching what used to be picked by hand in
#'   the OECD Data Explorer) : Frequency = Annual, Vessel = "All vessels".

load_oecd_maritime_transport_extension <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading OECD maritime transport CO2 emissions")

  # OECD country codes mapped to FIGARO country codes.
  # Columns: country, country_code_oecd.
  oecd_country_bridge <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "figaro_countries"
    ) %>%
    select(
      country = country_code_figaro,
      country_code_oecd
    )

  oecd_maritime_url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/",
    "OECD.SDD.NAD.SEEA,DSD_MARITIME_TRANSPORT@DF_MARITIME_TRANSPORT,1.0/all?",
    "dimensionAtObservation=AllDimensions",
    "&format=csvfilewithlabels"
  )

  oecd_maritime_file_path <- file.path(download_dir, "oecd_maritime_transport.csv")

  if (!file.exists(oecd_maritime_file_path) | !use_temp_data)
  {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    curl_download(oecd_maritime_url, oecd_maritime_file_path, quiet = !verbose)
  }

  oecd_maritime_raw <- fread(oecd_maritime_file_path)

  # OECD maritime transport CO2 emissions by OECD alpha-3 area.
  oecd_maritime_alpha3 <- oecd_maritime_raw %>%
    filter(
      FREQ == "A" &
      VESSEL == "ALL_VESSELS" &
      as.character(TIME_PERIOD) %in% years
     ) %>%
    as_tibble() %>%
    transmute(
      country_code_oecd = REF_AREA,
      year = as.character(TIME_PERIOD),
      value = OBS_VALUE / 1000
    ) %>%
    left_join(oecd_country_bridge, by = "country_code_oecd") %>%
    mutate(
      country = if_else(country_code_oecd == "W", "WORLD", country)
    )

  oecd_maritime_world <- oecd_maritime_alpha3 %>%
    filter(country == "WORLD") %>%
    select(year, value_world = value)

  oecd_maritime_excluding_rest_of_world <- oecd_maritime_alpha3 %>%
    filter(!is.na(country), country != "WORLD") %>%
    group_by(year) %>%
    summarise(value_excl_row = sum(value), .groups = "drop")

  # Residual rest-of-world maritime emissions needed by FIGARO.
  # Columns: year, country, value.
  oecd_maritime_rest_of_world <- oecd_maritime_world %>%
    left_join(oecd_maritime_excluding_rest_of_world, by = "year") %>%
    mutate(country = "FIGW1", value = value_world - value_excl_row) %>%
    select(year, country, value)

  # OECD maritime transport CO2 emissions in kt CO2, including FIGARO rest-of-world.
  # Columns: year, country, value.
  oecd_maritime_alpha3 %>%
    filter(!is.na(country), country != "WORLD") %>%
    select(year, country, value) %>%
    bind_rows(oecd_maritime_rest_of_world)
}

# ------------------------------------------------------------------------------------------
#' EU physical energy flow accounts (PEFA), for the reference period only
#'
#' Only used to derive the households / industries split of road transport emissions (CRF
#'   1.A.3.b_noRES) - a fixed key, averaged over the same reference period as the other
#'   CRF -> NACE allocation coefficients (2014-2018, cf. Eurostat's methodological footnote 7).

load_pefa_eu_extension <- function(
  use_temp_data = TRUE,
  verbose = FALSE
) {

  if (verbose) message("Downloading EU physical energy flow accounts (PEFA)")

  # Eurostat PEFA NACE codes mapped to FIGARO A64 industries.
  # Columns: industry, nace_r2.
  pefa_nace_bridge <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "cpa_nace_a64"
    ) %>%
    filter(!grepl("HH", code_a64_figaro)) %>%
    select(industry = code_a64_figaro, nace_r2 = code_a64_eurostat_aea)

  pefa_file_path <- file.path(download_dir, "env_ac_pefasu_2014_2015.csv")

  if (!file.exists(pefa_file_path) | !use_temp_data)
  {
    dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
    pefa_raw_energy_use <- get_eurostat(
      "env_ac_pefasu",
      filters = list(
        stk_flow = "USE",
        prod_nrg = c("P14", "P17"),
        geo = "EU27_2020",
        time = as.character(seq(2014, 2015)) # avoid confidential data
      ),
      cache = FALSE
    )
    
    write.csv(pefa_raw_energy_use, pefa_file_path, row.names = FALSE)
  }

  # Raw EU PEFA energy use from Eurostat, limited to the fuels and years used by the method.
  pefa_raw_energy_use <- read.csv(pefa_file_path)

  # EU PEFA energy use by FIGARO industry.
  # Columns: industry, nrg_consumption.
  pefa_raw_energy_use %>%
    rename(nace_r2 = nace_r2) %>%
    inner_join(pefa_nace_bridge, by = "nace_r2") %>%
    group_by(industry) %>%
    summarise(
      nrg_consumption = sum(values, na.rm = TRUE),
      .groups = "drop"
    )
}


# ==========================================================================================
# ESTIMATION (Eurostat's own CRF (Common Reporting Format) -> NACE allocation methodology)
# ==========================================================================================

#' Replicates Eurostat's FIGARO GHG-footprint methodology from EDGAR, for the requested years
#'
#' Returns one row per year x country x industry, with all gases converted to CO2-equivalent.
#' 
#' Structure (function) :
#'  - Metadata
#'  - Source data
#'  - Household Coefficients
#'  - CRF/IPCC To NACE Allocation Keys
#'  - Split EDGAR Between Households And Industries
#'  - Allocate EDGAR Industry Emissions To NACE Industries
#'  - Transport Adjustments
#'  - Assemble Estimated AEA
#'  - Fill Incomplete Official AEAs
#'  - Final Assembly
#'  - CO2e Conversion
#' 

estimate_ghg_from_edgar <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  # ======================================================================================
  # Metadata

  # FIGARO country metadata, including Eurostat AEA coverage and Annex I flags.
  figaro_country_metadata <- read_xlsx(
      file.path(resources_dir, "nomenclatures.xlsx"),
      "figaro_countries"
    ) %>%
    mutate(annex1 = as.logical(annex1))

  # FIGARO country codes for EU27 countries, used to build Eurostat allocation intensities.
  eu27_country_codes <- figaro_country_metadata %>%
    filter(EU27_esa_code == "SIS21") %>%
    pull(country_code_figaro)

  # FIGARO country codes for Annex I countries, used for country-specific residential coefficients.
  annex1_country_codes <- figaro_country_metadata %>%
    filter(annex1) %>%
    pull(country_code_figaro)

  # Countries with complete official Eurostat AEA; these are used directly.
  complete_aea_country_codes <- figaro_country_metadata %>%
    filter(eurostat_aea_available == "complete") %>%
    pull(country_code_figaro)

  # Countries with incomplete official Eurostat AEA; missing values are filled from estimates.
  incomplete_aea_country_codes <- figaro_country_metadata %>%
    filter(eurostat_aea_available == "incomplete") %>%
    pull(country_code_figaro)

  # Countries estimated from EDGAR rather than complete official Eurostat AEA.
  estimated_country_codes <- figaro_country_metadata %>%
    filter(eurostat_aea_available != "complete", country_code_figaro != "GB") %>%
    pull(country_code_figaro)

  # ======================================================================================
  # Source Data

  # --------------------------------------------------
  # FIGARO Economic data

  # Raw FIGARO main aggregates for the reference years.
  # Columns: id, year, country, industry, aggregate, value.
  figaro_main_aggregates_raw <- map_dfr(
    ghg_reference_years,
    load_local_figaro_main_aggregates
  )

  # FIGARO production/output aggregate (PRD) used to build allocation keys.
  # Columns: year, country, industry, output.
  figaro_output_reference <- figaro_main_aggregates_raw %>%
    filter(
      year %in% ghg_reference_years,
      aggregate == "PRD"
    ) %>%
    mutate(
      output = value
    ) %>%
    select(year, country, industry, output)

  # --------------------------------------------------
  # AEA emissions data

  # Official Eurostat Air Emission Accounts.
  # Columns: year, country, industry, gas, value.
  eurostat_aea_emissions <- load_aea_eurostat_extension(
      union(years, ghg_reference_years),
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, industry, gas, value)

  # Official UK Air Emission Accounts from ONS.
  # Columns: year, country, industry, gas, value.
  gb_aea_emissions <- load_aea_gb_extension(
      years,
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, industry, gas, value)

  # Official Swiss Air Emission Accounts from BFS.
  # Columns: year, country, industry, gas, value.
  ch_aea_emissions <- load_aea_ch_extension(
      years,
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, industry, gas, value)

  # --------------------------------------------------
  # EDGAR emissions data

  # EDGAR emissions by country, IPCC 2006 category, gas, and year.
  # Columns: year, country, ipcc_code_2006, gas, value.
  edgar_emissions_by_ipcc <- load_edgar_emissions_extension(
      years,
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, ipcc_code_2006, gas, value)

  # --------------------------------------------------
  # OECD transport emissions data

  # OECD CO2 emissions for air transport.
  # Columns: year, country, value.
  oecd_air_transport_emissions <- load_oecd_air_transport_extension(
      years,
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, value)

  # OECD CO2 emissions for maritime transport.
  # Columns: year, country, value.
  oecd_maritime_transport_emissions <- load_oecd_maritime_transport_extension(
      years,
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(year, country, value)

  # --------------------------------------------------
  # Energy use data

  # EU physical energy flow accounts used for road transport allocation.
  # Columns: industry, nrg_consumption.
  pefa_energy_use <- load_pefa_eu_extension(
      use_temp_data = use_temp_data,
      verbose = verbose
    ) %>%
    select(industry, nrg_consumption)

  # ======================================================================================
  # Household Coefficients

  # --------------------------------------------------
  # Household coefficients

  # -------------------------
  # Global household coefficients

  # Default household share by gas and IPCC category.
  # Columns: gas, ipcc_code_2006, household_coeff.
  household_global_coefficients <- read_xlsx(file.path(resources_dir, "HH_global_coeff.xlsx")) %>%
    mutate(
      gas = if_else(gas_type %in% c("HFC", "PFC", "NF3_SF6"), paste0(gas_type, "_CO2E"), gas_type)
    ) %>%
    filter(!is.na(HH_coeff)) %>%
    select(gas, ipcc_code_2006, household_coeff = HH_coeff)

  # EU27-derived household share for HFC / CRF 2.F, replacing the fixed global coefficient.
  # Scalar: household share of EU27 HFC emissions over the reference years.
  hfc_2f_household_coefficient <- eurostat_aea_emissions %>%
    filter(
      country %in% eu27_country_codes,
      gas == "HFC_CO2E",
      year %in% ghg_reference_years
    ) %>%
    summarise(household_coeff = sum(value[industry == "HH"]) / sum(value)) %>%
    pull(household_coeff)

  household_global_coefficients <- household_global_coefficients %>%
    mutate(
      household_coeff = if_else(
        gas == "HFC_CO2E" & ipcc_code_2006 == "2.F",
        hfc_2f_household_coefficient,
        household_coeff
      )
    ) %>%
    filter(!is.na(household_coeff)) %>%
    select(gas, ipcc_code_2006, household_coeff)

  # -------------------------
  # Residential household coefficients

  # Country-specific household share for CRF 1.A.4 from residential emissions.
  # Columns: gas, ipcc_code_2006, country, household_coeff.
  household_residential_coefficients <- read_xlsx(file.path(resources_dir, "HH_residential_coeff_CRF_1A4.xlsx")) %>%
    filter(time_period == "2021") %>%
    mutate(
      gas = gas_type,
      country = emis_country,
      household_coeff = unfccc_1A4b / unfccc_1A4,
      ipcc_code_2006 = "1.A.4"
    ) %>%
    filter(!is.na(household_coeff)) %>%
    select(gas, ipcc_code_2006, country, household_coeff)

  # -------------------------
  # Final household coefficients

  # Household share for each EDGAR country, gas, and IPCC category.
  # Cascade: residential coefficient, otherwise global coefficient, otherwise 0.
  # Columns: country, ipcc_code_2006, gas, household_coeff.
  household_coefficients <- edgar_emissions_by_ipcc %>%
    distinct(country, ipcc_code_2006, gas) %>%
    filter(country %in% estimated_country_codes) %>%
    left_join(
      household_global_coefficients %>% rename(global_household_coeff = household_coeff),
      by = c("gas", "ipcc_code_2006")
    ) %>%
    mutate(
      country_1A4 = if_else(!country %in% annex1_country_codes, "EU27", country)
    ) %>%
    left_join(
      household_residential_coefficients %>%
        rename(residential_household_coeff = household_coeff, country_1A4 = country),
      by = c("gas", "ipcc_code_2006", "country_1A4")
    ) %>%
    mutate(
      household_coeff = coalesce(residential_household_coeff, global_household_coeff, 0)
    ) %>%
    select(country, ipcc_code_2006, gas, household_coeff)

  # ======================================================================================
  # CRF/IPCC To NACE Allocation Keys

  # -------------------------
  # CRF/IPCC to NACE bridge

  # Candidate NACE/FIGARO industries for each gas and IPCC category.
  # Columns: gas, ipcc_code_2006, industry.
  crf_to_nace_bridge <- map_dfr(crf_gas_sheets, function(gas_sheet) {
      read_xlsx(
        file.path(resources_dir, "CRF_allocation_to_NACE.xlsx"),
        gas_sheet,
        .name_repair = "unique_quiet"
      ) %>%
        mutate(gas = gas_sheet)
    }) %>%
    filter(code_a64_figaro != "HH") %>%
    mutate(
      gas = if_else(gas %in% c("HFC", "PFC", "NF3_SF6"), paste0(gas, "_CO2E"), gas),
      industry = code_a64_figaro
    ) %>%
    select(gas, ipcc_code_2006, industry)

  # -------------------------
  # Reference outputs and intensities

  # Country and industry output over the reference years.
  # Columns: country, industry, output.
  industry_outputs_reference <- figaro_output_reference %>%
    filter(
      year %in% ghg_reference_years
    ) %>%
    group_by(country, industry) %>%
    summarise(
      output = sum(output),
      .groups = "drop"
    ) %>%
    select(country, industry, output)

  # EU27 emission intensity by industry and gas over the reference years.
  # Columns: industry, gas, emission_intensity.
  eu27_industry_emission_intensities <- eurostat_aea_emissions %>%
    inner_join(
      figaro_output_reference,
      by = c("year", "country", "industry")
    ) %>%
    filter(
      country %in% eu27_country_codes,
      year %in% ghg_reference_years
    ) %>%
    group_by(industry, gas) %>%
    summarise(
      value = sum(value),
      output = sum(output),
      .groups = "drop"
    ) %>%
    mutate(
      emission_intensity = value / output
    ) %>%
    select(industry, gas, emission_intensity)

  # EU27 energy intensity by industry for road transport allocation.
  # Columns: ipcc_code_2006, industry, nrg_intensity.
  eu27_road_energy_intensities <- industry_outputs_reference %>%
    filter(
      country %in% eu27_country_codes
    ) %>%
    group_by(industry) %>%
    summarise(
      output = sum(output),
      .groups = "drop"
    ) %>%
    select(industry, output) %>%
    inner_join(pefa_energy_use, by = "industry") %>%
    mutate(
      nrg_intensity = nrg_consumption / output,
      ipcc_code_2006 = "1.A.3.b_noRES"
    ) %>%
    select(ipcc_code_2006, industry, nrg_intensity)

  # -------------------------
  # Default allocation keys

  # Allocation keys based on country output and EU27 emission intensities.
  # Columns: country, ipcc_code_2006, gas, industry, distr_key.
  default_industry_allocation_keys <- crf_to_nace_bridge %>%
    filter(!ipcc_code_2006 %in% c("1.A.3.a", "1.A.3.b_noRES")) %>%
    left_join(industry_outputs_reference, by = c("industry"), relationship = "many-to-many") %>%
    left_join(eu27_industry_emission_intensities, by = c("industry", "gas")) %>%
    mutate(
      weight = output * emission_intensity
    ) %>%
    group_by(country, ipcc_code_2006, gas) %>%
    mutate(distr_key = weight / sum(weight)) %>%
    ungroup() %>%
    select(country, ipcc_code_2006, gas, industry, distr_key)

  # -------------------------
  # Road transport allocation keys

  # Allocation keys based on country output and EU27 PEFA energy intensities.
  # Columns: country, ipcc_code_2006, gas, industry, distr_key.
  road_transport_allocation_keys <- crf_to_nace_bridge %>%
    filter(ipcc_code_2006 == "1.A.3.b_noRES") %>%
    left_join(industry_outputs_reference, by = c("industry"), relationship = "many-to-many") %>%
    left_join(eu27_road_energy_intensities, by = c("ipcc_code_2006", "industry")) %>%
    mutate(
      weight = output * nrg_intensity
    ) %>%
    group_by(country, ipcc_code_2006, gas) %>%
    mutate(distr_key = weight / sum(weight)) %>%
    ungroup() %>%
    select(country, ipcc_code_2006, gas, industry, distr_key)

  # -------------------------
  # Civil aviation allocation keys

  # Temporary one-to-one allocation keys for CRF 1.A.3.a, replaced later by OECD air transport.
  # Columns: country, ipcc_code_2006, gas, industry, distr_key.
  civil_aviation_allocation_keys <- crf_to_nace_bridge %>%
    filter(ipcc_code_2006 == "1.A.3.a") %>%
    left_join(industry_outputs_reference, by = c("industry"), relationship = "many-to-many") %>%
    mutate(
      distr_key = 1
    ) %>%
    select(country, ipcc_code_2006, gas, industry, distr_key)

  # -------------------------
  # Final allocation keys

  # Final allocation keys used to distribute EDGAR industry emissions to NACE industries.
  # Columns: country, ipcc_code_2006, gas, industry, distr_key.
  industry_allocation_keys <- bind_rows(
    default_industry_allocation_keys,
    road_transport_allocation_keys,
    civil_aviation_allocation_keys
  )

  # ======================================================================================
  # Split EDGAR Between Households And Industries

  # EDGAR emissions allocated to households.
  # Columns: year, country, industry, gas, value.
  edgar_household_emissions <- edgar_emissions_by_ipcc %>%
    inner_join(
      household_coefficients,
      by = c("country", "ipcc_code_2006", "gas")
    ) %>%
    mutate(
      value = value * household_coeff
    ) %>%
    group_by(year, country, gas) %>%
    summarise(
      value = sum(value),
      .groups = "drop"
    ) %>%
    mutate(industry = "HH") %>%
    select(year, country, industry, gas, value)

  # --------------------------------------------------

  # EDGAR emissions kept for industries, still by IPCC category before NACE allocation.
  # Columns: year, country, ipcc_code_2006, gas, value.
  edgar_industry_emissions_by_ipcc <- edgar_emissions_by_ipcc %>%
    inner_join(
      household_coefficients,
      by = c("country", "ipcc_code_2006", "gas")
    ) %>%
    mutate(
      value = value * (1 - household_coeff)
    ) %>%
    select(year, country, ipcc_code_2006, gas, value)

  # ======================================================================================
  # Allocate EDGAR Industry Emissions To NACE Industries

  # EDGAR industry emissions allocated from IPCC categories to NACE/FIGARO industries.
  # Columns: year, country, industry, gas, value.
  estimated_industry_emissions <- edgar_industry_emissions_by_ipcc %>%
    left_join(
      industry_allocation_keys,
      by = c("country", "ipcc_code_2006", "gas"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      value = value * distr_key
    ) %>%
    group_by(year, gas, country, industry) %>%
    summarise(
      value = sum(value), 
      .groups = "drop"
    ) %>%
    select(year, country, industry, gas, value)

  # ======================================================================================
  # Transport Adjustments

  # -------------------------
  # International maritime transport

  # Global EDGAR CO2 emissions for international maritime transport.
  # Columns: year, global_value.
  edgar_international_maritime_emissions <- edgar_emissions_by_ipcc %>%
    filter(
      country == "EDGAR_SEA",
      gas == "CO2"
    ) %>%
    select(year, global_value = value)

  # EDGAR international maritime emissions allocated to countries with OECD maritime shares.
  # Columns: year, country, industry, gas, value.
  h50_international_maritime_emissions <- oecd_maritime_transport_emissions %>%
    inner_join(edgar_international_maritime_emissions, by = "year") %>%
    group_by(year) %>%
    mutate(
      value = global_value * value / sum(value)
    ) %>%
    ungroup() %>%
    filter(country %in% estimated_country_codes) %>%
    mutate(
      gas = "CO2", 
      industry = "H50"
    ) %>%
    select(year, country, industry, gas, value)

  # -------------------------
  # Air transport

  # H51 air transport emissions, replacing EDGAR-based CRF 1.A.3.a with OECD where available.
  # Columns: year, country, industry, gas, value.
  h51_air_transport_emissions <- estimated_industry_emissions %>%
    filter(industry == "H51_from_1.A.3.a") %>%
    rename(value_from_edgar = value) %>%
    left_join(oecd_air_transport_emissions, by = c("year", "country")) %>%
    rename(value_from_oecd = value) %>%
    mutate(
      value = coalesce(value_from_oecd, value_from_edgar),
      industry = "H51"
    ) %>%
    select(year, country, industry, gas, value)

  # -------------------------
  # Adjusted industry emissions

  # Industry emissions after maritime and air transport adjustments.
  # Columns: year, country, industry, gas, value.
  estimated_industry_emissions_adjusted <- estimated_industry_emissions %>%
    filter(industry != "H51_from_1.A.3.a") %>%
    bind_rows(
      h50_international_maritime_emissions,
      h51_air_transport_emissions %>% filter(country %in% estimated_country_codes)
    ) %>%
    group_by(year, gas, country, industry) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    select(year, country, industry, gas, value)

  # ======================================================================================
  # Assemble Estimated AEA

  # Estimated AEA emissions from EDGAR-based household and industry estimates.
  # Columns: year, country, industry, gas, value.
  estimated_aea_emissions <- bind_rows(
    edgar_household_emissions, 
    estimated_industry_emissions_adjusted
  ) %>%
    select(year, country, industry, gas, value)

  # ======================================================================================
  # Fill Incomplete Official AEAs

  # Years with official values are used directly; missing years are chain-linked from
  # EDGAR-based estimates and anchored on the latest available official year.

  # -------------------------
  # Switzerland official AEA

  # H50/H51 split shares from estimated AEA, used to split BFS combined water/air transport.
  # Columns: gas, year, industry, split_value, split_share.
  ch_h50_h51_split_shares <- estimated_aea_emissions %>%
    filter(
      country == "CH",
      industry %in% c("H50", "H51")
    ) %>%
    select(gas, year, industry, split_value = value) %>%
    group_by(gas, year) %>%
    mutate(split_share = split_value / sum(split_value)) %>%
    ungroup()

  # Swiss BFS combined H50/H51 emissions split into H50 and H51.
  # Columns: year, country, industry, gas, value.
  ch_h50_h51_emissions <- ch_aea_emissions %>%
    filter(industry == "H50H51_combined") %>%
    select(gas, year, country, combined_value = value) %>%
    inner_join(ch_h50_h51_split_shares, by = c("gas", "year")) %>%
    mutate(value = combined_value * split_share) %>%
    select(year, country, industry, gas, value)

  # Swiss official AEA from BFS, with combined H50/H51 replaced by split values.
  # Columns: year, country, industry, gas, value.
  ch_official_aea_emissions <- ch_aea_emissions %>%
    filter(industry != "H50H51_combined") %>%
    bind_rows(ch_h50_h51_emissions) %>%
    select(year, country, industry, gas, value)

  # -------------------------
  # Official incomplete AEA

  # Official AEA for incomplete countries, with Swiss BFS values preferred over Eurostat.
  # Columns: year, country, industry, gas, obs.
  incomplete_official_aea_emissions <- eurostat_aea_emissions %>%
    filter(country == "CH") %>%
    rename(eurostat_value = value) %>%
    full_join(
      ch_official_aea_emissions %>% rename(bfs_value = value),
      by = c("gas", "year", "country", "industry")
    ) %>%
    mutate(value = coalesce(bfs_value, eurostat_value)) %>%
    select(year, country, industry, gas, value) %>%
    bind_rows(eurostat_aea_emissions %>% filter(country %in% setdiff(incomplete_aea_country_codes, "CH"))) %>%
    rename(obs = value)

  # EDGAR-based estimates for countries with incomplete official AEA.
  # Columns: year, country, industry, gas, estimate.
  incomplete_estimated_aea_emissions <- estimated_aea_emissions %>%
    filter(country %in% incomplete_aea_country_codes) %>%
    rename(estimate = value)

  # Official incomplete AEA filled with EDGAR-based estimates.
  # Columns: year, country, industry, gas, value.
  incomplete_filled_aea_emissions <- full_join(
      incomplete_official_aea_emissions,
      incomplete_estimated_aea_emissions,
      by = c("gas", "year", "country", "industry")
    ) %>%
    arrange(gas, country, industry, year) %>%
    group_by(gas, country, industry) %>%
    mutate(
      has_any_obs = any(!is.na(obs)),
      last_obs_year = if_else(has_any_obs, suppressWarnings(max(year[!is.na(obs)])), NA_character_),
      value = case_when(
        !is.na(obs) ~ obs,
        has_any_obs ~ estimate * sum(obs * (year == last_obs_year), na.rm = TRUE) /
          sum(estimate * (year == last_obs_year), na.rm = TRUE),
        TRUE ~ estimate
      )
    ) %>%
    ungroup() %>%
    mutate(value = if_else(is.infinite(value), NA_real_, value)) %>%
    select(year, country, industry, gas, value) %>%
    mutate(value = replace_na(value, 0)) %>%
    filter(year %in% years)

  # ======================================================================================
  # Final Assembly

  # Complete official Eurostat AEA used directly.
  # Columns: year, country, industry, gas, value.
  complete_official_aea_emissions <- eurostat_aea_emissions %>%
    filter(
      country %in% complete_aea_country_codes,
      year %in% years
    ) %>%
    mutate(
      value = if_else(industry == "U", 0, value)
    ) %>%
    select(year, country, industry, gas, value)

  # Fully estimated AEA for countries without incomplete official AEA handling.
  # Columns: year, country, industry, gas, value.
  fully_estimated_aea_emissions <- estimated_aea_emissions %>%
    filter(!country %in% incomplete_aea_country_codes, year %in% years) %>%
    select(year, country, industry, gas, value)

  # Final AEA emissions by gas, combining official, filled, estimated, and UK sources.
  # Columns: year, country, industry, gas, value.
  final_aea_emissions_by_gas <- bind_rows(
    complete_official_aea_emissions,
    incomplete_filled_aea_emissions,
    fully_estimated_aea_emissions,
    gb_aea_emissions
  ) %>%
    select(year, country, industry, gas, value)

  # ======================================================================================
  # CO2e Conversion

  # Final GHG emissions converted to CO2 equivalent and aggregated across gases.
  # Columns: year, country, industry, value, unit.
  final_ghg_emissions_co2e <- final_aea_emissions_by_gas %>%
    mutate(
      value_co2eq = case_when(
        gas == "CH4" ~ value * 28,
        gas == "N2O" ~ value * 265,
        TRUE ~ value
      )
    ) %>%
    group_by(year, country, industry) %>%
    summarise(
      value = sum(value_co2eq),
      .groups = "drop"
    ) %>% 
    mutate(
      unit = "TCO2E"
    )

  final_ghg_emissions_co2e
}
