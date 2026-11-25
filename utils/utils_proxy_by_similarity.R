# La Societe Nouvelle

# ----------------------------------------------------------------------------------------------------
# proxy_missing_value_by_similarity
#
# Estimates missing values in a raw vector using a similarity index approach.
#
# Methods:
#   - VAFC : Value-Added
#   - PROD : Production
#   - EMPE : Employees
#
# @param raw_vector A data frame containing country, industry, value, and flag columns.
# @param basis An integer defining the reference year (default: 2018).
# @param parallelize A logical value indicating whether to parallelize computation (default: TRUE).
# @param proxy A character string specifying the proxy key ('VAFC', 'PROD', 'EMPE').
# Return A modified version of `raw_vector` with missing values estimated.
# Used compute_similarity

proxy_missing_value_by_similarity = function(
  raw_vector,
  indic_i,
  year_basis = 2018,
  parallelize = TRUE,
  proxy = "VAFC",
  min_value = -Inf,
  max_value = Inf,
  verbose = TRUE,
  workers = getOption("lsn.proxy_workers", max(1, detectCores() - 1))
) {
  # --------------------------------------------------
  # Utils

  # ...

  # -------------------------
  # Similarity mode

  if(!proxy %in% c("VAFC", "PROD", "EMPE")) {
    stop("Please pick one of the following proxy key : VAFC (value-added), PROD (production), EMPE (Employees)")
  }

  # -------------------------
  # Data type (intensity / rate / index)

  type <- case_when(
    indic_i %in% c("GHG","HAZ","MAT","NRG","WAS","WAT") ~ "intensity",
    TRUE ~ "other"
  )

  # --------------------------------------------------
  # Missing data

  missing_values_indexes <- which(is.na(raw_vector$value))
  # missing_values_indexes <- c(1052) # debug

  if (length(missing_values_indexes) == 0) {
    if (verbose) print("No missing value")
    return(raw_vector)
  }

  if (verbose) cat(paste0("Missing value(s) : ", length(missing_values_indexes)),"\n")

  # --------------------------------------------------
  # Metadata

  years <- unique(raw_vector$year)

  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      country = code,
    ) %>%
    select(country)

  insee_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      nace_niv5 = code,
      nace_niv4 = classe,
      nace_niv3 = groupe,
      nace_niv2 = division
    ) %>%
    mutate(across(everything(), ~ gsub("\\.", "", .x))) %>%
    select(nace_niv5,nace_niv4,nace_niv3,nace_niv2,industry)

  nace_lookup <- insee_nace_niv5 %>%
    pivot_longer(
      cols = c(nace_niv5, nace_niv4, nace_niv3, nace_niv2),
      names_to = "nace_level",
      values_to = "nace_code"
    ) %>%
    select(nace_code, nace_level, industry) %>%
    distinct()

  # --------------------------------------------------
  # OECD SBS Data

  # -------------------------
  # Fetching data (or using cache)

  file <- list.files(dirname(tempdir()), recursive = T, full.names = T) %>% subset(grepl(paste0("SBSDATA",proxy),.))

  if (length(file) == 1) {
    sbs_raw_data <- readRDS(file)
  } else {
    # URL SBS data (OECD)
    base_url_oecd_sbs_data <- paste0(
      "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_SdbSBSC_ISIC4@DF_SdbS_ISIC4,1.0/",
      "A..",proxy,".._T.","?",
      "startPeriod=","2010",
      "&dimensionAtObservation=AllDimensions",
      "&format=csvfilewithlabels"
    )
    # Fetching data
    sbs_raw_data <- read.csv(
      base_url_oecd_sbs_data,
      check.names = F
    )
    # Save RDS
    saveRDS(sbs_raw_data, tempfile(pattern = paste0("SBSDATA",proxy), fileext = ".rds"))
    if (verbose) print(paste0("SBS data cached for proxy : ",proxy))
  }

  # -------------------------
  # clean data

  sbs_data <- sbs_raw_data %>%
    mutate(
      year = TIME_PERIOD,
      country = countrycode(REF_AREA,"iso3c","iso2c",warn = F,nomatch = NULL),
      activity = ACTIVITY,
      nace_code = substring(ACTIVITY,2),
      value = OBS_VALUE
    ) %>%
    select(year, country, activity, nace_code, value)

  # -------------------------
  # build dataframe with activity share by industry

  base_grid <- crossing(
      year = year_basis,
      country = figaro_countries$country
    ) %>%
    merge(nace_lookup)

  oecd_sbs_data <- base_grid %>%
    left_join(sbs_data, by = c("year", "country", "nace_code")) %>%
    # remove division matching FIGARO industry (false similarity)
    filter(nace_code != substring(industry,2)) %>%
    # remove incomplete industry at each level
    group_by(year, country, industry, nace_level) %>%
    filter(!any(is.na(value))) %>%
    # compute activity distribution (by industry, at each level)
    group_by(year, country, industry, nace_level) %>%
    filter(sum(value) != 0) %>%
    mutate(
      share = value / sum(value)
    ) %>%
    ungroup() %>%
    select(year, country, industry, nace_level, nace_code, share) %>%
    arrange(country, industry, nace_level, nace_code)

  # --------------------------------------------------
  # FIGARO Main aggregates data

  main_aggregates_data_raw <- map_dfr(
    years,
    load_local_figaro_main_aggregates
  )

  figaro_main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, NVA)

  # --------------------------------------------------
  # Computations

  if (verbose) cat("Building missing data...\n")

  # -------------------------
  # Parallel mode

  # Missing cells by country x industry series.
  missing_keys <- split(
    missing_values_indexes,
    paste(raw_vector$country[missing_values_indexes], raw_vector$industry[missing_values_indexes])
  )

  if (parallelize)
  {
    old_plan <- plan()
    old_progressr_enable <- getOption("progressr.enable")

    on.exit(plan(old_plan), add = TRUE)
    on.exit(options(progressr.enable = old_progressr_enable), add = TRUE)

    suppressPackageStartupMessages(
      suppressMessages(
        registerDoFuture()
      )
    )
    plan(multisession, workers = workers)

    options(progressr.enable = TRUE)
    handlers("txtprogressbar")

    results <- with_progress({
      # Progress bar
      p <- progressor(along = missing_keys)
      # Computations
      foreach(
        key_indexes = missing_keys,
        .combine = rbind,
        .options.future = list(
          packages = c("dplyr", "progressr"),
          conditions = structure("condition", exclude = "message")
        )
      ) %dofuture% {
        # Compute proxy data
        proxy_data <- get_proxy_values_by_similarity(
          raw_vector = raw_vector,
          type = type,
          indexes = key_indexes,
          oecd_sbs_data = oecd_sbs_data,
          figaro_main_aggregates_data = figaro_main_aggregates_data,
          min_value = min_value,
          max_value = max_value
        )
        # progress
        p(sprintf("%s %s", raw_vector$country[key_indexes[1]], raw_vector$industry[key_indexes[1]]))
        proxy_data
      }
    }, enable = TRUE)
  }

  # -------------------------
  # Sequential mode

  else
  {
    old_progressr_enable <- getOption("progressr.enable")

    on.exit(options(progressr.enable = old_progressr_enable), add = TRUE)

    options(progressr.enable = TRUE)
    handlers("txtprogressbar")

    results <- with_progress({
      p <- progressor(along = missing_keys)

      foreach(
        key_indexes = missing_keys,
        .combine = rbind
      ) %do% {
        # Compute proxy data
        proxy_data <- get_proxy_values_by_similarity(
          raw_vector = raw_vector,
          type = type,
          indexes = key_indexes,
          oecd_sbs_data = oecd_sbs_data,
          figaro_main_aggregates_data = figaro_main_aggregates_data,
          min_value = min_value,
          max_value = max_value
        )
        # progress
        p(sprintf("%s %s", raw_vector$country[key_indexes[1]], raw_vector$industry[key_indexes[1]]))
        proxy_data
      }
    }, enable = TRUE)
  }

  # check if error
  failed <- results$index[is.na(results$value)]
  if (length(failed) > 0) {
    i <- failed[1]
    stop(paste0("ERROR PROXY ", i, " - ", raw_vector$country[i], " ", raw_vector$industry[i]))
  }

  # assign results to complete dataframe
  raw_vector$value[results$index] <- results$value
  raw_vector$flag[results$index]  <- "e"

  # --------------------------------------------------

  # format data
  vector <- raw_vector

  # print(vector %>% as_tibble())
  # print(vector %>% filter(is.na(value)) %>% as_tibble())

  return(vector)
}


# get_proxy_values_by_similarity() --------------------------------------------
# Missing cells of one country x industry series (indexes), from a panel computed once.

get_proxy_values_by_similarity = function(
  raw_vector,
  type, # extension type (intensity/index/rate)
  indexes,
  oecd_sbs_data,
  figaro_main_aggregates_data,
  num_proxy = 5,
  min_value = -Inf,
  max_value = Inf
) {
  # --------------------------------------------------

  country_i <- raw_vector$country[indexes[1]]
  industry_i <- raw_vector$industry[indexes[1]]

  # --------------------------------------------------
  # Impacts data for industry

  period_sectoral_vector <- raw_vector %>%
    filter(
      !is.na(value),
      industry == industry_i,
      country != country_i
    ) %>%
    select(year,country,industry,value)

  countries_with_data_over_period <- unique(period_sectoral_vector$country)

  # Value added of the series, by year
  series_va <- figaro_main_aggregates_data %>%
    filter(country == country_i, industry == industry_i) %>%
    select(year, NVA)

  # --------------------------------------------------
  # Weighted mean of donors' values (or intensities) over all their years

  compute_proxy_value <- function(similar_countries) {
    proxy_source <- period_sectoral_vector %>%
      merge(similar_countries)

    if (type == "intensity") {
      proxy_source <- proxy_source %>%
        merge(figaro_main_aggregates_data) %>% # by year, country, industry
        mutate(value = value / NVA)
    }

    proxy_value <- proxy_source %>%
      filter(is.finite(value), is.finite(similarity_score)) %>%
      summarise(
        value = sum(value * similarity_score) / sum(similarity_score),
        .groups = "drop"
      ) %>%
      pull(value)

    if (length(proxy_value) == 0 || !is.finite(proxy_value)) {
      proxy_value <- if (is.finite(min_value)) min_value else 0
    }

    proxy_value
  }

  # --------------------------------------------------
  # Similar countries, computed at the first cell with value added

  similar_countries <- NULL
  base_proxy_value <- NULL

  get_base_panel <- function() {
    sbs_subset <- oecd_sbs_data %>%
      filter(industry == industry_i, country == country_i)

    sbs_subset_candidates <- oecd_sbs_data %>%
      filter(industry == industry_i, country %in% countries_with_data_over_period)

    use_sbs_data <-
      nrow(sbs_subset) > 0 &&
      !all(is.na(sbs_subset$share)) &&
      nrow(sbs_subset_candidates) > 0 &&
      !all(is.na(sbs_subset_candidates$share))

    # SBS similarity, else FIGARO (share of industry VA in GDP)
    panel <- if (use_sbs_data) {
      get_similar_countries_sbs(country_i, industry_i, countries_with_data_over_period, oecd_sbs_data) %>%
        head(num_proxy)
    }

    if (is.null(panel) || nrow(panel) == 0) {
      panel <- get_similar_countries_figaro(country_i, industry_i, countries_with_data_over_period, figaro_main_aggregates_data) %>%
        head(num_proxy)
    }

    panel
  }

  # --------------------------------------------------
  # Values by missing cell

  values <- vapply(indexes, function(index) {
    year_i <- raw_vector$year[index]
    industry_va <- series_va$NVA[series_va$year == year_i]

    if (length(industry_va) != 1) return(NA_real_)

    # No value added
    if (industry_va == 0) return(0)

    if (is.null(similar_countries)) {
      similar_countries <<- get_base_panel()
      if (nrow(similar_countries) > 0) base_proxy_value <<- compute_proxy_value(similar_countries)
    }

    if (!is.null(base_proxy_value)) {
      proxy_value <- base_proxy_value
    } else {
      # Fallback panels: reporting countries of the year, then of the period
      countries <- period_sectoral_vector$country[period_sectoral_vector$year == year_i]
      if (length(countries) == 0) countries <- head(countries_with_data_over_period, num_proxy)

      proxy_value <- compute_proxy_value(data.frame(country = countries, similarity_score = rep(1, length(countries))))
    }

    value <- if (type == "intensity") proxy_value * industry_va else proxy_value
    pmin(pmax(value, min_value), max_value)
  }, numeric(1))

  data.frame(
    index = indexes,
    value = values
  )
}

get_similar_countries_sbs <- function(
  country_i,
  industry_i,
  candidate_countries,
  oecd_sbs_data
) {
  ref_sbs_shares <- oecd_sbs_data %>%
    filter(
      industry == industry_i,
      country == country_i
    ) %>%
    transmute(nace_level, nace_code, target_share = share)

  oecd_sbs_data %>%
    filter(
      industry == industry_i,
      country %in% candidate_countries
    ) %>%
    select(country, nace_level, nace_code, share) %>%
    merge(ref_sbs_shares) %>%
    mutate(
      distance = abs(coalesce(share, 0) - coalesce(target_share, 0))
    ) %>%
    group_by(country, nace_level) %>%
    summarise(
      distance = sum(distance),
      .groups = "drop"
    ) %>%
    group_by(country) %>%
    summarise(
      level_rank = case_when(
        any(nace_level == "nace_niv5") ~ "nace_niv5",
        any(nace_level == "nace_niv4") ~ "nace_niv4",
        any(nace_level == "nace_niv3") ~ "nace_niv3",
        any(nace_level == "nace_niv2") ~ "nace_niv2"
      ),
      distance = distance[nace_level == level_rank][1],
      similarity_score = 1 / (1e-6 + distance),
      .groups = "drop"
    ) %>%
    arrange(desc(similarity_score)) %>%
    select(country, similarity_score)
}

get_similar_countries_figaro <- function(
  country_i,
  industry_i,
  candidate_countries,
  figaro_main_aggregates_data
) {
  figaro_gdp_shares <- figaro_main_aggregates_data %>%
    group_by(year, country) %>%
    mutate(
      gdp_total = sum(NVA, na.rm = TRUE),
      gdp_share = if_else(gdp_total != 0, NVA / gdp_total, NA_real_)
    ) %>%
    ungroup() %>%
    filter(
      industry == industry_i,
      country %in% c(country_i, candidate_countries),
      gdp_share > 0
    ) %>%
    select(year, country, gdp_share)

  ref_industry_gdp_share <- figaro_gdp_shares %>%
    filter(country == country_i) %>%
    select(year, target_gdp_share = gdp_share)

  figaro_gdp_shares %>%
    filter(country %in% candidate_countries) %>%
    merge(ref_industry_gdp_share, by = "year") %>%
    mutate(
      distance = abs(gdp_share - target_gdp_share)
    ) %>%
    group_by(country) %>%
    summarise(
      distance = mean(distance, na.rm = TRUE),
      n_years = n(),
      .groups = "drop"
    ) %>%
    filter(is.finite(distance), n_years > 0) %>%
    mutate(
      similarity_score = 1 / (1e-6 + distance)
    ) %>%
    arrange(desc(similarity_score)) %>%
    select(country, similarity_score)
}
