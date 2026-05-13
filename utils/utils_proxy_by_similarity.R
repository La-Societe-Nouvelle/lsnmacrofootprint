# La Société Nouvelle

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
  verbose = TRUE
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
    plan(multisession, workers = max(1, detectCores() - 1))

    options(progressr.enable = TRUE)
    handlers("txtprogressbar")

    results <- with_progress({
      # Progress bar
      p <- progressor(along = missing_values_indexes)
      # Computations
      foreach(
        i = missing_values_indexes,
        .combine = rbind,
        .options.future = list(
          packages = c("dplyr", "progressr"),
          conditions = structure("condition", exclude = "message")
        )
      ) %dofuture% {
        # Compute proxy data
        proxy_data <- get_proxy_value_by_similarity(
          raw_vector = raw_vector,
          type = type,
          index = i,
          oecd_sbs_data = oecd_sbs_data,
          figaro_main_aggregates_data = figaro_main_aggregates_data
        )
        # check if error
        if (is.na(proxy_data$value)) {
          stop(paste0("ERROR PROXY ",i," - ",raw_vector$country[i], " ",raw_vector$industry[i]))
        }
        # progress
        p(sprintf("%s %s", raw_vector$country[i], raw_vector$industry[i]))
        # return index and results
        data.frame(
          index = i,
          value = proxy_data$value,
          flag = "e"
        )
      }
    }, enable = TRUE)

    # assign results to complete dataframe
    raw_vector$value[results$index] <- results$value
    raw_vector$flag[results$index]  <- results$flag
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
      p <- progressor(along = missing_values_indexes)

      foreach(
        i = missing_values_indexes,
        .combine = rbind
      ) %do% {
        # Compute proxy data
        proxy_data <- get_proxy_value_by_similarity(
          raw_vector = raw_vector,
          type = type,
          index = i,
          oecd_sbs_data = oecd_sbs_data,
          figaro_main_aggregates_data = figaro_main_aggregates_data
        )
        # check if error
        if (is.na(proxy_data$value)) {
          stop(paste0("ERROR PROXY ",i," - ",raw_vector$country[i], " ",raw_vector$industry[i]))
        }
        # progress
        p(sprintf("%s %s", raw_vector$country[i], raw_vector$industry[i]))
        # return index and results
        data.frame(
          index = i,
          value = proxy_data$value,
          flag = "e"
        )
      }
    }, enable = TRUE)

    # assign results to complete dataframe
    raw_vector$value[results$index] <- results$value
    raw_vector$flag[results$index]  <- results$flag
  }

  # --------------------------------------------------

  # format data
  vector <- raw_vector

  # print(vector %>% as_tibble())
  # print(vector %>% filter(is.na(value)) %>% as_tibble())

  return(vector)
}


# compute_similarity() --------------------------------------------------------
# Computes a similarity-based estimate for missing values using sectoral and national shares (ISO2).
# @param SBS_SHARES A data frame containing sectoral shares data.
# @param raw_vector A data frame with country, industry, and value columns.
# @param agg A data frame containing aggregate data.
# @param type A character string indicating the estimation type ('intensity' or 'other').
# @param num_proxy An integer specifying the number of proxy countries to consider (default: 5).
# @param similarity_countries description
# Return A data frame with estimated values, including country, industry, estimated value, and additional info.

# raw_vector must have id as {figaro_country}_{figaro_industry}
# # All in ISO2

get_proxy_value_by_similarity = function(
  raw_vector,
  type, # extension type (intensity/index/rate)
  index,
  oecd_sbs_data,
  figaro_main_aggregates_data,
  num_proxy = 5,
  verbose = TRUE
) {
  # --------------------------------------------------

  year_i <- raw_vector$year[index]
  country_i <- raw_vector$country[index]
  industry_i <- raw_vector$industry[index]

  # print(year_i)
  # print(country_i)
  # print(industry_i)

  # --------------------------------------------------
  # Impacts data for industry

  sectoral_vector <- raw_vector %>%
    filter(
      !is.na(value),
      year == year_i,
      industry == industry_i,
      country != country_i
    ) %>%
    select(year,country,industry,value)

  # print(raw_vector %>%
  #   filter(
  #     year == year_i,
  #     industry == industry_i,
  #     country != country_i
  #   ) %>%
  #   select(year,country,industry,value) %>%
  #   as_tibble())
  # print(sectoral_vector %>% as_tibble())

  # -------------------------------------------------------------------
  # Method to apply

  industry_va <- figaro_main_aggregates_data %>%
      filter(year == year_i, country == country_i, industry == industry_i) %>%
      pull(NVA)

  sbs_subset <- oecd_sbs_data %>%
    filter(industry == industry_i, country == country_i)

  sbs_subset_candidates <- oecd_sbs_data %>%
    filter(industry == industry_i, country %in% sectoral_vector$country)

  use_sbs_data <-
    nrow(sbs_subset) > 0 &&
    !all(is.na(sbs_subset$share)) &&
    nrow(sbs_subset_candidates) > 0 &&
    !all(is.na(sbs_subset_candidates$share))

  # --------------------------------------------------
  # No value added

  # print(industry_va == 0)

  if (industry_va == 0)
  {
    proxy_data <- figaro_main_aggregates_data %>%
      filter(year == year_i, country == country_i, industry == industry_i) %>%
      mutate(
        value = 0,
        flag = "z"
      ) %>%
      select(year, country, industry, value, flag)

    return(proxy_data)
  }

  # --------------------------------------------------
  # Proxy using SBS data

  else if (use_sbs_data)
  {
    # -------------------------
    # Target shares

    ref_sbs_shares <- oecd_sbs_data %>%
      filter(
        industry == industry_i,
        country == country_i
      ) %>%
      transmute(nace_level, nace_code, target_share = share)

    # -------------------------
    # Ranking countries

    similar_countries <- oecd_sbs_data %>%
      filter(
        industry == industry_i,
        country %in% sectoral_vector$country
      ) %>%
      select(country, nace_level, nace_code, share) %>%
      # merge target sahres & compute distance with targets
      merge(ref_sbs_shares) %>%
      mutate(
        distance = abs(coalesce(share, 0) - coalesce(target_share, 0))
      ) %>%
      # compute distance at the NACE level
      group_by(country, nace_level) %>%
      summarise(
        distance = sum(distance),
        .groups = "drop"
      ) %>%
      # compute distance at the country level (priority for the more granular level)
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
      # Keep only few similar countries
      arrange(desc(similarity_score)) %>%
      head(num_proxy) %>%
      select(country, similarity_score)

    # print(similar_countries %>% as_tibble())
  }

  # --------------------------------------------------
  # Proxy using FIGARO data
  # (share of industry VA in GDP)

  else
  {
    # -------------------------
    # Target industry GDP Share

    ref_industry_gdp_share <- figaro_main_aggregates_data %>%
      filter(
        year == year_i,
        country == country_i
      ) %>%
      mutate(
        gdp_share = NVA / sum(NVA)
      ) %>%
      filter(industry == industry_i) %>%
      pull(gdp_share)

    # -------------------------
    # Ranking countries

    similar_countries <- figaro_main_aggregates_data %>%
      filter(year == year_i) %>%
      group_by(country) %>%
      mutate(
        gdp_share = NVA / sum(NVA)
      ) %>%
      ungroup() %>%
      filter(
        industry == industry_i,
        country %in% sectoral_vector$country,
        gdp_share > 0
      ) %>%
      mutate(
        distance = abs(gdp_share - ref_industry_gdp_share),
        similarity_score = 1 / (1e-6 + distance)
      ) %>%
      # Keep only few similar countries
      arrange(desc(similarity_score)) %>%
      head(num_proxy) %>%
      select(country, similarity_score)

    # print(similar_countries %>% as_tibble())
  }

  # --------------------------------------------------
  # Build proxy data

  # print(type)
  # print(similar_countries)

  if (type == "intensity")
  {
    proxy_value <- sectoral_vector %>%
      merge(similar_countries) %>% # by country
      merge(figaro_main_aggregates_data) %>% # by year, country, industry
      mutate(
        ratio = value / NVA
      ) %>%
      summarise(
        value = sum(ratio * similarity_score) / sum(similarity_score),
        .groups= 'drop'
      ) %>%
      pull(value)

    proxy_data <- figaro_main_aggregates_data %>%
      filter(year == year_i, country == country_i, industry == industry_i) %>%
      mutate(
        value = proxy_value * NVA,
        flag = "e"
      ) %>%
      select(year, country, industry, value, flag)
  }
  else
  {
    proxy_value <- sectoral_vector %>%
      merge(similar_countries) %>% # by country
      summarise(
        value = sum(value * similarity_score) / sum(similarity_score),
        .groups= 'drop'
      ) %>%
      pull(value)

    proxy_data <- figaro_main_aggregates_data %>%
      filter(year == year_i, country == country_i, industry == industry_i) %>%
      mutate(
        value = proxy_value,
        flag = "e"
      ) %>%
      select(year, country, industry, value, flag)
  }

  return(proxy_data)
}
