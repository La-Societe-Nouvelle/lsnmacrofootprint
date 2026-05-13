# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for trend series
#'
#'
#'
#'
#' compute_trd()

# compute_trd() ---------------------------------------------------------------
# Computes trend (_trd) values for a specific indicator
# - Fetches historical data (_obs)
# - Determines appropriate modeling case (value, value/VA, value/X)
# - Simulates trends using montecarlo_sim_lsnmacro and get_indic_constraint
# - Formats results for database insertion
# Use get_indic_constraint AND montecarlo_sim_lsnmacro

build_trd_accounts <- function(
  indic_i,
  years = 2010:2030,
  verbose = FALSE
) {
  if (verbose) print(paste0("Computing trend for indic ", toupper(indic_i)))
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")
  source("utils/utils_outliers.R")

  source("trd_accounts/utils_montecarlo_forecasts.R")
  source("trd_accounts/utils_regression_forecasts.R")

  # -------------------------------------------------------------------
  # Metadata

  years <- tibble(year = as.character(years))

  metadata_indic <- read_delim(
      "metadata/metadata_indics.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code == indic_i) %>%
    select(type,defaultprecision,min,max)

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

  # -------------------------------------------------------------------
  # FIGARO Economic data

  if (verbose) print("Loading FIGARO data")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, NVA)

  # -------------------------------------------------------------------
  # OBS Accounts

  if (verbose) print("Loading OBS accounts data")

  obs_accounts_file <- paste0("accounts_obs_",tolower(indic_i),".csv")
  obs_accounts_path  <- file.path(output_dir, obs_accounts_file)

  obs_data_raw <- read.csv(obs_accounts_path)

  # Convert accounts data in fpt (with VA)
  obs_data <- obs_data_raw %>%
    merge(main_aggregates_data) %>%
    mutate(
      year = as.integer(year),
      value = case_when(
        # -------------------------
        # contribution rates
        metadata_indic$type == "rate" ~ ifelse(NVA > 0, value / NVA * 100, 0),
        # indexes
        metadata_indic$type == "index" ~ value,
        # intensities
        metadata_indic$type == "intensity" ~ ifelse(NVA > 0, value / NVA, 0)
        # -------------------------
      )) %>%
    select(year, country, industry, value, flag) %>%
    arrange(year, country, industry)

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) print("Building trend data")

  # Horizon 2030
  trend_years <- (max(obs_data$year) + 1):2030

  # compute trend for each serie (country,industry)
  trends_data_raw <- obs_data %>%
    group_by(country, industry) %>%
    group_split() %>%
    map_dfr(
      ~ compute_serie_forecast(
        data = .x,
        trend_years = trend_years,
        constraint = metadata_indic,
        verbose = verbose
      )
    ) %>%
    mutate(
      year = as.character(year),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  if (verbose) print("Cleaning outliers")

  # clean outliers
  trends_data <- trends_data_raw %>%
    rbind(obs_data) %>%
    clean_outliers(
      verbose = verbose
    ) %>%
    filter(year %in% trend_years)

  # build accounts
  figaro_trd_accounts <- trends_data %>%
    merge(main_aggregates_data) %>%
    mutate(
      indic = indic_i,
      year = as.character(year),
      serie_id = paste0(tolower(indic), "_trd"),
      value = case_when(
        # -------------------------
        # contribution rates
        metadata_indic$type == "rate" ~ value / 100 * NVA,
        # indexes
        metadata_indic$type == "index" ~ value,
        # intensities
        metadata_indic$type == "intensity" ~ value * NVA
        # -------------------------
      ),
      flag = "f"
    ) %>%
    select(serie_id, country, industry, year, value, flag)

  if (verbose) print("Checking data")

  # Check
  size <- length(trend_years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_trd_accounts) != size) {
    error_data <<- figaro_trd_accounts
    stop("ERROR - Wrong size for trend accounts")
  } else if (any(is.na(figaro_trd_accounts$value))) {
    error_data <<- figaro_trd_accounts
    stop("ERROR - NA values in trend accounts")
  }

  # -------------------------------------------------------------------
  # Formatting data

  if (verbose) print("Formattage...")

  formatted_data <- figaro_trd_accounts %>%
    mutate(
      serie_id    = paste0(tolower(indic_i), "_trd"),
      value       = round(value, digits = metadata_indic$defaultprecision),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, industry, country, year, value, flag, lastupdate) %>%
    arrange(serie_id, industry, country, year)

  # -------------------------------------------------------------------
  # Save data

  accounts_data_filename <- paste0("accounts_trd_",tolower(indic_i),".csv")
  accounts_data_path  <- file.path(output_dir, accounts_data_filename)
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}

# ----------------------------------------------------------------------------------------------------
#' Fonction pour projeter une série temporelle en combinant plusieurs modèles de prévision.
#'
#' Lance plusieurs prévisions Monte Carlo et des prévisions par régression,
#' puis combine les modèles retenus selon leur performance historique.
#' La combinaison finale utilise une pondération inverse à l’erreur et respecte
#' les contraintes liées à l'indicateur (borne inférieure / borne supérieure).
#'
#' @param data Série historique contenant au moins les colonnes year, country, industry et value.
#' @param trend_years Années futures à projeter.
#' @param n_mc_simulations Nombre de simulations Monte Carlo à générer.
#' @param constraint Contraintes liées à l'indicateur (bornes min/max).
#'
#' @return Vecteur des valeurs projetées.

compute_serie_forecast <- function(
  data,
  trend_years,
  n_mc_simulations = 2000,
  constraint,
  verbose = FALSE
) {
  if (verbose) message("Construction de la tendance pour ",unique(data$country)," - ",unique(data$industry))
  # --------------------------------------------------
  # Forecast params

  forecast_horizon <- length(trend_years)

  # --------------------------------------------------
  # Case - constant serie

  is_constant_serie <- length(unique(data$value)) == 1

  if (is_constant_serie) {

    if (verbose) print("Calcul des prévisions - Monte Carlo")

    return(tibble(
      year     = trend_years,
      country  = unique(data$country),
      industry = unique(data$industry),
      value    = unique(data$value)
    ))
  }

  # --------------------------------------------------
  # Run Monte Carlo simulations

  if (verbose) print("Calcul des prévisions - Monte Carlo")

  mc_configs <- tribble(
    ~strategy,  ~use_temporal_weights, ~outlier_handling,
    "diff",     FALSE,                 "downweight",
    "diff",     TRUE,                  "downweight",
    "growth",   FALSE,                 "downweight",
    "growth",   TRUE,                  "downweight"
  )

  mc_forecasts <- pmap(
    mc_configs,
    \(strategy, use_temporal_weights, outlier_handling) {
      run_mc_forecasts(
        data = data,
        forecast_horizon = forecast_horizon,
        constraint = constraint,
        strategy = strategy,
        use_temporal_weights = use_temporal_weights,
        outlier_handling = outlier_handling,
        n_simulations = n_mc_simulations
      )
    }
  )

  # --------------------------------------------------
  # Run Regression forecasts (linear/quadratic)

  if (verbose) print("Calcul des prévisions - Régression")

  regression_forecasts <- run_regression_forecasts(
    data = data,
    forecast_horizon = forecast_horizon,
    constraint = constraint
  )

  # --------------------------------------------------
  # Final model combination

  if (verbose) print("Combinaison des prévisions")

  final_forecast <- combine_forecast_models(
    data = data,
    forecasts = c(
      mc_forecasts,
      list(regression_forecasts)
    ),
    constraint = constraint
  )

  # --------------------------------------------------
  # Fallback

  if (is.null(final_forecast)) {

    if (verbose) message("Use fallback for forecast")

    fallback_forecast <- compute_fallback_forecast(
      data = data,
      forecast_horizon = forecast_horizon,
      constraint = constraint
    )

    return(tibble(
      year     = trend_years,
      country  = unique(data$country),
      industry = unique(data$industry),
      value    = fallback_forecast
    ))
  }

  # --------------------------------------------------
  # Return forecasted values

  return(tibble(
    year     = trend_years,
    country  = unique(data$country),
    industry = unique(data$industry),
    value    = final_forecast
  ))
}

# ----------------------------------------------------------------------------------------------------
#' Combine plusieurs modèles de prévision.
#'
#' Filtre les modèles non alignés avec la tendance historique, puis combine les
#' prévisions restantes avec une pondération inverse à leur performance.
#' Si aucun modèle n’est retenu, une projection de repli basée sur le CAGR historique
#' est utilisée.
#'
#' @param data Série historique contenant au moins les colonnes year et value.
#' @param models Liste de modèles à combiner. Chaque modèle doit contenir results et performance.
#' @param constraint Liste optionnelle de contraintes min/max appliquées aux prévisions.
#' @param tol_low Seuil bas de tolérance par rapport au CAGR historique.
#' @param tol_high Seuil haut de tolérance par rapport au CAGR historique.
#'
#' @return Liste contenant les prévisions combinées et leur performance.

combine_forecast_models <- function(
  data,
  forecasts,
  constraint,
  cagr_ratio_min = 0.5,
  cagr_ratio_max = 2.0
) {
  # --------------------------------------------------
  # Collect forecasts

  forecasts <- Filter(Negate(is.null), forecasts)

  # --------------------------------------------------
  # Prepare historical data

  last_year <- max(data$year)
  last_value <- data$value[data$year == last_year]

  # --------------------------------------------------
  # Compute CAGR to assess forecasts

  obs_data_cagr <- get_cagr(data$value)

  forecasts_cagrs <- map_dbl(
    forecasts,
    \(forecast) get_cagr(c(last_value, forecast))
  )

  # Detect unaligned forecasts

  invalid_forecast_cagr <- !is.finite(forecasts_cagrs)
  outside_cagr_range <- if (is.finite(obs_data_cagr)) {
    too_low <- abs(forecasts_cagrs) < cagr_ratio_min * abs(obs_data_cagr)
    too_high <- abs(forecasts_cagrs) > cagr_ratio_max * abs(obs_data_cagr)
    too_low | too_high
  } else {
    FALSE
  }

  unaligned_forecasts <- invalid_forecast_cagr | outside_cagr_range

  # Skip if all forecasts are unaligned
  if (all(unaligned_forecasts)) {
    return(NULL)
  }

  # --------------------------------------------------
  # Combine forecasts based on performance

  selected_forecasts <- forecasts[!unaligned_forecasts]

  forecast_matrix <- sapply(
    selected_forecasts,
    \(forecast) forecast
  )

  forecast_performances <- map_dbl(
    selected_forecasts,
    \(forecast) assess_performance(data, forecast)
  )

  model_weights <- forecast_performances / sum(forecast_performances)

  # Combined results
  combined_results <- as.numeric(forecast_matrix %*% model_weights)

  # --------------------------------------------------
  # Apply constraints

  combined_results <- pmax(combined_results, constraint$min)
  combined_results <- pmin(combined_results, constraint$max)

  # --------------------------------------------------
  # Return forecast

  return(combined_results)
}

compute_fallback_forecast <- function(
  data,
  forecast_horizon,
  constraint
) {
  # --------------------------------------------------

  historical_data <- data[order(as.integer(data$year)), , drop = FALSE]
  historical_data$year <- as.integer(historical_data$year)

  last_year <- max(historical_data$year)
  last_value <- historical_data$value[historical_data$year == last_year]

  # --------------------------------------------------
  # Compute historical CAGR

  historical_data_cagr <- get_cagr(historical_data$value)

  # --------------------------------------------------
  # Fallback if all models are unaligned

  use_cagr <- is.finite(historical_data_cagr) && last_value > 0

  fallback_results <- if (use_cagr) {
    last_value * (1 + historical_data_cagr)^(seq_len(forecast_horizon))
  } else {
    rep(last_value, forecast_horizon)
  }

  # --------------------------------------------------
  # Apply constraints

  fallback_results <- pmax(fallback_results, constraint$min)
  fallback_results <- pmin(fallback_results, constraint$max)

  # --------------------------------------------------
  # Return results

  return(fallback_results)
}

# Compound Annual Growth Rate
get_cagr <- function(values) {

  values <- as.numeric(values)
  values <- values[is.finite(values) & values > 0]

  n_periods <- length(values) - 1

  if (n_periods < 1) {
    return(NA_real_)
  }

  first_value <- values[1]
  last_value  <- values[length(values)]

  (last_value / first_value)^(1 / n_periods) - 1
}

# Compute RMSE to assess the performance of a forecast
assess_performance <- function(
  data,
  forecast
) {
  # Last observed historical value
  last_observed_value <- tail(data$value[!is.na(data$value)], 1)

  # Average forecast annual variation
  average_forecast_variation <- mean(diff(forecast), na.rm = TRUE)

  # Retropolate historical values from the last observed value
  retropolated_values <- last_observed_value -
    average_forecast_variation * seq_len(nrow(data))

  # Compute RMSE between observed and retropolated values
  sqrt(mean((data$value - retropolated_values) ^ 2, na.rm = TRUE))
}
