# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
#' Fonction pour réaliser des simulations Monte Carlo à partir des variations historiques.
#'
#' Initialise les trajectoires avec la dernière valeur observée, tire aléatoirement
#' des variations passées, puis projette la série sur l’horizon demandé.
#'
#' @param data Série historique contenant au moins les colonnes id, year et value.
#' @param forecast_horizon Nombre d'années à simuler.
#' @param strategy Méthode d’évolution : "diff" ou "growth".
#' @param use_temporal_weights Si TRUE, donne plus de poids aux années récentes.
#' @param outlier_handling Traitement des outliers : "include", "exclude" ou "downweight".
#' @param n_simulations Nombre de trajectoires Monte Carlo à générer.
#' @param verbose Si TRUE, affiche les messages
#'
#' @return Liste contenant les prévisions et la performance.

run_mc_forecasts <- function(
  data,
  forecast_horizon,
  constraint,
  strategy,
  use_temporal_weights,
  outlier_handling = "downweight",
  n_simulations,
  verbose = FALSE
) {
  if (verbose) print("run_mc_forecast")
  # --------------------------------------------------

  # Simulation matrix (with last known historical value)
  last_value <- tail(data$value[!is.na(data$value)], 1)
  simulation_matrix <- matrix(
    last_value,
    nrow = forecast_horizon + 1,
    ncol = n_simulations
  )

  # Variation pool
  variation_pool <- get_variation_pool(
    data,
    strategy
  )

  if (length(variation_pool) == 0 || all(!is.finite(variation_pool))) {
    return(NULL)
  }

  # Weights
  weights <- get_weights(
    variation_pool        = variation_pool,
    use_temporal_weights  = use_temporal_weights,
    outlier_handling      = outlier_handling
  )

  # --------------------------------------------------
  # Monte Carlo simulations

  for (step in seq_len(forecast_horizon)) {

    sampled_variations <- sample(
      variation_pool,
      size = n_simulations,
      replace = TRUE,
      prob = weights
    )

    if (strategy == "diff") {

      simulation_matrix[step + 1, ] <-
        simulation_matrix[step, ] + sampled_variations

    } else if (strategy == "growth") {

      simulation_matrix[step + 1, ] <-
        simulation_matrix[step, ] * exp(sampled_variations)

    } else {
      stop("Unknown strategy. Use 'diff' or 'growth'.")
    }
  }

  # --------------------------------------------------
  # Results

  results <- rowMeans(simulation_matrix, na.rm = TRUE)[-1]

  # Apply constraints
  results <- pmax(results, constraint$min)
  results <- pmin(results, constraint$max)

  return(results)
}

# ----------------------------------------------------------------------------------------------------
#' Fonction pour obtenir l'échantillon de variations historiques d’une série.
#'
#' Les variations sont annualisées afin de tenir compte des éventuels écarts entre années successives.
#' Elles peuvent être calculées en niveau (`diff`) ou en croissance logarithmique (`growth`).
#'
#' @param data Série historique contenant au moins les colonnes year et value.
#' @param strategy Méthode de calcul des variations : "diff" ou "growth".
#'
#' @return Vecteur numérique nommé par année, contenant les variations historiques valides.

get_variation_pool <- function(
  data,
  strategy
) {
  # If there are differences in the gaps of years
  year_diff <- diff(data$year)

  # Variation annuelle moyenne en niveau
  if (strategy == "diff")   pool <- diff(data$value)      / year_diff

  # Variation annuelle moyenne en logarithme
  if (strategy == "growth") pool <- diff(log(data$value)) / year_diff

  # Assign names based on corresponding years (excluding the first year of diff)
  names(pool) <- data$year[-(nrow(data) - length(pool))]

  # Remove NA and infinite values
  pool <- subset(pool, !is.na(pool) & !is.infinite(pool))

  return(pool)
}

# ----------------------------------------------------------------------------------------------------
#' Fonction pour calculer les poids de tirage pour la simulation Monte Carlo.
#'
#' Combine une pondération temporelle optionnelle avec un traitement des valeurs atypiques.
#' Les outliers peuvent être inclus, exclus ou minorés.
#' Les années 2020 et 2021 sont testées séparément avec un z-score hors période Covid.
#'
#' @param variation_pool Vecteur des variations historiques, nommé par année.
#' @param use_temporal_weights Si TRUE, donne plus de poids aux années récentes.
#' @param outlier_handling Traitement des outliers : "include", "exclude" ou "downweight".
#' @param downweight_penalty_factor Facteur de minoration si outlier_handling = "downweight".
#'
#' @return Vecteur de poids normalisés utilisable

get_weights <- function(
  variation_pool,
  use_temporal_weights,
  outlier_handling,
  downweight_penalty_factor = 0.3
) {
  # --------------------------------------------------
  # Z-scores

  # Standard Z-score across all years

  zscore <- scale(variation_pool)

  # Z-score excluding 2020 and 2021 for specific penalty handling (COVID)

  covid_years <- names(variation_pool) %in% c("2020", "2021")
  sample_no_covid <- variation_pool[!covid_years]
  zscore_covid <- scale(
    variation_pool,
    center = mean(sample_no_covid, na.rm = TRUE),
    scale  = sd(sample_no_covid, na.rm = TRUE)
  )

  # --------------------------------------------------
  # Outliers weighting

  # outlier penalty factor

  outlier_penalty_factor <- switch(
    outlier_handling,
    "include"    = 1,
    "exclude"    = 0,
    "downweight" = downweight_penalty_factor
  )

  # outlier weights

  is_outlier_standard <- abs(zscore) > 1
  is_outlier_covid <- covid_years & abs(zscore_covid) > 1
  is_outlier <- is_outlier_standard | is_outlier_covid

  outlier_weights <- ifelse(
    is_outlier,
    outlier_penalty_factor,
    1
  )

  # --------------------------------------------------
  # Temporal weighting

  # Temporal weighting
  time_weights <- if (use_temporal_weights) {
    seq_along(variation_pool)
  } else {
    rep(1, length(variation_pool))
  }

  # --------------------------------------------------
  # Final sampling weights

  raw_sampling_weights <- time_weights * outlier_weights
  sampling_weights <- raw_sampling_weights / sum(raw_sampling_weights)

  # If all weights are zero or unavailable, use uniform sampling
  if (is.null(sampling_weights) || all(is.na(sampling_weights))) {
    sampling_weights <- NULL
  }

  return(sampling_weights)
}