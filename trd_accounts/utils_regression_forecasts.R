# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
#' Fonction pour calculer des prévisions par régression.
#'
#' Estime une régression linéaire et, si suffisamment d’observations sont disponibles,
#' une régression quadratique. Les années atypiques peuvent être exclues du jeu
#' d’estimation afin de limiter leur effet sur les tendances projetées.
#'
#' @param data Série historique contenant au moins les colonnes year et value.
#' @param forecast_horizon Nombre d'années futures à projeter.
#'
#' @return Liste contenant les prévisions linéaire et quadratique.

run_regression_forecasts <- function(
  data,
  forecast_horizon,
  constraint,
  verbose = FALSE
) {
  if (verbose) print("run_regression_forecasts")
  # --------------------------------------------------
  # Forecast years

  last_year <- max(data$year)
  forecast_years <- seq(
    from = last_year + 1,
    length.out = forecast_horizon
  )

  # --------------------------------------------------
  # Prepare regression data

  historical_values <- data$value
  names(historical_values) <- data$year

  zscore <- scale(historical_values)

  covid_years <- names(historical_values) %in% c("2020", "2021")
  historical_values_no_covid <- historical_values[!covid_years]
  zscore_covid <- scale(
    historical_values,
    center = mean(historical_values_no_covid, na.rm = TRUE),
    scale  = sd(historical_values_no_covid, na.rm = TRUE)
  )

  non_outlier_years <- !(covid_years & abs(zscore_covid) > 1 & abs(zscore) < 2)

  regression_data <- data.frame(
      year  = as.numeric(names(historical_values)),
      value = as.numeric(historical_values)
    ) %>%
    filter(non_outlier_years)

  # --------------------------------------------------
  # Regression forecast

  # -------------------------
  # Linear

  linear_forecast <- run_regression_forecast(
    data = data,
    regression_data = regression_data,
    formula = value ~ year,
    forecast_years = forecast_years
  )

  # -------------------------
  # Quadratic

  quadratic_forecast <- if (nrow(regression_data) >= 3) {
    run_regression_forecast(
      data = data,
      regression_data = regression_data,
      formula = value ~ poly(year, 2),
      forecast_years = forecast_years
    )
  } else {
    NULL
  }

  # --------------------------------------------------

  combined_regression_forecast <- combine_regression_forecasts(
    data = data,
    models = list(
      linear = linear_forecast,
      quadratic = quadratic_forecast
    ),
    constraint = constraint
  )

  return(combined_regression_forecast)
}

# ----------------------------------------------------------------------------------------------------
#' run_regression_forecast
#'
#' Ajuste un modèle de régression et projette les valeurs futures.
#'
#' Calibre la prévision pour qu’elle parte de la dernière valeur observée,
#' puis évalue la qualité du modèle sur les résidus historiques.
#'
#' @param data Série historique complète contenant au moins year et value.
#' @param regression_data Données utilisées pour ajuster la régression.
#' @param formula Formule du modèle de régression, par exemple value ~ year.
#' @param forecast_years Années futures à prédire.
#'
#' @return Liste contenant les prévisions et la performance

run_regression_forecast <- function(
  data,
  regression_data,
  formula,
  forecast_years
) {

  # Estimate regression model on the selected historical data
  model <- lm(formula, data = regression_data)

  # Compute calibration factor so the forecast starts from the last observed value
  last_year <- max(data$year)
  last_value <- data$value[data$year == last_year]
  calibration_factor <- predict(
    model,
    newdata = data.frame(year = last_year)
  ) / last_value

  # Forecast future years and apply calibration
  forecast <- predict(
      model,
      newdata = data.frame(year = forecast_years)
    ) / calibration_factor

  # Evaluate model performance on historical residuals
  performance <- sqrt(mean(abs(model$residuals)))

  # Return output
  return(list(
    results     = forecast,
    performance = performance
  ))
}

# ----------------------------------------------------------------------------------------------------

combine_regression_forecasts <- function(
  data,
  models,
  constraint = NULL
) {
  # -------------------------

  models <- Filter(Negate(is.null), models)
  model_performances <- map_dbl(models, "performance")

  # Compute inverse-performance weights
  inverse_performances <- 1 / model_performances
  model_weights <- inverse_performances / sum(inverse_performances)

  # Combine forecasts
  forecast_matrix <- do.call(cbind, map(models, "results"))
  combined_results <- as.numeric(forecast_matrix %*% model_weights)

  # Apply constraints
  combined_results <- pmax(combined_results, constraint$min)
  combined_results <- pmin(combined_results, constraint$max)

  return(combined_results)
}