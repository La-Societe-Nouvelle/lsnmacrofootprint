# La Société Nouvelle

# Moirai-1.1-R-small forecast candidate (CC-BY-NC-4.0), via virtualenv MOIRAI_VENV.

MOIRAI_MODEL_ID <- "Salesforce/moirai-1.1-R-small"
MOIRAI_VENV     <- "lsnmacro_moirai"

# Near-constant series: range negligible in absolute terms or relative to scale.
is_near_constant_serie <- function(values, tol_rel = 1e-4, tol_abs = 1e-8) {
  rng <- diff(range(values, na.rm = TRUE))
  scale <- max(abs(values), na.rm = TRUE)
  rng <= tol_abs || (scale > 0 && rng / scale <= tol_rel)
}

# Python module, loaded once per R session.
.moirai_env <- new.env(parent = emptyenv())

get_moirai_module <- function(verbose = FALSE) {
  if (!is.null(.moirai_env$module)) {
    return(.moirai_env$module)
  }

  if (verbose) message("Loading Moirai model (", MOIRAI_MODEL_ID, ")...")

  reticulate::use_virtualenv(MOIRAI_VENV, required = TRUE)
  reticulate::source_python(file.path("trd_accounts", "moirai_forecast.py"), envir = .moirai_env)

  .moirai_env$module <- .moirai_env$moirai_load_module(MOIRAI_MODEL_ID)
  .moirai_env$module
}

# NULL when Moirai is unavailable.
run_moirai_forecasts <- function(
  data,
  forecast_horizon,
  constraint,
  patch_size  = "auto",
  num_samples = 100,
  verbose     = FALSE
) {
  module <- tryCatch(
    get_moirai_module(verbose = verbose),
    error = function(e) {
      if (verbose) message("Moirai unavailable: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(module)) return(NULL)

  forecast <- tryCatch(
    unlist(.moirai_env$moirai_predict(
      module            = module,
      values            = as.numeric(data$value),
      start_year        = as.integer(min(as.numeric(data$year))),
      prediction_length = as.integer(forecast_horizon),
      context_length    = length(data$value),
      patch_size        = patch_size,
      num_samples       = as.integer(num_samples)
    )),
    error = function(e) {
      if (verbose) message("Moirai forecast failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(forecast) || length(forecast) != forecast_horizon) return(NULL)

  pmin(pmax(forecast, constraint$min), constraint$max)
}
