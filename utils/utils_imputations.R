# La Societe Nouvelle

#' Complete partially observed time series.
#'
#' Missing values are completed only for series with at least one observed value.
#' Fully missing series are left as NA so they can be completed later by
#' similarity.

complete_series <- function(
  data,
  serie_pkey = c("country", "industry"), # without year
  min_value = -Inf,
  max_value = Inf,
  verbose = FALSE
) {
  if (verbose) message("Complete partial time series")

  if (!all(c(serie_pkey, "year", "value") %in% colnames(data))) {
    stop("data must contain serie_pkey columns, year and value")
  }

  if (!"flag" %in% colnames(data)) {
    data$flag <- ""
  }

  dataset_colnames <- colnames(data)
  original_year_class <- class(data$year)

  series_keys <- data %>%
    tidyr::unite(id, !!!syms(serie_pkey), remove = FALSE) %>%
    select(id, all_of(serie_pkey)) %>%
    distinct()

  time_series <- data %>%
    left_join(series_keys, by = serie_pkey) %>%
    transmute(id, year = as.integer(year), value)

  if (verbose) message("detecting missing values...")

  missing_values_flags <- time_series %>%
    group_by(id) %>%
    mutate(
      has_observed_value = any(!is.na(value)),
      has_missing_value = any(is.na(value)),
      is_missing = is.na(value) & has_observed_value
    ) %>%
    ungroup() %>%
    filter(has_missing_value, has_observed_value) %>%
    select(id, year, is_missing)

  if (!any(missing_values_flags$is_missing)) {
    if (verbose) message("no partial missing values, no calculation")
    return(data)
  }

  if (verbose) {
    message("Number of missing values to complete: ", sum(missing_values_flags$is_missing))
  }

  completed_time_series <- time_series %>%
    group_by(id) %>%
    group_modify(
      ~ complete_one_series(
        .x,
        min_value = min_value,
        max_value = max_value
      )
    ) %>%
    ungroup() %>%
    transmute(id, year, imputed_value = value)

  adjusted_data <- data %>%
    mutate(.complete_series_year = as.integer(year)) %>%
    left_join(series_keys, by = serie_pkey) %>%
    left_join(
      missing_values_flags,
      by = c("id", ".complete_series_year" = "year")
    ) %>%
    left_join(
      completed_time_series,
      by = c("id", ".complete_series_year" = "year")
    ) %>%
    mutate(
      is_missing = coalesce(is_missing, FALSE),
      value = if_else(is_missing & !is.na(imputed_value), imputed_value, value),
      flag = if_else(is_missing & !is.na(imputed_value), "r", flag)
    ) %>%
    select(all_of(dataset_colnames))

  if ("character" %in% original_year_class) {
    adjusted_data$year <- as.character(adjusted_data$year)
  }

  return(adjusted_data)
}

complete_one_series <- function(
  data,
  min_value = -Inf,
  max_value = Inf
) {
  data <- data %>% arrange(year)

  observed <- data %>%
    filter(!is.na(value), is.finite(value))

  if (nrow(observed) == 0 || !any(is.na(data$value))) {
    return(data)
  }

  completed_value <- data$value
  missing_indexes <- which(is.na(completed_value))

  for (i in missing_indexes) {
    year_i <- data$year[i]
    previous_obs <- observed %>% filter(year < year_i)
    next_obs <- observed %>% filter(year > year_i)

    if (nrow(previous_obs) > 0 && nrow(next_obs) > 0) {
      completed_value[i] <- approx(
        x = observed$year,
        y = observed$value,
        xout = year_i,
        rule = 1
      )$y
    } else {
      completed_value[i] <- predict_series_edge(
        observed,
        year_i,
        min_value = min_value
      )
    }
  }

  fallback_value <- if (nrow(observed) > 0) {
    median(observed$value, na.rm = TRUE)
  } else {
    NA_real_
  }

  completed_value[is.na(completed_value)] <- fallback_value
  completed_value <- pmax(completed_value, min_value)
  completed_value <- pmin(completed_value, max_value)

  data$value <- completed_value
  return(data)
}

predict_series_edge <- function(
  observed,
  year_i,
  min_value = -Inf
) {
  if (nrow(observed) >= 3) {
    use_log_model <- is.finite(min_value) && min_value >= 0 && all(observed$value > 0)

    prediction <- tryCatch(
      {
        if (use_log_model) {
          model <- lm(log(value) ~ year, data = observed)
          exp(predict(model, newdata = data.frame(year = year_i)))
        } else {
          model <- lm(value ~ year, data = observed)
          predict(model, newdata = data.frame(year = year_i))
        }
      },
      error = function(e) NA_real_
    )

    if (is.finite(prediction)) {
      return(as.numeric(prediction))
    }
  }

  if (nrow(observed) >= 1) {
    nearest_index <- which.min(abs(observed$year - year_i))
    return(observed$value[nearest_index])
  }

  return(NA_real_)
}
