# La Société Nouvelle

#' Utils pour corriger les valeurs atypiques d’un panel de séries temporelles.
#'
#' Construit un identifiant de série à partir de "serie_pkey", détecte les
#' valeurs atypiques avec `detect_outlier_fbi()`, les impute avec
#' `impute_outliers_fbi()`, puis remplace uniquement les valeurs identifiées
#' comme outliers.
#'
#' @param data Data frame contenant les séries temporelles à nettoyer.
#' @param serie_pkey Colonnes identifiant une série, hors année.
#' @param kmax Nombre maximal de facteurs/itérations utilisés par la méthode FBI.
#' @param verbose Si TRUE, affiche des messages d’avancement.
#'
#' @return Data frame initial avec les valeurs atypiques remplacées et un flag de correction.

clean_outliers <- function(
  data,
  serie_pkey = c("country", "industry"), # without year
  kmax       = 2,
  use_tw_apc = FALSE,
  verbose    = FALSE
) {
  if (verbose) message("Clean outliers")
  # --------------------------------------------------
  # Prepare data

  dataset_colnames <- colnames(data)

  # table avec colonnes PKEY
  series_keys <- data %>%
    unite(id, !!!syms(serie_pkey), remove = FALSE) %>%
    select(id, all_of(serie_pkey)) %>%
    distinct()

  time_series <- data %>%
    left_join(series_keys, by = serie_pkey) %>%
    select(id, year, value)

  # --------------------------------------------------
  # Handle outiliers

  if (verbose) message("detecting outliers...")

  # Detect outliers
  outlier_flags  <- detect_outlier_fbi(time_series) %>%
    select(id, year, is_outlier)

  if (!any(outlier_flags$is_outlier)) {
    if (verbose) message("no outliers, no calculation")
    return(data)
  } else {
    n_outliers <- sum(outlier_flags$is_outlier, na.rm = TRUE)
    if (verbose) message("Number of outliers detected: ", n_outliers)
  }

  # --------------------------------------------------
  # Imputation via FBI

  if (verbose) message("imputing data...")

  time_series_cleared <- time_series %>%
    left_join(outlier_flags, by = c("id", "year")) %>%
    mutate(
      value = if_else(is_outlier, NA_real_, value)
    ) %>%
    select(id, year, value)

  imputed_time_series <- impute_outliers_fbi(
      time_series_cleared,
      kmax = kmax,
      use_tw_apc = use_tw_apc
    ) %>%
    rename(imputed_value = value) %>%
    select(id, year, imputed_value)

  # --------------------------------------------------
  # Replace values (with imputed values)

  if (verbose) message("replacing values...")

  adjusted_data <- data %>%
    merge(series_keys) %>% # by columns serie_pkey
    left_join(outlier_flags, by = c("id", "year")) %>%
    left_join(imputed_time_series, by = c("id", "year")) %>%
    mutate(
      value = if_else(is_outlier, imputed_value, value),
      flag = if_else(is_outlier, "r", flag)
    ) %>%
    select(all_of(dataset_colnames))

  return(adjusted_data)
}

# ----------------------------------------------------------------------------------------------------
#' Fonction pour détecter les valeurs atypiques (outliers) dans chaque série temporelle.
#'
#' Calcule, pour chaque id, la médiane, l’écart interquartile et l’amplitude
#' relative de la série, puis identifie les observations suffisamment éloignées
#' de la médiane pour être considérées comme outliers.
#'
#' @param time_series Data frame contenant les colonnes id, year et value.
#' @param tolerance Seuil minimal d’amplitude relative pour appliquer la détection.
#'
#' @return Data frame initial enrichi avec les indicateurs de dispersion et la colonne is_outlier.
#'
#' Adaptation of Bennie Chen code fbi : Factor-Based Imputation for Missing Data 
#' GitHub : https://github.com/cykbennie/fbi/tree/master

detect_outlier_fbi <- function(
  time_series,
  tolerance = 0.01
) {

  time_series <- time_series %>%
    group_by(id) %>%
    mutate(
      median    = quantile(value, 0.5,  na.rm = TRUE),
      Q1        = quantile(value, 0.25, na.rm = TRUE),
      Q3        = quantile(value, 0.75, na.rm = TRUE),
      interquartile_range = Q3 - Q1,
      range_values = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      relative_range = range_values / if_else(median == 0, 1, median),
      is_outlier = (relative_range >= tolerance)
        & (interquartile_range > 0)
        & (abs(value - median) > (10 * interquartile_range))
    ) %>%
    ungroup()

  return(time_series)
}

# ----------------------------------------------------------------------------------------------------
#' Fonction pour imputer les valeurs manquantes d’un panel de séries temporelles avec la méthode FBI.
#'
#' Transforme les séries au format large (matrice), applique l’imputation Factor-Based Imputation
#' via `fbi::tp_apc()` ou `fbi::tw_apc()`, puis retourne les valeurs imputées au format long.
#'
#' Les valeurs à imputer doivent être renseignées en NA dans value.
#' 
#' @param time_series Data frame contenant les colonnes id, year et value.
#' @param kmax Nombre maximal de facteurs/itérations utilisés par la méthode FBI.
#'
#' @return Data frame au format long contenant id, year et value

impute_outliers_fbi <- function(
  time_series,
  kmax = 2,
  use_tw_apc = FALSE
) {
  # --------------------------------------------------
  # Build wide matrix for FBI imputation

  imputation_matrix <- time_series %>%
    arrange(year) %>%
    select(id, year, value) %>%
    pivot_wider(
      names_from = id,
      values_from = value
    ) %>%
    column_to_rownames("year")

  # --------------------------------------------------
  # Run FBI imputation

  imputed_matrix <- suppressPackageStartupMessages(
    suppressMessages(
      if (use_tw_apc) {
        fbi::tw_apc(
          as.matrix(imputation_matrix),
          kmax
          # center = TRUE,
          # standardize = TRUE,
          # re_estimate = TRUE
        )[["data"]]
      } else {
        fbi::tp_apc(
          as.matrix(imputation_matrix),
          kmax
          # center = TRUE,
          # standardize = TRUE,
          # re_estimate = TRUE
        )[["data"]]
      }
    )
  )

  # --------------------------------------------------
  # Convert imputed matrix back to long format

  adjusted_time_series <- as.data.frame(imputed_matrix) %>%
    `colnames<-`(colnames(imputation_matrix)) %>%
    mutate(
      year = rownames(imputation_matrix)
    ) %>%
    pivot_longer(
      cols = -year,
      names_to = "id",
      values_to = "value"
    )

  return(adjusted_time_series)
}
