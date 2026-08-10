# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
#' Detect the latest *usable* year of an external data source.
#'
#' Problem: SDMX/REST APIs happily return a row for the most recent reference
#' year even when only a handful of countries/sectors have reported so far
#' (e.g. Eurostat env_ac_pefasu 2024: ~178 obs vs ~2800 for 2023 - a near-empty
#' preliminary release; or OECD DSD_WATER_PSUT 2024: 23 reporting countries vs
#' 47 in 2020). Requesting "the last year the API accepts" is therefore not
#' the same as "the last complete year".
#'
#' detect_max_usable_year() probes forward from a known-good year, accepting a
#' candidate year only if a completeness check (reporting-unit count, top-N
#' values, median value) stays within tolerance of the previous confirmed
#' year. Results are cached (default: 30 days) to avoid re-probing every run.
#'
#' Scope: this only decides *which years to request* from a source. It does
#' not replace proxy_missing_value_by_similarity() (utils/utils_proxy_by_similarity.R),
#' which still fills individual missing country/industry cells once the
#' requested years are fixed - that function's input contract
#' (year/country/industry/value/flag) is untouched by anything here.

# ----------------------------------------------------------------------------------------------------
#' Compares a candidate year's raw data to a reference (already validated) year.
#'
#' @param candidate_df Raw data for the candidate year (one row per reporting unit).
#' @param reference_df Raw data for the last confirmed-complete year.
#' @param group_col Column identifying the reporting unit (e.g. "country", "reporter", "geo").
#' @param value_col Column holding the observed value.
#' @param sector_col Optional column identifying the sector/activity/product breakdown (e.g.
#'   "nace_r2", "product", "ACTIVITY"). When supplied, compares the *median number of distinct
#'   sector_col values per group_col unit* between candidate and reference - not a dataset-wide
#'   distinct count, which would miss a per-reporter collapse: on Eurostat env_ac_pefasu 2024 the
#'   union of nace_r2 codes across all countries stayed at 88 in both years, but only one country
#'   (Norway) actually published the full 88-code breakdown while the rest reported only a
#'   handful - a global distinct-sector count would have read that as "complete". NULL (default)
#'   skips this check.
#' @param min_count_ratio Minimum ratio of distinct group_col values vs reference (default 0.8).
#'   Floor only - a candidate year with *more* reporting units than the reference is never
#'   rejected on this basis alone.
#' @param min_sector_ratio Same floor, applied to distinct sector_col values (default 0.8).
#'   Ignored when sector_col is NULL.
#' @param tolerance_test Allowed relative deviation from the reference order of magnitude for
#'   the median and for the sum of the n_top largest values (default 0.25, i.e. a candidate is
#'   accepted only if 0.75x <= candidate <= 1.25x the reference - both a collapse *and* a spike
#'   are treated as a red flag, since either can indicate a partial/miscoded preliminary release).
#' @param n_top Number of largest values compared between candidate and reference (default 5).
#' @return list(is_complete, count_ratio, sector_ratio, median_ratio, top_ratio, reason)

check_year_completeness <- function(
  candidate_df,
  reference_df,
  group_col,
  value_col,
  sector_col = NULL,
  min_count_ratio = 0.8,
  min_sector_ratio = 0.8,
  tolerance_test = 0.25,
  n_top = 5
) {
  if (is.null(candidate_df) || nrow(candidate_df) == 0) {
    return(list(
      is_complete  = FALSE,
      count_ratio  = 0,
      sector_ratio = NA_real_,
      median_ratio = NA_real_,
      top_ratio    = NA_real_,
      reason       = "no data returned for candidate year"
    ))
  }

  candidate_values <- as.numeric(candidate_df[[value_col]])
  reference_values  <- as.numeric(reference_df[[value_col]])

  n_candidate <- dplyr::n_distinct(candidate_df[[group_col]])
  n_reference <- dplyr::n_distinct(reference_df[[group_col]])
  count_ratio <- if (n_reference == 0) NA_real_ else n_candidate / n_reference

  sector_ratio <- NA_real_
  if (!is.null(sector_col)) {
    sector_density <- function(df) {
      df %>%
        dplyr::group_by(.data[[group_col]]) %>%
        dplyr::summarise(n_sectors = dplyr::n_distinct(.data[[sector_col]]), .groups = "drop") %>%
        dplyr::pull(n_sectors) %>%
        stats::median(na.rm = TRUE)
    }
    density_candidate <- sector_density(candidate_df)
    density_reference  <- sector_density(reference_df)
    sector_ratio <- if (is.na(density_reference) || density_reference == 0) {
      NA_real_
    } else {
      density_candidate / density_reference
    }
  }

  top_n <- function(x) sum(sort(x, decreasing = TRUE)[seq_len(min(n_top, length(x)))], na.rm = TRUE)
  top_candidate <- top_n(candidate_values)
  top_reference <- top_n(reference_values)
  top_ratio <- if (is.na(top_reference) || top_reference == 0) NA_real_ else top_candidate / top_reference

  median_candidate <- stats::median(candidate_values, na.rm = TRUE)
  median_reference  <- stats::median(reference_values, na.rm = TRUE)
  median_ratio <- if (is.na(median_reference) || median_reference == 0) {
    NA_real_
  } else {
    median_candidate / median_reference
  }

  # Order-of-magnitude band: [1 - tolerance_test, 1 + tolerance_test], e.g. [0.75, 1.25]
  in_band <- function(ratio) is.na(ratio) || (ratio >= (1 - tolerance_test) && ratio <= (1 + tolerance_test))

  checks_ok <- c(
    is.na(count_ratio)  || count_ratio  >= min_count_ratio,
    is.na(sector_ratio) || sector_ratio >= min_sector_ratio,
    in_band(top_ratio),
    in_band(median_ratio)
  )

  list(
    is_complete  = all(checks_ok),
    count_ratio  = count_ratio,
    sector_ratio = sector_ratio,
    median_ratio = median_ratio,
    top_ratio    = top_ratio,
    reason       = if (all(checks_ok)) "ok" else "reporting-count/sector floor missed or order of magnitude out of tolerance band"
  )
}

# ----------------------------------------------------------------------------------------------------
#' Shared max-usable-year cache: one JSON file, one entry per source_name.

source_years_cache_path <- function(cache_dir = download_dir) {
  file.path(cache_dir, "_source_max_years.json")
}

read_source_years_cache <- function(cache_path) {
  if (!file.exists(cache_path)) return(list())
  tryCatch(
    jsonlite::fromJSON(cache_path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

write_source_years_cache <- function(cache_path, source_name, max_year, details = list()) {
  cache <- read_source_years_cache(cache_path)
  cache[[source_name]] <- list(
    max_year   = max_year,
    checked_at = as.character(Sys.Date()),
    details    = details
  )
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(cache, cache_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(cache)
}

# ----------------------------------------------------------------------------------------------------
#' Detects the latest usable year for one external source, with caching.
#'
#' Only ever extends *upward* from known_good_year - it never proposes a year
#' below the one already trusted by the caller.
#'
#' @param source_name Unique cache key, e.g. "HAZ_PRODCOM".
#' @param fetch_year_fn function(year) -> data.frame of raw rows for that year (already
#'   filtered down to whatever slice of the source the caller actually uses), or
#'   NULL/empty/error if the source has no data at all for that year.
#' @param group_col,value_col,sector_col See check_year_completeness(). sector_col defaults to
#'   NULL (skip the sector-stability check) since not every source exposes one at probe time.
#' @param known_good_year Last year already trusted as complete (starting point).
#' @param lookahead How many *candidate steps* beyond known_good_year to probe (default 2) -
#'   e.g. with step = 2 and lookahead = 2, candidates are known_good_year + 2 and + 4.
#' @param step Interval between candidate years (default 1 = annual). Set to 2 for a biennial
#'   source (e.g. Eurostat env_wasgen) so the probe doesn't waste a request - and, more
#'   importantly, doesn't stop early - on a year the source structurally never publishes.
#' @param min_count_ratio,min_sector_ratio,tolerance_test See check_year_completeness()
#'   (defaults 0.8/0.8/0.25).
#' @param cache_dir,cache_max_age_days Cache location / TTL in days (default 30 = 1 month).
#' @param force_refresh Ignore the cache and re-probe now.
#' @param verbose Log each candidate year's ratios and verdict.
#' @return integer: the latest year considered usable (>= known_good_year).

detect_max_usable_year <- function(
  source_name,
  fetch_year_fn,
  group_col,
  value_col,
  known_good_year,
  sector_col = NULL,
  lookahead = 2,
  step = 1,
  min_count_ratio = 0.8,
  min_sector_ratio = 0.8,
  tolerance_test = 0.25,
  cache_dir = download_dir,
  cache_max_age_days = 30,
  force_refresh = FALSE,
  verbose = FALSE
) {
  cache_path   <- source_years_cache_path(cache_dir)
  cache        <- read_source_years_cache(cache_path)
  cached_entry <- cache[[source_name]]

  if (!force_refresh && !is.null(cached_entry)) {
    age_days <- suppressWarnings(
      as.numeric(difftime(Sys.Date(), as.Date(cached_entry$checked_at), units = "days"))
    )
    if (!is.na(age_days) && age_days <= cache_max_age_days) {
      if (verbose) message(sprintf(
        "%s: using cached max year %s (checked %.0f day(s) ago, refreshed every %d)",
        source_name, cached_entry$max_year, age_days, cache_max_age_days
      ))
      return(as.integer(cached_entry$max_year))
    }
  }

  reference_df <- fetch_year_fn(known_good_year)
  if (is.null(reference_df) || nrow(reference_df) == 0) {
    stop(source_name, ": known_good_year ", known_good_year,
         " returned no data - check fetch_year_fn before relying on detection")
  }

  max_year <- known_good_year
  details  <- list()

  if (lookahead > 0) {
    candidate_years <- known_good_year + step * seq_len(lookahead)
    for (candidate_year in candidate_years) {
      candidate_df <- tryCatch(fetch_year_fn(candidate_year), error = function(e) NULL)

      check <- check_year_completeness(
        candidate_df, reference_df, group_col, value_col,
        sector_col        = sector_col,
        min_count_ratio   = min_count_ratio,
        min_sector_ratio  = min_sector_ratio,
        tolerance_test    = tolerance_test
      )

      fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f", x))
      if (verbose) message(sprintf(
        "%s: year %s -> count_ratio=%s sector_ratio=%s top_ratio=%s median_ratio=%s (%s)",
        source_name, candidate_year,
        fmt(check$count_ratio), fmt(check$sector_ratio), fmt(check$top_ratio), fmt(check$median_ratio),
        if (check$is_complete) "OK" else paste("rejected:", check$reason)
      ))

      if (!check$is_complete) break

      max_year     <- candidate_year
      reference_df <- candidate_df
      details      <- check[c("count_ratio", "sector_ratio", "top_ratio", "median_ratio")]
    }
  }

  write_source_years_cache(cache_path, source_name, max_year, details)

  as.integer(max_year)
}
