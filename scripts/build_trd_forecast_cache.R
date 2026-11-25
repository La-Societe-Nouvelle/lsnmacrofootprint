# La Société Nouvelle

# Batch trd forecasts of obs series from the database, cached by indic and resumable.
# Usage, from the project root: Rscript scripts/build_trd_forecast_cache.R [indics] [n_workers] [batch_size]

readRenviron(".Renviron")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(purrr); library(readr)
  library(arrow); library(DBI); library(RPostgres)
  library(doFuture); library(foreach); library(future)
})

source("db/stats_db.R")
source("trd_accounts/utils_montecarlo_forecasts.R")
source("trd_accounts/utils_regression_forecasts.R")
source("trd_accounts/utils_moirai_forecasts.R")
source("trd_accounts/trend_accounts_builder.R")

# --------------------------------------------------------------------------
# Params

args <- commandArgs(trailingOnly = TRUE)

all_indics <- c("GEQ","WAS","HAZ","WAT","KNW","NRG","IDR","GHG","ART","ECO","MAT","SOC")
indics     <- if (length(args) >= 1 && nzchar(args[1])) strsplit(args[1], ",")[[1]] else all_indics
n_workers  <- if (length(args) >= 2) as.integer(args[2]) else 3
batch_size <- if (length(args) >= 3) as.integer(args[3]) else 200

cache_dir <- "data_temp/trd_forecast_cache"

# --------------------------------------------------------------------------
# Cache

read_cached_keys <- function(indic_i) {
  indic_path <- file.path(cache_dir, paste0("indic=", indic_i))
  if (!dir.exists(indic_path)) return(tibble(country = character(), industry = character()))
  open_dataset(indic_path) %>% distinct(country, industry) %>% collect()
}

write_forecast_batch <- function(results) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  # Unique file names per batch.
  batch_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(1e6, 1))
  write_dataset(
    results,
    path = cache_dir,
    partitioning = "indic",
    format = "parquet",
    compression = "zstd",
    basename_template = paste0("part-", batch_id, "-{i}.parquet")
  )
}

# --------------------------------------------------------------------------
# Obs data

con <- get_connection_db()
obs_raw <- dbGetQuery(con, sprintf(
  "SELECT serie_id, industry, country, year, value FROM impacts.direct_impacts WHERE right(serie_id,3) = 'obs' AND left(serie_id,3) IN (%s)",
  paste(sprintf("'%s'", tolower(indics)), collapse = ",")
))
dbDisconnect(con)

obs_data_all <- obs_raw %>%
  mutate(indic = toupper(substr(serie_id, 1, 3)), year = as.integer(year), value = as.numeric(value)) %>%
  select(indic, country, industry, year, value)

metadata_indics <- read_delim("metadata/metadata_indics.csv", delim = ";", show_col_types = FALSE)

plan(multisession, workers = n_workers)
on.exit(plan(sequential), add = TRUE)

# --------------------------------------------------------------------------
# Forecasts by indic, in batches

for (indic_i in indics) {
  message("=== ", indic_i)

  obs_data <- obs_data_all %>% filter(indic == indic_i) %>% select(-indic)
  if (nrow(obs_data) == 0) next

  constraint <- metadata_indics %>% filter(code == indic_i) %>% select(type, defaultprecision, min, max)
  trend_years <- (max(obs_data$year) + 1):2040
  if (min(trend_years) > 2040) next

  todo_keys <- obs_data %>%
    distinct(country, industry) %>%
    anti_join(read_cached_keys(indic_i), by = c("country", "industry"))

  series_list <- obs_data %>%
    inner_join(todo_keys, by = c("country", "industry")) %>%
    group_by(country, industry) %>%
    group_split()

  for (batch in split(series_list, ceiling(seq_along(series_list) / batch_size))) {
    batch_results <- foreach(
      serie_data = batch,
      .combine = bind_rows,
      .options.future = list(
        packages = c("dplyr", "tidyr", "purrr", "tibble", "reticulate"),
        conditions = structure("condition", exclude = "message"),
        seed = TRUE
      )
    ) %dofuture% {
      res <- tryCatch(
        compute_serie_forecast(serie_data, trend_years, constraint = constraint, use_moirai = TRUE),
        error = function(e) NULL
      )
      if (!is.null(res)) res$indic <- indic_i
      res
    }

    if (!is.null(batch_results) && nrow(batch_results) > 0) write_forecast_batch(batch_results)
    message("  ", length(batch), " series")
  }
}
