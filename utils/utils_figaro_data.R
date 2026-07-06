# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
#' Fonction pour télécharger les données FIGARO

download_figaro_file <- function(
  filename,
  verbose = FALSE,
  data_dir = "data_figaro"
) {
  filepath <- file.path(data_dir, filename)

  endpoint <- paste0(
    "https://api.sinese.fr/v2/figarodata/",
    utils::URLencode(filename, reserved = TRUE)
  )

  response <- curl_fetch_memory(endpoint)

  if (response$status_code < 200 || response$status_code >= 300) {
    stop(
      "Unable to get a download URL for ", filename,
      " (HTTP ", response$status_code, ")."
    )
  }

  response_data <- fromJSON(rawToChar(response$content))
  download_url <- response_data$data$url

  if (verbose) {
    message("Downloading ", filename, "...")
  }

  curl_download(
    url = download_url,
    destfile = filepath,
    quiet = !verbose
  )
}

load_figaro_data_from_remote <- function(
  years = 2010:2030,
  verbose = FALSE
) {
  # --------------------------------------------------
  # Main aggregates

  for (year_i in years) {
    filename <- paste0("figaro_main_aggregates_", year_i, ".parquet")
    download_figaro_file(filename, verbose)
  }

  # --------------------------------------------------
  # Intermediate inputs

  for (year_i in years) {
    filename <- paste0("figaro_intermediate_inputs_", year_i, ".parquet")
    download_figaro_file(filename, verbose)
  }

  # --------------------------------------------------
  # Capital use

  for (year_i in years) {
    filename <- paste0("figaro_capital_use_", year_i, ".parquet")
    download_figaro_file(filename, verbose)
  }

  # --------------------------------------------------
  # NA prices

  download_figaro_file("figaro_na_prices.parquet", verbose)

  # --------------------------------------------------

  if (verbose) {
    message("FIGARO data download complete.")
  }
}

# ----------------------------------------------------------------------------------------------------
#' Fonctions pour charger les données FIGARO à partir des fichiers en local
#'
#' Default folder : data_figaro/*

load_local_figaro_main_aggregates <- function(
  year_i
) {
  main_aggregates_filename <- paste0("figaro_main_aggregates_", year_i, ".parquet")
  main_aggregates_filepath <- file.path("data_figaro/", main_aggregates_filename)

  read_parquet(main_aggregates_filepath) %>%
    filter(industry != "TOTAL") %>%
    mutate(
      id = paste0(country, "_", industry)
    ) %>%
    select(id, year, country, industry, aggregate, value)
}

load_local_figaro_intermediate_inputs <- function(
  year_i
) {
  intermediate_inputs_filename <- paste0("figaro_intermediate_inputs_", year_i, ".parquet")
  intermediate_inputs_filepath <- file.path("data_figaro/", intermediate_inputs_filename)

  intermediate_inputs <- read_parquet(intermediate_inputs_filepath) %>%
    mutate(
      year = year_i,
      use_id = paste0(use_country, "_", use_industry),
      resource_id = paste0(resource_country, "_", resource_industry)
    ) %>%
    select(year, use_id, use_country, use_industry, resource_id, resource_country, resource_industry, value)
}

load_local_figaro_capital_use <- function(
  year_i
) {
  capital_use_filename <- paste0("figaro_capital_use_", year_i, ".parquet")
  capital_use_filepath <- file.path("data_figaro/", capital_use_filename)

  capital_use <- read_parquet(capital_use_filepath) %>%
    mutate(
      year = year_i,
      use_id = paste0(use_country, "_", use_industry),
      resource_id = paste0(resource_country, "_", resource_industry)
    ) %>%
    select(year, use_id, use_country, use_industry, resource_id, resource_country, resource_industry, value)
}
