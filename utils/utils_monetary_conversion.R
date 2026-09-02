# La Societe Nouvelle

get_oecd_exchange_rates <- function(cache_key, url, update = FALSE, verbose = TRUE) {
  cache_dir <- file.path("data_temp", "monetary_conversion")
  cache_file <- file.path(cache_dir, paste0(cache_key, ".csv"))

  if (!update && file.exists(cache_file)) {
    if (verbose) message("Cached OECD data used: ", cache_file)
    return(read.csv(cache_file))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  tmp_file <- tempfile(fileext = ".csv")
  download_error <- NULL

  for (attempt in 1:3) {
    download_error <- tryCatch(
      {
        handle <- curl::new_handle(timeout = 180, connecttimeout = 30)
        curl::curl_download(url, tmp_file, quiet = !verbose, handle = handle)
        NULL
      },
      error = function(e) e
    )

    if (is.null(download_error)) {
      data <- read.csv(tmp_file)
      write.csv(data, cache_file, row.names = FALSE)
      if (verbose) message("OECD data cached: ", cache_file)
      return(data)
    }

    if (verbose) {
      message(
        "OECD download failed for ", cache_key,
        " (attempt ", attempt, "/3): ", conditionMessage(download_error)
      )
    }
    Sys.sleep(attempt)
  }

  if (file.exists(cache_file)) {
    warning(
      "OECD download failed for ", cache_key,
      "; using existing cache: ", cache_file,
      call. = FALSE
    )
    return(read.csv(cache_file))
  }

  stop(
    "Unable to download OECD exchange-rate data for ", cache_key,
    " and no cache is available at ", cache_file, ". Last error: ",
    conditionMessage(download_error),
    call. = FALSE
  )
}

from_pound_to_euro <- function(year, update = FALSE, verbose = TRUE) {
  from_pound_to_euro_data <- get_oecd_exchange_rates(
    cache_key = "OECD_POUND",
    url = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.GBR+EU27_2020...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels",
    update = update,
    verbose = verbose
  )

  from_pound_to_euro_data %>%
    filter(TIME_PERIOD == year) %>%
    summarise(value = OBS_VALUE[REF_AREA == "EU27_2020"] / OBS_VALUE[REF_AREA == "GBR"]) %>%
    pull(value)
}

from_usd_to_euro <- function(year, update = FALSE, verbose = FALSE) {
  from_usd_to_euro_data <- get_oecd_exchange_rates(
    cache_key = "OECD_DOLLAR",
    url = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.EU27_2020...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels",
    update = update,
    verbose = verbose
  )

  from_usd_to_euro_data %>%
    filter(TIME_PERIOD == year) %>%
    pull(OBS_VALUE)
}

from_cad_to_euro <- function(year, update = FALSE, verbose = TRUE) {
  from_cad_to_euro_data <- get_oecd_exchange_rates(
    cache_key = "OECD_CDOLLAR",
    url = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.EU27_2020+CAN...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels",
    update = update,
    verbose = verbose
  )

  from_cad_to_euro_data %>%
    filter(TIME_PERIOD == year) %>%
    summarise(value = OBS_VALUE[REF_AREA == "EU27_2020"] / OBS_VALUE[REF_AREA == "CAN"]) %>%
    pull(value)
}

from_dkk_to_euro <- function(year, update = FALSE, verbose = TRUE) {
  from_dkk_to_euro_data <- tryCatch(
    get_oecd_exchange_rates(
      cache_key = "OECD_DKK",
      url = "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/A.DNK+EU27_2020...EXC_A.......?&dimensionAtObservation=AllDimensions&format=csvfilewithlabels",
      update = update,
      verbose = verbose
    ),
    error = function(e) {
      warning(
        "OECD DKK/EUR download unavailable; using fixed ERM II parity 1 / 7.46038. ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(from_dkk_to_euro_data)) {
    return(1 / 7.46038)
  }

  from_dkk_to_euro_data %>%
    filter(TIME_PERIOD == year) %>%
    summarise(value = OBS_VALUE[REF_AREA == "EU27_2020"] / OBS_VALUE[REF_AREA == "DNK"]) %>%
    pull(value)
}
