# La Société Nouvelle

#' ----------------------------------------------------------------------------------------------------
#' Non-financial FIGARO accounts builder for domestic production (ECO)
#'
#' Main sources:
#'   - FIGARO main aggregates data
#'
#' Unit: values are in CPMEUR
#'
#' build_eco_obs_accounts()

build_eco_obs_accounts <- function(
  years = 2010:2023,
  use_temp_data = TRUE,
  verbose = FALSE
) {
  if (verbose) message("Build ECO accounts for observed data")
  # -------------------------------------------------------------------
  # Utils

  source("utils/utils_figaro_data.R")

  # -------------------------------------------------------------------
  # Metadata

  if (verbose) cat("Loading metadata...\n")

  years <- tibble(year = as.character(years))

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
  if (verbose) cat("Metadata loaded\n")

  # FIGARO Economic data

  if (verbose) cat("Loading FIGARO data...\n")

  main_aggregates_data_raw <- map_dfr(
    years$year,
    load_local_figaro_main_aggregates
  )

  main_aggregates_data <- main_aggregates_data_raw %>%
    filter(industry != "TOTAL") %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(country, industry, year, NVA)

  if (verbose) cat("FIGARO data loaded\n")

  # -------------------------------------------------------------------
  # Building FIGARO accounts

  if (verbose) cat("Building FIGARO accounts...\n")

  figaro_eco_accounts <- figaro_industries %>%
    merge(figaro_countries) %>%
    crossing(years) %>%
    left_join(main_aggregates_data, by = c("year", "country", "industry")) %>%
    mutate(
      value = case_when(
        country == "FR" ~ NVA,
        TRUE            ~ 0
      ),
      flag = ""
    ) %>%
    select(year, country, industry, value, flag)

  # Check
  size <- nrow(years)*nrow(figaro_industries)*nrow(figaro_countries)
  if (nrow(figaro_eco_accounts) != size) {
    error_data <<- figaro_eco_accounts
    stop("ERROR - Wrong size for obs accounts (ECO)")
  } else if (any(is.na(figaro_eco_accounts$value))) {
    error_data <<- figaro_eco_accounts
    stop("ERROR - NA values in obs accounts (ECO)")
  }

  # -------------------------------------------------------------------
  if (verbose) message("Accounts ready !")

  # Formatting data

  formatted_data <- figaro_eco_accounts %>%
    mutate(
      serie_id    = "eco_obs",
      value       = round(value, digits = 0),
      lastupdate  = Sys.Date()
    ) %>%
    select(serie_id, country, industry, year, value, flag, lastupdate) %>%
    arrange(serie_id, country, industry, year)

  # -------------------------------------------------------------------
  if (verbose) print(formatted_data %>% as_tibble())

  # Save data

  accounts_data_path  <- file.path(output_dir, "accounts_obs_eco.csv")
  write.csv(formatted_data, accounts_data_path, row.names = FALSE)

  # Return
  return(formatted_data)
}
