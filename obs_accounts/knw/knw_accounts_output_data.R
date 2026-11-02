# La Societe Nouvelle

# Controle des donnees partagees avec le producteur, avant imputation.
# Les caches sont consultes localement ; refresh_sources autorise leur acquisition.
check_knw_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {

  # --- Sources et perimetre --------------------------------------------------

  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/knw/knw_accounts_sources.R", encoding = "UTF-8-BOM")

  years <- as.character(years)
  refresh_sources <- isTRUE(refresh_sources) || !isTRUE(use_temp_data)
  source_errors <- list()

  industries <- read_delim(
    "metadata/metadata_figaro_industries.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>% filter(code != "TOTAL")

  countries <- read_delim(
    "metadata/metadata_figaro_countries.csv",
    delim = ";",
    show_col_types = FALSE
  )$code

  load_source <- function(
    name,
    loader,
    source_years = years
  ) {
    data <- load_accounts_source_for_check(loader, source_years, refresh_sources, verbose)
    error <- attr(data, "source_error")
    if (!is.null(error)) source_errors[[name]] <<- error
    data
  }

  coverage <- function(
    data,
    scope = "industry",
    units = industries$code,
    structural_zero_row = FALSE
  ) {
    accounts_source_coverage(data, years, units, countries, scope, structural_zero_row)
  }

  # --- Disponibilite des sources effectivement utilisees ----------------------

  anberd_data <- load_source("anberd", load_anberd_knw_source)
  training_data <- load_source("trng_cvt", load_training_knw_source)
  stan_data <- load_source("stan", load_stan_knw_source)

  if (nrow(anberd_data) > 0L) {
    bridge <- read_delim(
      "obs_accounts/knw/anberd_correspondence_table_activity.csv",
      delim = ";", show_col_types = FALSE
    ) %>% transmute(industry = figaro_industry, anberd_activity)

    anberd_data <- anberd_data %>% filter(!is.na(value)) %>%
      inner_join(bridge, by = "anberd_activity", relationship = "many-to-many")
  }

  if (nrow(training_data) > 0L) {
    bridge <- read_delim(
      "obs_accounts/knw/eurostat_correspondence_table_nace_r2.csv",
      delim = ";", show_col_types = FALSE
    ) %>% transmute(industry = figaro_industry, nace_r2)

    training_data <- training_data %>% filter(!is.na(trng_cvt)) %>%
      inner_join(bridge, by = "nace_r2", relationship = "many-to-many")
  }

  if (nrow(stan_data) > 0L) {
    stan_data <- stan_data %>% filter(!is.na(value)) %>%
      distinct(year, country, industry, aggregate) %>%
      count(year, country, industry) %>% filter(n == 2)
  }

  anberd <- coverage(anberd_data)
  training <- coverage(training_data)
  stan <- coverage(stan_data)
  synthesis <- map_dfr(years, function(year_i) {
    rows <- bind_rows(anberd, training, stan) %>% filter(year == year_i)
    tibble(
      year = year_i,
      status = if (all(rows$status == "OK")) "OK" else "NOK",
      available_data_fr = min(rows$available_data_fr),
      available_data_row = min(rows$available_data_row),
      available_data_countries = min(rows$available_data_countries)
    )
  })

  reports <- list(anberd = anberd, trng_cvt = training, stan = stan, synthesis = synthesis)

  # --- Rapports et erreurs de chargement --------------------------------------

  print_accounts_source_checks(reports)
  reports$source_errors <- source_errors
  invisible(reports)
}
