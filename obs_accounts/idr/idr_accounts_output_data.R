# La Societe Nouvelle

# Controle des donnees partagees avec le producteur, avant imputation.
# Les caches sont consultes localement ; refresh_sources autorise leur acquisition.
check_idr_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {

  # --- Sources et perimetre --------------------------------------------------

  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/idr/idr_accounts_sources.R", encoding = "UTF-8-BOM")

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

  bts_data <- map_dfr(years, function(year_i) {
    load_source(paste0("bts_", year_i), load_bts_idr_source, year_i)
  })

  if (nrow(bts_data) > 0L) {
    bts_data <- bts_data %>%
      filter(!is.na(working_hours), !is.na(net_pay)) %>%
      transmute(year, country, industry = branch)
  }

  bts <- coverage(bts_data, scope = "fr_industry", units = unique(industries$branch))
  ilostat_data <- load_source("ilostat", load_ilostat_idr_source)

  if (nrow(ilostat_data) > 0L) {
    ilostat_data <- ilostat_data %>%
      filter(!is.na(value)) %>%
      distinct(year, country, decile) %>%
      count(year, country) %>%
      filter(n == 4)
  }

  ilostat <- coverage(ilostat_data, scope = "country")
  synthesis <- bts %>%
    select(year, status_bts = status, available_data_fr) %>%
    left_join(select(ilostat, year, status_ilostat = status, available_data_row, available_data_countries), by = "year") %>%
    transmute(
      year,
      status = if_else(status_bts == "OK" & status_ilostat == "OK", "OK", "NOK"),
      available_data_fr, available_data_row, available_data_countries
    )

  reports <- list(bts = bts, ilostat = ilostat, synthesis = synthesis)

  # --- Rapports et erreurs de chargement --------------------------------------

  print_accounts_source_checks(reports)
  reports$source_errors <- source_errors
  invisible(reports)
}
