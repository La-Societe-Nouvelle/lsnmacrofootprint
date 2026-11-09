# La Societe Nouvelle

# Controle des donnees partagees avec le producteur, avant imputation.
# Les caches sont consultes localement ; refresh_sources autorise leur acquisition.
check_art_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {

  # --- Sources et perimetre --------------------------------------------------

  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/art/art_accounts_sources.R", encoding = "UTF-8-BOM")

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

  stock <- load_source("StockEtablissements", load_sirene_art_stock_source)
  history <- map_dfr(years, function(year_i) {
    load_source(paste0("StockEtablissementsHistorique_", year_i), load_sirene_art_history_source, year_i)
  })

  bridge <- read_delim(
    "metadata/metadata_nace_niv5.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>% transmute(code_ape = gsub("\\.", "", code), industry)

  map_industries <- function(data) {
    if (nrow(data) == 0L) return(data)
    data %>%
      mutate(code_ape = gsub("\\.", "", activitePrincipaleEtablissement)) %>%
      inner_join(bridge, by = "code_ape", relationship = "many-to-many") %>%
      mutate(country = "FR")
  }

  stock <- map_industries(stock)
  history <- map_industries(history)
  stock_available <- nrow(stock) > 0L
  if (stock_available) stock <- stock %>% crossing(year = years)
  stock_check <- coverage(stock, structural_zero_row = TRUE)
  history_check <- coverage(history, structural_zero_row = TRUE)
  synthesis <- history_check

  if (!stock_available) {
    synthesis <- synthesis %>% mutate(
      status = "NOK", available_data_fr = 0,
      available_data_row = 0, available_data_countries = 0
    )
  }

  reports <- list(
    StockEtablissements = paste0("Stock courant : couverture France ", stock_check$available_data_fr[1], " %"),
    StockEtablissementsHistorique = history_check,
    synthesis = synthesis
  )

  # --- Rapports et erreurs de chargement --------------------------------------

  print_accounts_source_checks(reports)
  reports$source_errors <- source_errors
  invisible(reports)
}
