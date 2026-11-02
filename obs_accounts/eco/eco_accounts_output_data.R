# La Societe Nouvelle

# Controle des donnees partagees avec le producteur, avant imputation.
# Les caches sont consultes localement ; refresh_sources autorise leur acquisition.
check_eco_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {

  # --- Sources et perimetre --------------------------------------------------

  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/eco/eco_accounts_sources.R", encoding = "UTF-8-BOM")

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

  # Lire chaque fichier separement pour conserver les annees disponibles si
  # un fichier d'une autre annee est absent ou invalide.
  source("utils/utils_figaro_data.R", encoding = "UTF-8-BOM")

  data <- map_dfr(years, function(year_i) {
    load_source(paste0("figaro_", year_i), load_figaro_eco_source, year_i)
  })

  if (nrow(data) > 0L) data <- data %>% filter(aggregate == "NVA", !is.na(value))
  figaro <- coverage(data)
  reports <- list(figaro_main_aggregates = figaro, synthesis = figaro)

  # --- Rapports et erreurs de chargement --------------------------------------

  print_accounts_source_checks(reports)
  reports$source_errors <- source_errors
  invisible(reports)
}
