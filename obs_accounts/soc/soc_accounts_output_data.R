# La Societe Nouvelle

# Controle des donnees partagees avec le producteur, avant imputation.
# Les caches sont consultes localement ; refresh_sources autorise leur acquisition.
check_soc_obs_accounts_output_data <- function(
  years = 2010:2025,
  use_temp_data = TRUE,
  verbose = FALSE,
  refresh_sources = FALSE
) {

  # --- Sources et perimetre --------------------------------------------------

  source("utils/utils_accounts_sources.R", encoding = "UTF-8-BOM")
  source("obs_accounts/soc/soc_accounts_sources.R", encoding = "UTF-8-BOM")

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

  data <- load_source("urssaf", load_urssaf_soc_source)

  if (nrow(data) > 0L) {
    bridge <- read_delim(
      "obs_accounts/soc/table_passage_urssaf.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>% transmute(industry = figaro_industry, secteur_na88)

    data <- data %>%
      filter(!is.na(masse_salariale)) %>%
      inner_join(bridge, by = "secteur_na88", relationship = "many-to-many") %>%
      mutate(year = as.character(annee), country = "FR")
  }

  urssaf <- coverage(data, scope = "fr_industry", structural_zero_row = TRUE)
  reports <- list(urssaf = urssaf, synthesis = urssaf)

  # --- Rapports et erreurs de chargement --------------------------------------

  print_accounts_source_checks(reports)
  reports$source_errors <- source_errors
  invisible(reports)
}
