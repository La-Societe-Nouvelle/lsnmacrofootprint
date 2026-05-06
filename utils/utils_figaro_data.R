# La Société Nouvelle

# ----------------------------------------------------------------------------------------------------
#' Fonction pour télécharger les données FIGARO

load_figaro_data_from_remote <- function(

) {
  # ...
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
      use_id = paste0(use_country, "_", use_industry),
      resource_id = paste0(resource_country, "_", resource_industry)
    ) %>%
    select(use_id, use_country, use_industry, resource_id, resource_country, resource_industry, value)
}
