# La Societe Nouvelle

# Chargeurs partages par le producteur et le controle de disponibilite.

# =============================================================================
# 1. FIGARO : agregats economiques par pays, branche et annee
# =============================================================================

load_figaro_eco_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  map_dfr(as.character(years), function(
    year_i
  ) {
    filename <- paste0("figaro_main_aggregates_", year_i, ".parquet")

    data <- read_accounts_file_source(
      file.path(figaro_data_dir, filename),
      fetch = function(
        destination
      ) {
        download_figaro_file(
          filename,
          verbose = verbose,
          data_dir = figaro_data_dir,
          destfile = destination
        )
      },
      # Eviter de garder le cache verrouille sous Windows apres sa lecture.
      read_file = function(
        path
      ) read_parquet(path, mmap = FALSE),
      columns = c("country", "industry", "year", "aggregate", "value"),
      use_temp_data = use_temp_data,
      allow_download = allow_download
    )

    data %>%
      filter(industry != "TOTAL", as.character(year) == year_i) %>%
      mutate(id = paste0(country, "_", industry)) %>%
      select(id, year, country, industry, aggregate, value)
  })
}
