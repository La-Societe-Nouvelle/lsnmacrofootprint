# Temp utils to download FIGARO data

# --------------------------------------------------
# Paramètres
# --------------------------------------------------

years <- 2010:2030
data_dir <- "data_figaro"

# con <- ... ta connexion db
con <<- get_connection_db()

# --------------------------------------------------
# Utilitaire d'export
# --------------------------------------------------

write_figaro_parquet <- function(df, table_name, year_i) {
  output_filepath <- file.path(data_dir, paste0(table_name, "_", year_i, ".parquet"))

  write_parquet(df, output_filepath)

  message("Export terminé : ", output_filepath)
}

# --------------------------------------------------
# 1. figaro_capital_use
# --------------------------------------------------

get_figaro_capital_use <- function(con, year_i) {
  query <- paste0(
    "SELECT year, use_country, use_industry, resource_country, resource_industry, value ",
    "FROM models.figaro_capital_use ",
    "WHERE year = '", year_i, "'"
  )

  dbGetQuery(con, query) %>%
    mutate(
      year = as.character(year),
      value = as.numeric(value)
    ) %>%
    select(year, use_country, use_industry, resource_country, resource_industry, value)
}

# --------------------------------------------------
# 2. figaro_intermediate_inputs
# --------------------------------------------------

get_figaro_intermediate_inputs <- function(con, year_i) {
  query <- paste0(
    "SELECT year, use_country, use_industry, resource_country, resource_industry, value ",
    "FROM models.figaro_intermediate_inputs ",
    "WHERE year = '", year_i, "'"
  )

  dbGetQuery(con, query) %>%
    mutate(
      year = as.character(year),
      value = as.numeric(value)
    ) %>%
    select(year, use_country, use_industry, resource_country, resource_industry, value)
}

# --------------------------------------------------
# 3. figaro_main_aggregates corrigée
# --------------------------------------------------

get_figaro_main_aggregates <- function(con, year_i, data_dir = "data_figaro") {
  main_query <- paste0(
    "SELECT year, country, industry, aggregate, value ",
    "FROM models.figaro_main_aggregates ",
    "WHERE year = '", year_i, "'"
  )

  main_aggregates_raw <- dbGetQuery(con, main_query) %>%
    mutate(
      year = as.character(year),
      value = as.numeric(value)
    ) %>%
    filter(industry != "TOTAL")

  capital_use_filepath <- file.path(data_dir, paste0("figaro_capital_use_", year_i, ".parquet"))

  capital_use <- read_parquet(capital_use_filepath) %>%
    mutate(
      year = as.character(year),
      value = as.numeric(value)
    ) %>%
    select(year, use_country, use_industry, resource_country, resource_industry, value)

  cfc_aggregates <- capital_use %>%
    group_by(year, use_country, use_industry) %>%
    summarise(CFC = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(country = use_country, industry = use_industry)

  main_aggregates_wide <- main_aggregates_raw %>%
    pivot_wider(names_from = aggregate, values_from = value) %>%
    select(year, country, industry, X, VA, D, P2)

  main_aggregates_corrected <- main_aggregates_wide %>%
    left_join(cfc_aggregates, by = c("year", "country", "industry")) %>%
    mutate(
      CFC = if_else(is.na(CFC), 0, CFC),
      PRD = X,
      GVA = VA,
      IC  = P2,
      NVA = VA - CFC
    ) %>%
    pivot_longer(
      cols = c(PRD, IC, CFC, NVA, GVA, D),
      names_to = "aggregate",
      values_to = "value"
    ) %>%
    mutate(
      value = if_else(is.finite(value), value, 0),
      value = round(value, digits = 3),
      flag = ""
    ) %>%
    select(year, country, industry, aggregate, value, flag) %>%
    arrange(country, industry, aggregate)

  main_aggregates_corrected
}

# --------------------------------------------------
# Boucle annuelle
# --------------------------------------------------

walk(years, \(year_i) {
  message("Traitement année : ", year_i)

  # figaro_capital_use <- get_figaro_capital_use(con, year_i)
  # write_figaro_parquet(figaro_capital_use, "figaro_capital_use", year_i)

  # figaro_intermediate_inputs <- get_figaro_intermediate_inputs(con, year_i)
  # write_figaro_parquet(figaro_intermediate_inputs, "figaro_intermediate_inputs", year_i)

  figaro_main_aggregates <- get_figaro_main_aggregates(con, year_i, data_dir)
  write_figaro_parquet(figaro_main_aggregates, "figaro_main_aggregates", year_i)

  rm(figaro_main_aggregates, figaro_capital_use, figaro_intermediate_inputs)
  gc()
})
