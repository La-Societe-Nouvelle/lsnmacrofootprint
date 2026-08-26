# La Société Nouvelle

####################################################################################################
# UTILS

source("disaggregation/utils.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# TRAITEMENT

build_disaggregated_footprints <<- function(
  YEAR = 2022,
  use_temp_data = FALSE,
  do_update = TRUE
) {
  # ----------------------------------------------------------------------------------------------------
  # Building EEIO Data

  # Fetching UK data
  data_uk_eeio <- read_delim("disaggregation/data_temp/footprints_uk_eeio.csv", delim = ",", show_col_types = FALSE)

  # Fetching US data
  data_us_eeio <- read_delim("disaggregation/data_temp/footprints_us_eeio.csv", delim = ",", show_col_types = FALSE)

  # Fetching CANADA data
  data_canada_eeio <- read_delim("disaggregation/data_temp/footprints_canada_eeio.csv", delim = ",", show_col_types = FALSE)

  # Fetching DENMARK data
  data_denmark_eeio <- read_delim("disaggregation/data_temp/data_denmark_eeio.csv", delim = ",", show_col_types = FALSE)

  # Fetching FIGARO data
  data_figaro <<- fetch_figaro_data(YEAR)
  message("[INFO] Ok - Empreintes EEIO FIGARO")

  # ----------------------------------------------------------------------------------------------------
  # Building Imported embedded emissions

  if (!use_temp_data) {
    imported_fpt_fr <- get_figaro_imported_embedded_emissions(YEAR)
    message("[INFO] Ok - Empreintes (Importations)")
    write.csv(imported_fpt_fr, file = "disaggregation/data_temp/imported_fpt_fr.csv", row.names = FALSE)
  }
  imported_fpt_fr <- read_delim("disaggregation/data_temp/imported_fpt_fr.csv", delim = ",", show_col_types = FALSE)

  # ----------------------------------------------------------------------------------------------------
  # Fetching ESANE Data

  if (!use_temp_data) {
    data_esane <- fetch_esane_data()
    message("[INFO] Ok - Données ESANE")
    write.csv(data_esane, file = "disaggregation/data_temp/data_esane.csv", row.names = FALSE)
  }
  data_esane <- read_delim("disaggregation/data_temp/data_esane.csv", delim = ",", show_col_types = FALSE)
  
  # ----------------------------------------------------------------------------------------------------
  # Fetching prices data

  data_prices <<- fetch_na_prices(YEAR)
  print(data_prices %>% as_tibble())
  print(unique(data_prices$year))
  message("[INFO] Ok - Indices prix")

  # ----------------------------------------------------------------------------------------------------
  # Build A*732 Data

  # --------------------------------------------------
  # Metadata A*732

  metadata_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      code_ape_a732 = code,
      libelle_ape_a732 = label_fr,
      figaro_industry = industry
    ) %>%
    select(code_ape_a732, figaro_industry, libelle_ape_a732)

  # --------------------------------------------------
  # EEIO data Compilation

  data_figaro_eeio <- metadata_nace_niv5 %>%
    merge(data_figaro) %>%
    mutate(
      eeio_model = "FIGARO",
      accuracy_mapping_a732 = "4",
      unit = "GCO2E_EUR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732)

  data_eeio_models <- data_figaro_eeio %>%
    # -------------------------
    # bind EEIO fpt data
    rbind(data_us_eeio) %>%
    rbind(data_uk_eeio) %>%
    rbind(data_canada_eeio) %>%
    rbind(data_denmark_eeio) %>%
    # -------------------------
    # metadata A*732
    merge(metadata_nace_niv5) %>%
    # -------------------------
    # add imported embedded fpt
    merge(imported_fpt_fr) %>% # by figaro_industry
    mutate(
      fpt = if_else(eeio_model == "FIGARO", fpt, fpt + imported_fpt) # /!\ FIGARO imported embedded fpt included
    ) %>%
    # -------------------------
    # filtrage
    group_by(eeio_model, figaro_industry, aggregate, year) %>%
    mutate(
      fpt_min_industry = min(fpt),
      fpt_max_industry = max(fpt),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    merge(data_figaro %>% rename(fpt_ref_industry = fpt)) %>%
    filter(
      accuracy_mapping_a732 < 5            # ignore data less relevant than FIGARO
      # fpt_min_industry <= fpt_ref_industry, # fpt -> ref FIGARO
      # fpt_max_industry >= fpt_ref_industry  # fpt -> ref FIGARO
    ) %>%
    # -------------------------
    # add coef
    mutate(
      coef_accuracy = case_when(
        accuracy_mapping_a732 == "1"  ~ 8, # 5
        accuracy_mapping_a732 == "2"  ~ 5, # 3
        accuracy_mapping_a732 == "3"  ~ 2, # 2
        accuracy_mapping_a732 == "4"  ~ 1,
        T ~ 0
      )
    ) %>%
    # -------------------------
    # ouput
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732, coef_accuracy)

  # --------------------------------------------------
  # EEIO data agregation

  nace_a732_fpt <- metadata_nace_niv5 %>%
    crossing(aggregate = c("FD", "PRD", "IC", "GVA")) %>%
    mutate(year = YEAR) %>%
    # merge EEIO models fpt
    merge(data_eeio_models) %>%
    filter(fpt > 0) %>% # /!\ remove negative fpt (temp)
    group_by(code_ape_a732, aggregate, unit, year) %>%
    summarise(
      fpt = sum(fpt * coef_accuracy) / sum(coef_accuracy),
      accuracy_fpt = sum(as.numeric(accuracy_mapping_a732) * coef_accuracy) / sum(coef_accuracy),
      .groups = "drop"
    ) %>%
    {
      print(as_tibble(.))
    .
    } %>%
    # compute prd fpt with ESANE data
    filter(aggregate %in% c("GVA", "IC", "PRD")) %>%
    # Actualisation des empreintes
    rename(base_year = year) %>%
    merge(data_prices %>% filter(year == "2022")) %>%
    mutate(
      fpt = fpt * coef_price_index
    ) %>%
    # output
    mutate(
      fpt = round(fpt, 0),
      accuracy_fpt = round(accuracy_fpt, 1)
    ) %>%
    merge(metadata_nace_niv5) %>%
    select(year, code_ape_a732, aggregate, fpt, accuracy_fpt, unit, libelle_ape_a732)

  message("Traitement terminé")
  print(nace_a732_fpt %>% as_tibble())

  if (do_update) {
    write.csv(nace_a732_fpt, file = "disaggregation/data_temp/nace_a732_fpt.csv", row.names = FALSE)
  }
}

####################################################################################################