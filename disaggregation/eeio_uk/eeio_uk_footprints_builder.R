# La Société Nouvelle

####################################################################################################
# UTILS

source("disaggregation/utils.R")
source("utils/utils_monetary_conversion.R")

####################################################################################################
# BUILDING UK EEIO DATA

build_uk_eeio_footprints <<- function(year_i, verbose = TRUE)
{
  message("[LOG] Fetching and formatting UK EEIO")

  # ----------------------------------------------------------------------------------------------------
  # Metadata

  eeio_size = 104

  eeio_industries <- read_delim(
      "disaggregation/eeio_uk/metadata_uk_eeio_industries.csv",
      delim = ";",
      na = character(),
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = uk_eeio_industry_code
    ) %>%
    select(eeio_industry)

  table_passage_a732 <- read_delim(
      "disaggregation/eeio_uk/table_passage_a732_uk.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      eeio_industry = code_eeio_uk
    ) %>%
    select(code_ape_a732, eeio_industry, accuracy_mapping_a732)

  metadata_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(
      code_ape_a732 = code,
      figaro_industry = industry
    ) %>%
    select(code_ape_a732, figaro_industry)

  correspondences_figaro <- table_passage_a732 %>%
    merge(metadata_nace_niv5) %>%
    select(eeio_industry, figaro_industry) %>%
    rbind(data.frame(eeio_industry = "L68A", figaro_industry = "L")) %>% # ajout L68A
    distinct()

  correspondences_sic_groups <- read_delim(
      "disaggregation/eeio_uk/metadata_uk_sic_groups.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(sic_group = code) %>%
    select(sic_group, iot_ref)

  # ----------------------------------------------------------------------------------------------------
  # EEIO Data

  url_sut = "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb24.xlsx"

  file_sut = curl_download(url_sut, tempfile())

  # --------------------------------------------------
  # Production (in GBP)

  # Table 104x1

  x = suppressMessages(
        read_xlsx(file_sut, sheet = paste0("Table 1 - Supply ", year_i), skip = 2)
      ) %>%
      filter(grepl('CPA',`...1`)) %>% # garde les lignes commençant par CPA_
      mutate(`...1` = gsub("CPA_", "", `...1`)) %>%
      column_to_rownames('...1') %>%
      rownames_to_column(var = "eeio_industry") %>%
      mutate(
        x = `Total domestic\r\noutput of \r\nproducts at basic prices`,
        year = year_i,
        unit = "GBP"
      ) %>%
      select(eeio_industry, year, x, unit) %>%
      arrange(eeio_industry)

  if (nrow(x) != 104) {
    message("[ERROR] Format incorrect pour X")
    print(x %>% as_tibble())
  }
  message("[INFO] Ok - Table X")

  # --------------------------------------------------
  # Intermediate inputs (in GBP)

  # Table IO 104x104

  z = suppressMessages(
      read_xlsx(file_sut, sheet = paste0("Table 2 - Int Con ", year_i), skip = 3)
    ) %>%
    rename(ons_uk_product = `...1`) %>%
    filter(grepl('CPA',ons_uk_product)) %>% # garde les lignes commençant par CPA_
    mutate(ons_uk_product = sub("^CPA_", "", ons_uk_product)) %>% # Remove CPA_
    column_to_rownames(var = "ons_uk_product") %>%
    # /!\ Exception (different codes used in columns)
    rename(
      `C11.01-6 & C12` = `C1101T1106 & C12`,
      `C241_3` = `C241T243`,
      `F41, F42 & F43` = `F41, F42  & F43`,
      `H493_5` = `H493T495`
    ) %>%
    {.[,-c(1,ncol(.))]} %>% # supprime la première et la dernière colonne
    {
      ids <- sort(intersect(rownames(.), colnames(.)))
      .[ids, ids, drop = FALSE]
    } %>%
    mutate_all(as.numeric)

  if (!identical(rownames(z), colnames(z))) {
    idx <- which(rownames(z) != colnames(z))
    message("[ERROR] rownames != colnames (", length(idx), " différences)")
    print(data.frame(
      i = idx,
      row = rownames(z)[idx],
      col = colnames(z)[idx]
    ) |> head(20))
    stop("Mismatch rownames/colnames dans z")
  }
  if (nrow(z) != 104 || ncol(z) != 104) {
    message("[ERROR] Format incorrect pour Z")
    print(z %>% as_tibble())
    stop("Error dans z")
  }
  message("[INFO] Ok - Table Z")

  # --------------------------------------------------
  # Main aggregates (in GBP)

  intermediate_consumptions <- data.frame(
    eeio_industry = colnames(z),
    p2 = colSums(z)
  )

  main_aggregates <- x %>%
    merge(intermediate_consumptions) %>%
    mutate(
      va = x - p2
    ) %>%
    select(eeio_industry, year, unit, x, p2, va) %>%
    arrange(eeio_industry)

  # --------------------------------------------------
  # Coef PRG / GHG Emissions

  # Table 104x1

  url_emissions_data = "https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/ukenvironmentalaccountsatmosphericemissionsgreenhousegasemissionsbyeconomicsectorandgasunitedkingdom/current/05atmoshpericemissionsghg.xlsx"

  file_emissions_ons_data = curl_download(url_emissions_data, tempfile())

  emissions_data = suppressMessages(
      read_xlsx(file_emissions_ons_data, sheet = "GHG total", skip = 6, col_names = FALSE, col_types = "text")[,-c(1:26)]
    ) %>%
    {
      headers <- as.character(unlist(.[1,-1], use.names = FALSE))
      headers <- ifelse(grepl("^\\d+\\.\\d{10,}$", headers),
                        format(round(suppressWarnings(as.numeric(headers)), 2), scientific = FALSE, trim = TRUE),
                        headers)
      headers <- sub("\\.$", "", ifelse(grepl("\\.", headers), sub("0+$", "", headers), headers))
      names(.) <- c("year", headers)
      .
    } %>%
    .[-c(1,2), ] %>%
    pivot_longer(-year, names_to = "sic_group", values_to = "value") %>%
    filter(sic_group != "Total", year == year_i, !(sic_group %in% c("100","101"))) %>%
    mutate(
      value = ifelse(
        value == "[low]" | is.na(value),
        NA_real_,
        round(as.numeric(value), 2)
      )
    ) %>%
    left_join(correspondences_sic_groups, by = "sic_group") %>%
    group_by(iot_ref) %>%
    summarise(
      emissions = 1000 * sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rbind(data.frame(iot_ref = "L68A", emissions = 0)) %>% # ajout L68A
    rename(eeio_industry = iot_ref) %>%
    select(eeio_industry, emissions) %>%
    arrange(eeio_industry)

  if (nrow(emissions_data) != 104) {
    message("[ERROR] Format incorrect pour emissions_data")
    print(emissions_data %>% as_tibble())
  }
  message("[INFO] Ok - Table Emissions")

  # --------------------------------------------------
  # Compute footprints

  # Table 104x4 (Aggregates : PRD, IC, GVA, DF)

  ghg_fpt = compute_ghg_fpt("GB", z, main_aggregates, emissions_data, correspondences_figaro, year_i)

  # --------------------------------------------------
  # Monetary conversion

  pound_eur <- from_pound_to_euro(year_i)

  ghg_fpt_eur <- ghg_fpt %>%
    mutate(
      fpt = fpt*pound_eur,
      unit = "GCO2E_EUR"
    ) %>%
    select(eeio_country, eeio_industry, aggregate, fpt, unit, year)

  # --------------------------------------------------
  # Mapping A*732

  ghg_fpt_a732 <- ghg_fpt_eur %>%
    merge(table_passage_a732) %>%
    group_by(eeio_country, aggregate, unit, year, code_ape_a732, accuracy_mapping_a732) %>%
    summarise(
      fpt = round(mean(fpt, na.rm = TRUE), digits = 0),
      .groups = "drop"
    ) %>%
    mutate(
      eeio_model = "EEIO_UK",
      country = "FR"
    ) %>%
    select(eeio_model, country, code_ape_a732, aggregate, fpt, unit, year, accuracy_mapping_a732) %>%
    arrange(year, code_ape_a732, aggregate)

  # --------------------------------------------------

  print(ghg_fpt_a732 %>% as_tibble())

  write.csv(
    ghg_fpt_a732,
    file = "disaggregation/data_temp/footprints_uk_eeio.csv",
    row.names = FALSE
  )
  message("[INFO] Ok - Empreintes EEIO UK")
}