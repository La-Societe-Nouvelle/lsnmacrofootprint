# La Société Nouvelle

####################################################################################################

# Etapes :
#   1- Construction de la matrice des consommations intermédiaires domestiques sur le niveau du modèle EEIO
#   2- Construction des intensités directes
#   3- Calcul des empreintes/facteurs par industrie

compute_ghg_fpt <<- function(eeio_country, z, main_aggregates, emissions_data, correspondences_figaro, year_i)
{
  # --------------------------------------------------
  # Metadata

  figaro_industries <- read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(
      figaro_industry = code
    ) %>%
    select(figaro_industry)

  correspondence <- correspondences_figaro %>%
    group_by(eeio_industry) %>%
    mutate(share = 1 / n()) %>%
    ungroup() %>%
    select(eeio_industry, figaro_industry, share)

  # --------------------------------------------------
  # 1- Construire la matrice des consommations intermédiaires domestiques sur le niveau du modèle EEIO

  # -------------------------
  # Estimation des parts "domestiques" des consommations intermédiaires, à partir des données FR

  # Table 4096x1

  figaro_intermediate_inputs_fr_raw_data <- load_local_figaro_intermediate_inputs(year_i)

  domestic_share_intermediate_inputs_fr <- figaro_intermediate_inputs_fr_raw_data %>%
    filter(
      use_country == "FR"
    ) %>%
    group_by(use_country, use_industry, resource_industry) %>%
    summarise(
      total_resources = sum(value, na.rm = TRUE),
      domestic_resources = sum(value[resource_country == "FR"], na.rm = TRUE),
      domestic_share = if_else(total_resources == 0, 0, domestic_resources / total_resources),
      .groups = "drop"
    ) %>%
    select(use_country, use_industry, resource_industry, domestic_share)

  # -------------------------
  # Projection des parts domestiques sur le niveau du modèle EEIO via correspondences_figaro

  # Matrice (size_eeiox64) avec nombre de correspondances (pondérations de ventilation par industrie)

  M <- correspondences_figaro %>%
    # completion pour alignement A*64
    mutate(
      figaro_industry = factor(
        figaro_industry,
        levels = figaro_industries$figaro_industry
      )
    ) %>%
    group_by(eeio_industry) %>%
    mutate(share = 1 / n()) %>%
    ungroup() %>%
    arrange(eeio_industry, figaro_industry) %>%
    xtabs(share ~ eeio_industry + figaro_industry, data = .) %>%
    as.matrix()

  # Matrice (64x64) des parts domestiques des consommations intermédiaires - Nomenclature FIGARO

  D <- domestic_share_intermediate_inputs_fr %>%
    arrange(use_industry, resource_industry) %>%
    xtabs(domestic_share ~ use_industry + resource_industry, data = .) %>%
    as.matrix()

  # Matrice (size_eeioxsize_eeio) des parts domestiques (FR) des consommations intermédiaires - Nomenclature EEIO

  z_domestic_shares <- M %*% D %*% t(M)

  # -------------------------
  # Construction de la matrice des consommations intermédiaires domestiques

  # Matrice (size_eeioxsize_eeio) des consommations intermédiaires domestiques - Nomenclature EEIO

  z_domestic_inputs <- z * z_domestic_shares

  message("[INFO] OK - Z Domestic Inputs")

  # ----------------------------------------------------------------------------------------------------
  # 2- Construction des intensités directes

  # -------------------------
  # Récupération des données FIGARO / données d'émission

  ghg_obs_data_filepath <- file.path("data_output", "accounts_obs_ghg.csv")
  direct_impacts_ghg_raw_data <- read.csv(ghg_obs_data_filepath)

  direct_impacts_ghg_data <- direct_impacts_ghg_raw_data %>%
    filter(year == year_i)

  figaro_main_aggregates_raw_data <- load_local_figaro_main_aggregates(year_i)

  figaro_main_aggregates_data <- figaro_main_aggregates_raw_data %>%
    pivot_wider(
      values_from = "value",
      names_from = "aggregate"
    ) %>%
    select(country, industry, year, PRD)

  # Emissions directes - Nomenclature FIGARO (128x1)

  figaro_impacts_ghg <- direct_impacts_ghg_data %>%
    filter(country %in% c("FR", eeio_country)) %>%
    rename(emissions = value) %>%
    select(country, industry, year, emissions)

  # Production - Nomenclature FIGARO (128x1)

  figaro_prd <- figaro_main_aggregates_data %>%
    filter(country %in% c("FR", eeio_country)) %>%
    rename(X = PRD) %>%
    select(country, industry, year, X)

  # Production GHG intensities - Nomenclature FIGARO (128x1)

  figaro_ghg_intensities <- figaro_impacts_ghg %>%
    merge(figaro_prd) %>%
    mutate(
      ghg_intensity = if_else(X == 0, 0, emissions / X)
    ) %>%
    select(country,industry,year,emissions,X,ghg_intensity)

  # -------------------------
  # Ecart des intensités (all industries)

  gap_ratio_by_industry <- figaro_ghg_intensities %>%
    select(country,industry,year,ghg_intensity) %>%
    # Compute gap ratios
    pivot_wider(names_from = country, values_from = ghg_intensity) %>%
    mutate(gap_ratio = FR / .data[[eeio_country]]) %>%
    mutate(
      figaro_industry = industry,
      figaro_coef_corr = ifelse(is.finite(gap_ratio), gap_ratio, 1.0)
    ) %>%
    select(year,figaro_industry,figaro_coef_corr)

  # -------------------------
  # Coefficients correcteurs - Nomenclature EEIO

  ghg_intensities_corr <- figaro_industries %>%
    merge(gap_ratio_by_industry) %>%
    merge(correspondence) %>%
    group_by(eeio_industry) %>%
    summarise(
      coef_corr = sum(figaro_coef_corr * share),
      .groups = "drop"
    ) %>%
    select(eeio_industry, coef_corr)

  # GHG intensities by FIGARO industry
  eeio_ghg_intensities <- emissions_data %>%
    merge(main_aggregates) %>%
    merge(correspondence) %>%
    group_by(figaro_industry) %>%
    summarise(
      emissions = sum(emissions * share),
      x = sum(x * share),
      .groups = "drop"
    ) %>%
    mutate(
      industry = figaro_industry,
      eeio_ghg_intensity = if_else(x > 0, (emissions / x), 0)
    ) %>%
    select(industry,eeio_ghg_intensity)

  ghg_intensities_corr_bis <- figaro_ghg_intensities %>%
    filter(country == "FR") %>%
    merge(eeio_ghg_intensities) %>%
    mutate(
      figaro_industry = industry,
      coef_corr = if_else(eeio_ghg_intensity > 0, ghg_intensity / eeio_ghg_intensity, 1.0)
    ) %>%
    select(figaro_industry,coef_corr) %>%
    merge(correspondence) %>%
    group_by(eeio_industry) %>%
    summarise(
      coef_corr = sum(coef_corr * share),
      .groups = "drop"
    ) %>%
    # filter(is.finite(coef_corr)) %>%
    select(eeio_industry, coef_corr)

  # --------------------------------------------------
  # Calcul des intensités d'émission (avec corrections)

  direct_ghg_intensity <- emissions_data %>%
    merge(main_aggregates) %>%
    left_join(ghg_intensities_corr_bis, by = "eeio_industry") %>%
    mutate(
      coef_corr = coalesce(coef_corr, 1),
      ghg_intensity = if_else(x > 0, (emissions / x) * coef_corr, 0)
    ) %>%
    select(eeio_industry, ghg_intensity)

  impact_vector <- direct_ghg_intensity %>%
    arrange(eeio_industry) %>%
    pull(ghg_intensity) %>%
    as.numeric()

  # ----------------------------------------------------------------------------------------------------
  # 3- Calcul des empreintes/facteurs par industrie

  A <- sweep(z_domestic_inputs, 2, main_aggregates$x, "/") %>% as.matrix()
  A[is.nan(A) | is.infinite(A)] = 0 ; diag(A)[diag(A) == 1] = 0.995
  I <- diag(1, nrow(A))
  L <- solve(I - A)

  # --------------------------------------------------
  # Empreintes - Demande finale

  fpt_fd_data <- as.numeric(t(impact_vector) %*% L)  # empreinte par unité de demande finale
  fpt_fd <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "FD",
    fpt = fpt_fd_data,
    year = year_i
  )

  # print("Empreintes FD ok")
  # print(fpt_fd %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Production

  fpt_prd_data  <- fpt_fd_data / diag(L)                      # conversion en "par unité de production"
  fpt_prd <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "PRD",
    fpt = fpt_prd_data,
    year = year_i
  )

  # print("Empreintes PRD ok")
  # print(fpt_prd %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Valeur ajoutée brute

  fpt_gva_data <- (impact_vector * main_aggregates$x) / main_aggregates$va
  fpt_gva <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "GVA",
    fpt = fpt_gva_data,
    year = year_i
  )

  # print("Empreintes GVA ok")
  # print(fpt_gva %>% as_tibble())

  # --------------------------------------------------
  # Empreintes - Consommations intermédiaires

  fpt_ic_data <- (fpt_prd_data * main_aggregates$x - fpt_gva_data * main_aggregates$va) / main_aggregates$p2
  fpt_ic <- data.frame(
    eeio_country = eeio_country,
    eeio_industry = emissions_data$eeio_industry,
    aggregate = "IC",
    fpt = fpt_ic_data,
    year = year_i
  )

  # print("Empreintes IC ok")
  # print(fpt_ic %>% as_tibble())

  # --------------------------------------------------
  # 8- Retour

  results <- fpt_fd %>%
    rbind(fpt_prd) %>%
    rbind(fpt_gva) %>%
    rbind(fpt_ic)

  # print(results %>% as_tibble())

  return(results)
}


####################################################################################################
# FIGARO IMPORTED EMBEDDED EMISSIONS

get_figaro_imported_embedded_emissions <<- function(year_i)
{
  # --------------------------------------------------
  # Fetch FIGARO Model

  # Intermediate inputs
  Z <- load_local_figaro_intermediate_inputs(year_i)

  # Production
  X <- load_local_figaro_main_aggregates(year_i) %>%
    filter(aggregate == "PRD") %>%
    rename(
      x = value
    ) %>%
    select(country, industry, year, x)

  # Emissions
  ghg_obs_data_filepath <- file.path("data_output", "accounts_obs_ghg.csv")
  direct_impacts_ghg_raw_data <- read.csv(ghg_obs_data_filepath)
  E <- direct_impacts_ghg_raw_data %>%
    filter(year == year_i) %>%
    rename(
      emissions = value
    ) %>%
    select(country, industry, year, emissions)

  # --------------------------------------------------
  # Compute footprint

  C = E %>%
    merge(X) %>%
    mutate(
      value = case_when(
        country == "FR" ~ 0,
        TRUE ~ replace_na(emissions / x, 0)
      )
    ) %>%
    pull(value) %>%
    as.numeric()

  A = Z %>%
    merge(X, by.x = c("use_country","use_industry"), by.y = c("country","industry")) %>%
    mutate(
      value = if_else(x > 0, value / x, 0)
    ) %>%
    arrange(use_country, use_industry, resource_country, resource_industry) %>%
    select(use_id, resource_id, value) %>%
    pivot_wider(
      names_from = "use_id",
      values_from = "value"
    ) %>%
    column_to_rownames("resource_id")

  L = solve(diag(nrow = nrow(A)) - A)

  fpt_raw_data = sweep(sweep(L, 2, diag(L), `/`), 1, C, `*`)

  fpt_data <- fpt_raw_data %>%
    as.data.frame() %>%
    rownames_to_column("resource_id") %>%
    pivot_longer(
      cols = -resource_id,
      names_to = "use_id",
      values_to = "fpt"
    ) %>%
    mutate(
      use_country  = sub("_.*$", "", use_id),
      use_industry = sub("^[^_]*_", "", use_id),
      resource_country  = sub("_.*$", "", resource_id),
      resource_industry = sub("^[^_]*_", "", resource_id),
      year = year_i
    )

  # --------------------------------------------------
  # Imported fpt for FR + A*732

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

  imported_fpt_fr <- fpt_data %>%
    filter(
      use_country == "FR",
      resource_country != "FR",
      year == year_i
    ) %>%
    group_by(use_country, use_industry, year) %>%
    summarise(
      imported_fpt = sum(fpt),
      .groups = "drop"
    ) %>%
    rename(
      country = use_country,
      figaro_industry = use_industry
    ) %>%
    merge(metadata_nace_niv5) %>%
    select(code_ape_a732, year, imported_fpt)

  return(imported_fpt_fr)
}

####################################################################################################
# ESANE DATA

fetch_esane_data <<- function()
{
  url_esane_data = "https://www.insee.fr/fr/statistiques/fichier/8241021/DD_esane22ep_caracteristiques.xlsx"

  file_esane_data = curl_download(url_esane_data, tempfile())

  esane_raw_data = file_esane_data %>%
    read_xlsx(
      skip = 10,
      col_types = "text"
    ) %>%
    rename(
      niveau_naf = `NIVEAU NAF`,
      secteur_activite = `Secteur d'activité`,
      chiffre_affaires_ht = `Chiffre d'affaires Hors Taxes`,
      valeur_ajoutee = `Valeur ajoutée - y compris autres produits et autres charges`
    ) %>%
    mutate(across(-c(niveau_naf,secteur_activite), as.numeric)) %>%
    filter(
      !is.na(valeur_ajoutee),
      chiffre_affaires_ht > 0
    ) %>%
    reframe(
      niveau_naf,
      secteur_activite,
      taux_va = valeur_ajoutee / chiffre_affaires_ht
    ) %>%
    arrange(secteur_activite)

  # -------------------------
  # A*732 Format

  metadata_nace_niv5 <- read_delim(
      "metadata/metadata_nace_niv5.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    mutate(
      code_ape_a732 = code,
      nace_niv5 = gsub("\\.", "", code),
      nace_niv4 = gsub("\\.", "", classe),
      nace_niv3 = gsub("\\.", "", groupe),
      nace_niv2 = division,
      sector = section
    ) %>%
    filter(code_ape_a732 != "00.00Z") %>%
    select(code_ape_a732, nace_niv5, nace_niv4, nace_niv3, nace_niv2, sector)

  data_esane_a732 <- esane_raw_data %>% filter(niveau_naf == "a732") %>% reframe(nace_niv5 = secteur_activite, taux_va_a732 = taux_va)
  data_esane_a615 <- esane_raw_data %>% filter(niveau_naf == "a615") %>% reframe(nace_niv4 = secteur_activite, taux_va_a615 = taux_va)
  data_esane_a272 <- esane_raw_data %>% filter(niveau_naf == "a272") %>% reframe(nace_niv3 = secteur_activite, taux_va_a272 = taux_va)
  data_esane_a88  <- esane_raw_data %>% filter(niveau_naf == "a88")  %>% reframe(nace_niv2 = secteur_activite, taux_va_a88 = taux_va)
  data_esane_a21  <- esane_raw_data %>% filter(niveau_naf == "a21")  %>% reframe(sector = secteur_activite, taux_va_a21 = taux_va)
  # data_esane_a10  <- esane_raw_data %>% filter(niveau_naf == "a10")  %>% reframe(nace_a10 = secteur_activite, taux_va_a10 = taux_va)

  data_esane = metadata_nace_niv5 %>%
    left_join(data_esane_a732, by = "nace_niv5") %>%
    left_join(data_esane_a615, by = "nace_niv4") %>%
    left_join(data_esane_a272, by = "nace_niv3") %>%
    left_join(data_esane_a88,  by = "nace_niv2") %>%
    left_join(data_esane_a21,  by = "sector") %>%
    # left_join(data_esane_a10,  by = "nace_a10") %>%
    mutate(
      taux_va = coalesce(
        taux_va_a732,
        taux_va_a615,
        taux_va_a272,
        taux_va_a88,
        taux_va_a21
        # taux_va_a10
      )
    ) %>%
    select(code_ape_a732, taux_va) %>%
    arrange(code_ape_a732)

  return(data_esane)
}

####################################################################################################
# FIGARO DATA

fetch_figaro_data <<- function(year_i)
{
  macro_fpt_data_filepath <- file.path("data_output", "footprints_obs_ghg.csv")
  figaro_macro_fpt_raw_data <- read.csv(macro_fpt_data_filepath)

  figaro_macro_fpt <- figaro_macro_fpt_raw_data %>%
    filter(
      year == year_i,
      country == "FR"
    ) %>%
    rename(
      fpt = value,
      figaro_industry = industry
    ) %>%
    select(country, figaro_industry, aggregate, year, fpt)

  return(figaro_macro_fpt)
}

####################################################################################################
# INDICES PRIX

fetch_na_prices <<- function(year_i)
{
  na_prices_filepath <- file.path(
    "data_figaro",
    "figaro_na_prices.parquet"
  )

  na_prices_raw <- read_parquet(na_prices_filepath)

  na_prices <- na_prices_raw %>%
    filter(aggregate == "PRD") %>%
    mutate(
      price_index = value,
      figaro_industry = industry
    ) %>%
    select(figaro_industry, year, price_index)

  base_indexes <- na_prices %>%
    filter(year == year_i) %>%
    mutate(
      base_index = price_index,
      base_year = year
    ) %>%
    select(figaro_industry, base_year, base_index)

  prices_indexes <- na_prices %>%
    merge(base_indexes) %>%
    mutate(
      coef_price_index = base_index / price_index
    ) %>%
  select(figaro_industry, year, base_year, coef_price_index) %>%
  arrange(figaro_industry, year)

  return(prices_indexes)
}
