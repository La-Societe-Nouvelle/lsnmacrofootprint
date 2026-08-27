
# La Société Nouvelle

# build_footprints() ----------------------------------------------------------
# Generates footprint indicators (PRD, VA, IC, plus derived NVA and CFC)
# for the requested years and serie by:
#   1. Load economic supply–use data, compute the Leontief inverse, and gather
#      direct impact vectors.
#   2. Convert these inputs into per‑branch footprints, then aggregate to
#      broader sector totals.
#   3. Optionally apply price adjustment, then return a consolidated data
#      frame of footprints by indicator, year, and aggregate.
# Then returning a consolidated data frame of footprints for each indicator, year, and aggregate.

build_footprints <- function(
  serie_id,
  verbose = FALSE
) {
  if (verbose) message(paste0("Building footprints for serie ", serie_id))
  # --------------------------------------------------
  # Utils

  # ...

  # --------------------------------------------------
  # Metadata

  if (verbose) message("Loading metadata...")

  # Indics
  metadata_indics <- read_delim(
      "metadata/metadata_indics.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(indic = code) %>%
    select(indic,type,defaultprecision,min,max)

  # FIGARO - Industries
  figaro_industries = read_delim(
      "metadata/metadata_figaro_industries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    filter(code != "TOTAL") %>%
    rename(industry = code) %>%
    select(industry)

  # FIGARO - Countries
  figaro_countries <- read_delim(
      "metadata/metadata_figaro_countries.csv",
      delim = ";",
      show_col_types = FALSE
    ) %>%
    rename(country = code) %>%
    select(country)

  # --------------------------------------------------
  # Load Accounts data

  if (verbose) print(paste0("Loading accounts data for serie ", serie_id))

  parts <- strsplit(serie_id, "_")[[1]]
  indic_i <- toupper(parts[1])
  serie_type <- parts[2]

  accounts_data_file_name <- paste0("accounts", "_", serie_type, "_", tolower(indic_i), ".csv")
  accounts_data_file_path <- file.path(output_dir, accounts_data_file_name)

  accounts_data_raw <- read.csv(accounts_data_file_path)

  accounts_data <- accounts_data_raw %>%
    select(serie_id, year, country, industry, value, flag)

  years <- accounts_data %>%
    pull(year) %>%
    unique() %>%
    as.character()

  # --------------------------------------------------

  footprints_data <- NULL

  for (year_i in years)
  {
    if (verbose) print(paste0("Processing year ", year_i))

    # --------------------------------------------------
    # Loading FIGARO data

    # -------------------------
    # FIGARO data files

    # Main aggregates

    main_aggregates_filename <- paste0("figaro_main_aggregates_", year_i, ".parquet")
    main_aggregates_filename_filepath <- file.path("data_figaro/", main_aggregates_filename)

    main_aggregates <- read_parquet(main_aggregates_filename_filepath) %>%
      filter(industry != "TOTAL") %>%
      mutate(
        id = paste0(country, "_", industry),
        amount = value
      ) %>%
      select(id, year, country, industry, aggregate, amount)

    # intermediate inputs

    intermediate_inputs_filename <- paste0("figaro_intermediate_inputs_", year_i, ".parquet")
    intermediate_inputs_filepath <- file.path("data_figaro/", intermediate_inputs_filename)

    intermediate_inputs <- read_parquet(intermediate_inputs_filepath) %>%
      mutate(
        use_id = paste0(use_country, "_", use_industry),
        resource_id = paste0(resource_country, "_", resource_industry)
      ) %>%
      select(use_id, resource_id, value)

    # Capital use

    capital_use_filename <- paste0("figaro_capital_use_", year_i, ".parquet")
    capital_use_filepath <- file.path("data_figaro/", capital_use_filename)

    capital_use <- read_parquet(capital_use_filepath) %>%
      mutate(
        use_id = paste0(use_country, "_", use_industry),
        resource_id = paste0(resource_country, "_", resource_industry)
      ) %>%
      select(use_id, resource_id, value)

    # -------------------------
    # Matrixes

    # Intermediata inputs (Z)
    z <- intermediate_inputs %>%
      select(use_id, resource_id, value) %>%
      arrange(use_id, resource_id) %>%
      pivot_wider(names_from = "use_id") %>%
      column_to_rownames("resource_id") %>%
      as.matrix()

    # Capital use (K)
    k <- capital_use %>%
      select(use_id, resource_id, value) %>%
      arrange(use_id, resource_id) %>%
      pivot_wider(names_from = "use_id") %>%
      column_to_rownames("resource_id") %>%
      as.matrix()

    # Final demand (D)
    d <- main_aggregates %>%
      filter(aggregate == "D") %>%
      select(id, amount) %>%
      arrange(id) %>%
      column_to_rownames("id") %>%
      as.matrix()

    # Production (X)
    x <- rowSums(z) + d
    x[x < 0 | is.na(x)] <- 0

    # Intermediates consumptions
    ic <- colSums(z)
    ic[ic < 0] <- 0

    # Consumptions of fixed capital
    cfc <- colSums(k)
    cfc[cfc < 0] <- 0

    # Gross value added
    gva <- x - ic
    gva[gva < 0] <- 0

    # Net value added
    nva <- x - ic - cfc
    nva[nva < 0] <- 0

    # --------------------------------------------------------------------
    # Computing Leontief inverse

    matrix_filepath <- file.path(
      "data_temp",
      paste0("figaro_inverse_leontief_", year_i, ".parquet")
    )

    if (!file.exists(matrix_filepath)) {
      # compute leontief inverse if not exist
      if (verbose) print(paste0("Computing Leontief inverse for year ", year_i))

      # A = (Z + K) x diag(x)^(-1), soit a_ij = (z_ij + k_ij) / x_j
      a <- sweep(z+k, 2, as.numeric(x), `/`)

      # a_ij = 0 si a_ij ∈ {NaN, Inf, -Inf}
      a[is.nan(a) | is.infinite(a)] <- 0

      # a_ii <- 0.995 si a_ii = 1
      diag(a)[diag(a) == 1] <- 0.995

      l <- solve(diag(nrow = nrow(a)) - a)
      rownames(l) <- rownames(a)
      colnames(l) <- colnames(a)

      matrix_l <- data.frame(
        id = rownames(l),
        as.data.frame(l, check.names = FALSE),
        check.names = FALSE
      )

      write_parquet(matrix_l, matrix_filepath)
    }

    l <- read_parquet(matrix_filepath) %>%
      column_to_rownames("id") %>%
      as.matrix()

    # M = L x diag(1 / diag(L))
    m <- sweep(l, 2, diag(l), `/`)

    # Z_X - Intrants IC directs par unité de production
    z_x <- sweep(z, 2, as.numeric(x), `/`)
    z_x[is.na(z_x) | is.infinite(z_x)] <- 0

    # K_X - Intrants CFC directs par unité de production
    k_x <- sweep(k, 2, as.numeric(x), `/`)
    k_x[is.na(k_x) | is.infinite(k_x)] <- 0

    # --------------------------------------------------------------------
    # Computing impacts vector

    metadata_indic <- metadata_indics %>%
      filter(indic == indic_i)

    e <- accounts_data %>%
      filter(year == year_i) %>%
      mutate(
        id = paste0(country, "_", industry)
      ) %>%
      select(id, value) %>%
      arrange(id) %>%
      column_to_rownames("id") %>%
      pull(value)

    c <- case_when(
      # -------------------------
      # contribution rates
      metadata_indic$type == "rate" ~ ifelse(x > 0 & nva > 0, (e / x) * 100, 0),
      # indexes
      metadata_indic$type == "index" ~ ifelse(x > 0, e * (nva / x), 0),
      # intensities
      metadata_indic$type == "intensity" ~ ifelse(x > 0, e / x, 0)
      # -------------------------
    )

    c[c %in% c(NaN, Inf, -Inf)] <- 0

    # --------------------------------------------------------------------
    # Computing footprints

    # -------------------------
    # prd footprint

    # P = N x diag(x)
    #   = diag(c) x M
    #   = diag(c) x L x diag(1 / diag(L)) x diag(x)

    fpt <- sweep(
      # N = diag(c) x M
      sweep(
        m,
        1,
        unlist(c), `*`
      ),
      2, x, `*`
    ) %>%
      colSums(na.rm = TRUE)

    prd_fpt <- case_when(
      x > 0 ~ fpt / x,
      TRUE  ~ 0
    )

    # --------------------------------------------------------------------
    # Macro-coherent (MC) footprints for derivated aggregates

    # -------------------------
    # ic contribution

    contribution_z_cal <- sweep(l %*% z_x, 2, diag(l), `/`)

    contribution_z_mc <- contribution_z_cal - diag(x = diag(contribution_z_cal))
    contribution_z_mc[is.na(contribution_z_mc) | is.infinite(contribution_z_mc)] <- 0

    indirect_impacts_ic_mc <- as.numeric(t(unlist(c)) %*% contribution_z_mc) * as.numeric(x)

    ic_mc_fpt <- case_when(
      ic > 0 ~ indirect_impacts_ic_mc / ic,
      TRUE   ~ 0
    )

    # -------------------------
    # cfc contribution

    contribution_k_cal <- sweep(l %*% k_x, 2, diag(l), `/`)

    contribution_k_mc <- contribution_k_cal - diag(x = diag(contribution_k_cal))
    contribution_k_mc[is.na(contribution_k_mc) | is.infinite(contribution_k_mc)] <- 0

    indirect_impacts_cfc_mc <- as.numeric(t(unlist(c)) %*% contribution_k_mc) * as.numeric(x)

    cfc_mc_fpt <- case_when(
      cfc > 0 ~ indirect_impacts_cfc_mc / cfc,
      TRUE   ~ 0
    )

    # -------------------------
    # nva/gva contribution

    direct_impacts_nva_mc <- as.numeric(c) * as.numeric(x)

    nva_mc_fpt  <- case_when(
      nva > 0 ~ direct_impacts_nva_mc / nva,
      TRUE    ~ 0
    )

    gva_mc_fpt  <- case_when(
      gva > 0 ~ (nva_mc_fpt * nva + cfc_mc_fpt * cfc) / gva,
      TRUE    ~ 0
    )

    # --------------------------------------------------------------------
    # Micro-coherent (UC) footprints for derivated aggregates

    # -------------------------
    # ic contribution

    contribution_z_uc <- t(unlist(c)) %*% sweep(l %*% z_x, 2, diag(l), `/`)
    contribution_z_uc[is.na(contribution_z_uc) | is.infinite(contribution_z_uc)] <- 0

    indirect_impacts_ic_uc <- as.numeric(contribution_z_uc) * as.numeric(x)

    ic_uc_fpt <- case_when(
      ic > 0 ~ indirect_impacts_ic_uc / ic,
      TRUE   ~ 0
    )

    # -------------------------
    # cfc contribution

    contribution_k_uc <- t(unlist(c)) %*% sweep(l %*% k_x, 2, diag(l), `/`)
    contribution_k_uc[is.na(contribution_k_uc) | is.infinite(contribution_k_uc)] <- 0

    indirect_impacts_cfc_uc <- as.numeric(contribution_k_uc) * as.numeric(x)

    cfc_uc_fpt <- case_when(
      cfc > 0 ~ indirect_impacts_cfc_uc / cfc,
      TRUE    ~ 0
    )

    # -------------------------
    # nva/gva contribution

    contribution_nva_uc <- c / diag(l)

    direct_impacts_nva_uc <- as.numeric(contribution_nva_uc) * as.numeric(x)

    nva_uc_fpt  <- case_when(
      nva > 0 ~ direct_impacts_nva_uc / nva,
      TRUE    ~ 0
    )

    gva_uc_fpt  <- case_when(
      gva > 0 ~ (nva_uc_fpt * nva + cfc_uc_fpt * cfc) / gva,
      TRUE    ~ 0
    )
    
    # --------------------------------------------------------------------
    # Binding

    macro_fpt_raw <- data.frame(
        serie_id = serie_id,
        indic    = indic_i,
        year     = year_i,
        id       = rownames(z),
        # Production footprints
        PRD      = as.numeric(prd_fpt),
        # Macro-coherent footprints
        IC_MC    = as.numeric(ic_mc_fpt),
        CFC_MC   = as.numeric(cfc_mc_fpt),
        NVA_MC   = as.numeric(nva_mc_fpt),
        GVA_MC   = as.numeric(gva_mc_fpt),
        # Unit-coherent footprints
        IC_UC    = as.numeric(ic_uc_fpt),
        CFC_UC   = as.numeric(cfc_uc_fpt),
        NVA_UC   = as.numeric(nva_uc_fpt),
        GVA_UC   = as.numeric(gva_uc_fpt)
      ) %>%
      pivot_longer(
        -c(serie_id, indic, year, id),
        names_to = "aggregate"
      ) %>%
      merge(metadata_indics) %>%
      mutate(
        value = case_when(
          value %in% c("NA", "NaN", "Inf") ~ 0,
          TRUE ~ round(value, digits = defaultprecision)
        ),
        country = sub("_.*$", "", id),
        industry = sub("^[^_]*_", "", id)
      ) %>%
      select(serie_id, indic, country, industry, year, aggregate, value)

    # --------------------------------------------------------------------
    # Footprint for all activities (TOTAL)

    macro_total_fpt <- macro_fpt_raw %>%
      mutate(
        aggregate_base = sub("_(MC|UC)$", "", aggregate)
      ) %>%
      merge(
        main_aggregates,
        by.x = c("year", "country", "industry", "aggregate_base"),
        by.y = c("year", "country", "industry", "aggregate")
      ) %>%
      group_by(serie_id, indic, country, year, aggregate) %>%
      reframe(
        total = ifelse(sum(amount) > 0, sum(value * amount) / sum(amount), 0),
        .groups = "drop"
      ) %>%
      mutate(
        value = total,
        industry = "TOTAL"
      ) %>%
      select(serie_id, indic, country, industry, year, aggregate, value)

    # --------------------------------------------------------------------
    # Formatting

    macro_fpt <- macro_fpt_raw %>%
      rbind(macro_total_fpt) %>%
      merge(metadata_indics) %>%
      mutate(
        serie_id   = serie_id,
        flag = case_when(
          # -------------------------
          # zéro “réel”
          value == 0 ~ "0",
          # > 0 mais arrondi à 0
          value > 0 & round(value, defaultprecision) == 0 ~ "0n",
          # autres cas
          TRUE ~ ""
          # -------------------------
        ),
        value = round(value, digits = defaultprecision),
        lastupdate = Sys.Date()
      ) %>%
      select(serie_id, indic, country, industry, year, aggregate, value, flag, lastupdate)

    # if (verbose) print(macro_fpt %>% as_tibble())
    if (verbose) print(macro_fpt %>% filter(country == "FR", aggregate == "PRD") %>% arrange(industry) %>% as_tibble())

    footprints_data <- rbind(footprints_data, macro_fpt)
  }

  return(footprints_data)
}
