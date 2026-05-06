
# La Société Nouvelle

# build_footprints() ----------------------------------------------------------
# Generates footprint indicators (PRD, VA, IC, plus derived NVA and CFC)
# for the requested years and impact indicators by:
#   1. Load economic supply–use data, compute the Leontief inverse, and gather
#      direct impact vectors.
#   2. Convert these inputs into per‑branch footprints, then aggregate to
#      broader sector totals.
#   3. Optionally apply price adjustment, then return a consolidated data
#      frame of footprints by indicator, year, and aggregate.
# Then returning a consolidated data frame of footprints for each indicator, year, and aggregate.

build_footprints <- function(
  series,
  verbose = FALSE
) {
  if (verbose) message("Building footprints")
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
  # Iteration over each serie

  all_footprints_data <- c()

  for (serie_id in series)
  {
    if (verbose) message(paste0("Building footprints for serie ", serie_id))

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
      select(serie_id, year, country, industry, value, flag) %>%
      arrange(year, country, industry)

    years <- accounts_data %>%
      pull(year) %>%
      unique() %>%
      as.character()

    # --------------------------------------------------

    footprints_data <<- c()

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
      cfc[ic < 0] <- 0

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

        a <- sweep(z+k, 2, as.numeric(x), `/`)
        a[is.nan(a) | is.infinite(a)] <- 0
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

      # Indirects contributions
      contribution_z <- sweep(z,2,x,`/`) ; contribution_z[is.na(contribution_z)] <- 0 ; contribution_z <- contribution_z %*% l
      contribution_k <- sweep(k,2,x,`/`) ; contribution_k[is.na(contribution_k)] <- 0 ; contribution_k <- contribution_k %*% l

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
        metadata_indic$type == "rate" ~ ifelse(x > 0 & gva > 0, (e *(nva/gva) / x) * 100, 0),
        # indexes
        metadata_indic$type == "index" ~ ifelse(x > 0, e * (nva / x), 0),
        # intensities
        metadata_indic$type == "intensity" ~ ifelse(x > 0, e / x, 0)
        # -------------------------
      )

      c[c %in% c(NaN, Inf, -Inf)] <- 0

      # --------------------------------------------------------------------
      # Computing footprints

      # prd footprint
      fpt <- sweep(
        sweep(
          sweep(l, 2, diag(l), `/`),
          1,
          unlist(c), `*`
        ),
        2, x, `*`
      ) %>%
        colSums(na.rm = TRUE)

      # ic contribution
      indirect_impacts_ic <- sweep(
        sweep(
          sweep(contribution_z, 2, diag(l), `/`),
          1, unlist(c), `*`
        ),
        2, x, `*`
      ) %>%
        colSums(na.rm = TRUE)

      # cfc contribution
      indirect_impacts_cfc <- sweep(
        sweep(
          sweep(contribution_k, 2, diag(l), `/`),
          1, unlist(c), `*`
        ),
        2, x, `*`
      ) %>%
        colSums(na.rm = TRUE)

      # --------------------------------------------------------------------
      # Building footprints for derivated aggregates

      prd_fpt <- case_when(
        x > 0 ~ fpt / x,
        TRUE  ~ 0
      )

      ic_fpt <- case_when(
        ic > 0 ~ indirect_impacts_ic / ic,
        TRUE   ~ 0
      )

      cfc_fpt <- case_when(
        cfc > 0 ~ indirect_impacts_cfc / cfc,
        TRUE   ~ 0
      )

      nva_fpt  <- case_when(
        nva > 0 ~ unlist(c * x / nva),
        TRUE   ~ 0
      )

      gva_fpt  <- case_when(
        gva > 0 ~ (nva_fpt * nva + cfc_fpt * cfc) / gva,
        TRUE   ~ 0
      )

      macro_fpt_raw <- data.frame(
          serie_id = serie_id,
          indic    = indic_i,
          year     = year_i,
          id       = rownames(z),
          PRD      = as.numeric(prd_fpt),
          IC       = as.numeric(ic_fpt),
          CFC      = as.numeric(cfc_fpt),
          NVA      = as.numeric(nva_fpt),
          GVA      = as.numeric(gva_fpt)
        ) %>%
        pivot_longer(5:9, names_to = "aggregate") %>%
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
        merge(main_aggregates) %>% # by year, country, industry, aggregate
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

      footprints_data <<- rbind(footprints_data, macro_fpt)
    }

    # --------------------------------------------------------------------
    # Saving data

    footprints_data_filename <- paste0("footprints", "_", serie_type, "_", tolower(indic_i), ".csv")
    footprints_data_path  <- file.path(output_dir, footprints_data_filename)

    write.csv(footprints_data, footprints_data_path, row.names = FALSE)

    all_footprints_data <- rbind(all_footprints_data, footprints_data)
  }

  return(all_footprints_data)
}
