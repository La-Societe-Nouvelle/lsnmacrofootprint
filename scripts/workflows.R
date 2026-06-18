# La Société Nouvelle

# -----------------------------------------------------------------------------
# Mise à jour des séries historiques

update_obs_accounts <- function(
  indics,
  do_clean_outliers = TRUE,
  do_update         = FALSE,
  verbose           = FALSE
) {
  # --------------------------------------------------
  # Loop through each requested indicator

  for (indic_i in indics) {

    if (verbose) print(paste0("Processing indicator: ", indic_i))

    # -------------------------
    # Source script

    path <- file.path(
      "obs_accounts",
      tolower(indic_i),
      paste0(tolower(indic_i), "_accounts_builder.R")
    )
    source(path)

    function_name <- paste0("build_", tolower(indic_i), "_obs_accounts")
    obs_accounts_builder <- get(function_name)

    # -------------------------
    # Build accounts data

    accounts_data <<- obs_accounts_builder(
      do_clean_outliers = TRUE,
      use_temp_data = FALSE,
      verbose = TRUE
    )

    # -------------------------
    # Local/Environment storage

    if (do_update)
    {
      accounts_data_path  <- file.path(output_dir, paste0("accounts_obs_", tolower(indic_i), ".csv"))
      write.csv(accounts_data, accounts_data_path, row.names = FALSE)
    }

    # -> Next indic
    # -------------------------
  }
}

# -----------------------------------------------------------------------------
# Mise à jour des séries tendancielles

update_trd_accounts <- function(
  indics,
  do_update         = FALSE,
  verbose           = FALSE
) {
  # --------------------------------------------------
  # Source script

  source("trd_accounts/trend_accounts_builder.R")

  # --------------------------------------------------
  # Loop through each requested indicator

  for (indic_i in indics)
  {
    if (verbose) print(paste0("Processing indicator: ", indic_i))

    # -------------------------
    # Build trend accounts data

    accounts_data <<- build_trd_accounts(
      indic_i,
      verbose = verbose
    )

    # -------------------------
    # Local/Environment storage

    if (do_update)
    {
      accounts_data_path  <- file.path(output_dir, paste0("accounts_trd_", tolower(indic_i), ".csv"))
      write.csv(accounts_data, accounts_data_path, row.names = FALSE)
    }

    # -> Next indic
    # -------------------------
  }
  # --------------------------------------------------
}


# -----------------------------------------------------------------------------
# Mise à jour des séries cibles

update_tgt_accounts <- function(
  indics,
  do_update         = FALSE,
  verbose           = TRUE
) {
  # --------------------------------------------------
  # Loop through each requested indicator

  for (indic_i in indics)
  {
    if (verbose) print(paste0("Processing indicator: ", indic_i))

    # -------------------------
    # Source script

    path <- file.path(
      "tgt_accounts",
      tolower(indic_i),
      paste0(tolower(indic_i), "_targets_builder", ".R")
    )
    source(path)

    function_name <- paste0("build_", tolower(indic_i), "_tgt_accounts")
    tgt_accounts_builder <- get(function_name)

    # -------------------------
    # Build accounts data

    accounts_data <<- tgt_accounts_builder(
      verbose = verbose
    )

    # -------------------------
    # Local/Environment storage

    if (do_update)
    {
      accounts_data_path  <- file.path(output_dir, paste0("accounts_tgt_", tolower(indic_i), ".csv"))
      write.csv(accounts_data, accounts_data_path, row.names = FALSE)
    }

    # -> Next indic
    # -------------------------
  }
}

# -----------------------------------------------------------------------------
# Mise à jour des empreintes

update_footprints <- function(
  indics = default_indics,
  do_update = FALSE,
  verbose   = TRUE
) {
  # -------------------------------------------------------------------
  # 1- Build serie ids to update

  tgt_indics_to_update <- intersect(tolower(indics), tolower(default_tgt_indics))
  tgt_indics_to_update <- tgt_indics_to_update[nzchar(tgt_indics_to_update)]

  series_obs <- paste0(tolower(indics), "_obs")
  series_trd <- paste0(tolower(indics), "_trd")
  series_tgt <- if (length(tgt_indics_to_update) > 0) {
    paste0(tgt_indics_to_update, "_tgt")
  } else {
    character(0)
  }
  series     <- c(series_obs, series_trd, series_tgt)

  # -------------------------------------------------------------------
  # 2- Build footprints

  macro_fpt_raw <<- build_footprints(series, verbose)

  # -------------------------------------------------------------------
  # 3- Format data

  footprints_data <- macro_fpt_raw %>%
    mutate(
      flag       = ifelse(grepl("(_trd|_tgt)$", serie_id), 'f', ''), # flag 'f' for forecasted data
      lastupdate = Sys.Date(),
      indic      = str_sub(serie_id, 1, 3),
      serie      = str_sub(serie_id, 5, 7)
    ) %>%
    select(serie_id, country, industry, year, aggregate, value, flag, lastupdate, indic, serie)

  # -------------------------------------------------------------------
  # 3- Detect outliers & replace

  # Outliers handled in accounts builders

  # -------------------------------------------------------------------
  # 4- Storage

  if (do_update)
  {
    footprint_groups <- footprints_data %>%
      group_by(serie, indic) %>%
      group_split()

    for (footprints_data_i in footprint_groups)
    {
      serie_type <- unique(footprints_data_i$serie)
      indic_i    <- unique(footprints_data_i$indic)

      footprints_data_filename <- paste0("footprints", "_", serie_type, "_", tolower(indic_i), ".csv")
      footprints_data_path     <- file.path(output_dir, footprints_data_filename)

      footprints_data_i <- footprints_data_i %>%
        select(-indic, -serie)

      write.csv(footprints_data_i, footprints_data_path, row.names = FALSE)
    }
  }

  # -------------------------------------------------------------------
}
