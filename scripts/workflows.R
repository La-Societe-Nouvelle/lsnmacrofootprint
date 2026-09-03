# La Société Nouvelle

# -----------------------------------------------------------------------------
# Mise à jour des séries historiques

update_obs_accounts <- function(
  indics            = default_indics,
  do_update         = FALSE,
  use_temp_data     = TRUE,
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
      use_temp_data = use_temp_data,
      verbose = verbose
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
  indics            = default_indics,
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
      verbose = FALSE
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
  indics            = default_tgt_indics,
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
  # 2- Build footprints for each serie

  for (serie_id in series)
  {
    # -------------------------
    # Build footprint

    macro_fpt_raw <<- build_footprints(serie_id, verbose)

    # -------------------------
    # Format data

    footprints_data <- macro_fpt_raw %>%
      mutate(
        flag       = ifelse(grepl("(_trd|_tgt)$", serie_id), 'f', ''), # flag 'f' for forecasted data
        lastupdate = Sys.Date(),
        indic      = str_sub(serie_id, 1, 3),
        serie      = str_sub(serie_id, 5, 7)
      ) %>%
      select(serie_id, country, industry, year, aggregate, value, flag, lastupdate, indic, serie)

    # -------------------------
    # Local/Environment storage

    if (do_update)
    {
      serie_type <- unique(footprints_data$serie)
      indic_i    <- unique(footprints_data$indic)

      footprints_data_filename <- paste0("footprints", "_", serie_type, "_", tolower(indic_i), ".csv")
      footprints_data_path     <- file.path(output_dir, footprints_data_filename)

      footprints_data <- footprints_data %>%
        select(-indic, -serie)

      write.csv(footprints_data, footprints_data_path, row.names = FALSE)
    }

    # -> Next serie
    # -------------------------
  }

  # -------------------------------------------------------------------
}

# -----------------------------------------------------------------------------
# Mise à jour des empreintes (désagrégation)

update_disaggregated_footprints <- function(
  year_i = 2022,
  do_update = FALSE,
  verbose   = TRUE
) {
  # --------------------------------------------------
  # Source scripts

  source("disaggregation/disaggregation.R")

  # EEIO Models
  source("disaggregation/eeio_canada/eeio_canada_footprints_builder.R")
  source("disaggregation/eeio_dk/eeio_dk_footprints_builder.R")
  source("disaggregation/eeio_uk/eeio_uk_footprints_builder.R")
  source("disaggregation/eeio_us/eeio_us_footprints_builder.R")

  # -------------------------------------------------------------------
  # 1- Build EEIO footprints

  build_canada_eeio_footprints(year_i, verbose = verbose)
  build_dk_eeio_footprints(year_i, verbose = verbose)
  build_uk_eeio_footprints(year_i, verbose = verbose)
  build_us_eeio_footprints(year_i, verbose = verbose)

  # -------------------------------------------------------------------
  # 2- Compile footprints data

  build_disaggregated_footprints(
    YEAR = year_i,
    use_temp_data = FALSE,
    do_update = TRUE
  )

  # -------------------------------------------------------------------
}
