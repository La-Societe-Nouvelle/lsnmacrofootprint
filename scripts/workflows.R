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

    accounts_data <<- obs_accounts_builder()

    # -------------------------
    # Local/Environment storage

    # data already saved in obs_accounts_builder()
    # accounts_data_path  <- file.path(output_dir, "accounts_obs_", tolower(indic_i),".csv")
    # write.csv(accounts_data, accounts_data_path, row.names = FALSE)

    # -------------------------
    # Update database

    if (do_update)
    {
      if (verbose) print(paste0("Updating database for: ", indic_i))

      # db
      source("db/update_data.R")

      # Update data
      serie <- paste0(tolower(indic_i), "_obs")
      update_direct_impacts_data(
        serie,
        accounts_data
      )
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

    trend_accounts_data <<- build_trd_accounts(indic_i)

    # -------------------------
    # Local/Environment storage

    # ...

    # -------------------------
    # Update database

    if (do_update)
    {
      if (verbose) print(paste0("Updating database for: ", indic_i))

      # db
      source("db/update_data.R")

      # Update data
      serie = paste0(tolower(indic_i), "_trd")
      update_direct_impacts_data(
        serie,
        trend_accounts_data
      )
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
      paste0("build_target_", tolower(indic_i), ".R")
    )
    source(path)

    function_name <- paste0("build_target_",tolower(indic_i), "")
    tgt_accounts_builder <- get(function_name)

    # -------------------------
    # Build accounts data

    accounts_data <<- tgt_accounts_builder()

    # -------------------------
    # Local/Environment storage

    # ...

    # -------------------------
    # Update database

    if (do_update)
    {
      if (verbose) print(paste0("Updating database for: ", indic_i))

      # db
      source("db/update_data.R")

      # Update data
      serie = paste0(tolower(indic_i), "_tgt")
      update_direct_impacts_data(
        serie,
        accounts_data
      )
    }

    # -> Next indic
    # -------------------------
  }
}

# -----------------------------------------------------------------------------
# Mise à jour des empreintes

update_check_fpt <- function(
  indics,
  tgt_indics = indics,
  do_update = FALSE,
  verbose   = TRUE
) {
  # -------------------------------------------------------------------
  # 1- Build serie ids to update

  series_obs <- paste0(tolower(indics), "_obs")
  series_trd <- paste0(tolower(indics), "_trd")
  series_tgt <- paste0(tolower(tgt_indics), "_tgt")
  series     <- c(series_obs, series_trd, series_tgt)

  # -------------------------------------------------------------------
  # 2- Build footprints

  macro_fpt_raw <<- build_footprints(series, verbose)

  # -------------------------------------------------------------------
  # 3- Format data

  footprint_data <- macro_fpt_raw %>%
    mutate(
      flag       = ifelse(grepl("(_trd|_tgt)$", serie_id), 'f', ''), # flag 'f' for forecasted data
      currency   = 'CPEUR',
      lastupdate = Sys.Date(),
      indic      = str_sub(serie_id, 1,3),
      serie      = str_sub(serie_id, 5,7),
      year       = as.numeric(year)
    )

  # -------------------------------------------------------------------
  # 3- Detect outliers & replace

  footprint_obs_trd_check <- map_dfr(indics, function(x)
  {
    df <- footprint_data %>%
      filter(serie %in% c("obs", "trd")) %>%
      filter(indic == tolower(x))

    if (x != "ECO") {
      df <- clean_outliers(
        df,
        serie_pkey = c("country","industry","aggregate")
      )
    }

    df
  })

  footprint_obs_tgt_check <- map_dfr(unique(footprint_data$indic), function(x)
  {
    df <- footprint_data %>%
      filter(serie %in% c("obs", "tgt")) %>%
      filter(indic == x)

    df <- clean_outliers(
      df,
      serie_pkey = c("country","industry","aggregate")
    )

    df
  })

  # -------------------------------------------------------------------
  # 4- Update in database

  footprint_SQL <- rbind(footprint_data          %>% filter(serie == "obs"),
                         footprint_obs_trd_check %>% filter(serie == "trd"),
                         footprint_obs_tgt_check %>% filter(serie == "tgt")) %>%
    mutate(country = countrycode(country,'iso2c','iso3c', nomatch = NULL)) %>%
    select(serie_id, year, country, industry, aggregate, value, flag, lastupdate, currency)

  if (do_update)
  {
    if (verbose) print("Push data")

    dbExecute(conn,
              paste0("DELETE FROM macrodata.macro_fpt WHERE serie_id IN (",
                          paste0("'", series, "'", collapse = ", "),");"))

    dbWriteTable(conn,
                 SQL("macrodata.macro_fpt"),
                 footprint_SQL,
                 append = T)

    if (verbose) print("Data updated")

  }
  # else {
  #   return(list(footprint_data = footprint_data,
  #                   footprint_SQL  = footprint_SQL))
  # }

  # -------------------------------------------------------------------
}
