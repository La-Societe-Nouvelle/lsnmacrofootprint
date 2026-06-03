# La Societe Nouvelle

# -------------------------------------------------------------------
# Helpers

read_output_files <- function(
  output_dir = "data_output",
  pattern,
  verbose = FALSE
) {
  files <- list.files(
    path = output_dir,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No file matching pattern '", pattern, "' found in ", output_dir, ".")
  }

  if (verbose) {
    message("Files to upload:")
    print(basename(files))
  }

  data <- purrr::map_dfr(
    files,
    readr::read_csv,
    col_types = readr::cols(
      flag = readr::col_character()
    ),
    show_col_types = FALSE
  )

  return(data)
}

upload_table_data <- function(
  data,
  schema,
  table,
  verbose = FALSE
) {
  if (!"serie_id" %in% names(data)) {
    stop("Column 'serie_id' is required to delete existing data before upload.")
  }

  conn <- get_connection_db()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  serie_ids <- unique(data$serie_id)
  serie_ids <- serie_ids[!is.na(serie_ids)]

  if (length(serie_ids) == 0) {
    stop("No valid serie_id found in data.")
  }

  if (verbose) {
    message(
      "Deleting existing rows from ",
      schema, ".", table,
      " for ", length(serie_ids), " serie_id(s)..."
    )
  }

  DBI::dbExecute(
    conn,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(conn, schema),
      ".",
      DBI::dbQuoteIdentifier(conn, table),
      " WHERE serie_id IN (",
      paste(DBI::dbQuoteString(conn, serie_ids), collapse = ", "),
      ")"
    )
  )

  if (verbose) {
    message("Uploading ", nrow(data), " rows into ", schema, ".", table, "...")
  }

  DBI::dbWriteTable(
    conn = conn,
    name = DBI::Id(schema = schema, table = table),
    value = data,
    append = TRUE
  )

  if (verbose) {
    message("Upload complete.")
    message("Uploaded series:")
    for (serie_id in serie_ids) {
      message("- ", serie_id)
    }
  }

  invisible(data)
}

# -------------------------------------------------------------------
# Accounts data

upload_accounts_data <- function(
  output_dir = "data_output",
  verbose = FALSE
) {
  if (verbose) message("read_output_files")
  accounts_data <- read_output_files(
    output_dir = output_dir,
    pattern = "^accounts_.*\\.csv$",
    verbose = verbose
  )

  if (verbose) message("upload_table_data")
  upload_table_data(
    data = accounts_data,
    schema = "impacts",
    table = "direct_impacts",
    verbose = verbose
  )
}

# -------------------------------------------------------------------
# Footprints data

upload_footprints_data <- function(
  output_dir = "data_output",
  series = NULL,
  verbose = FALSE
) {
  footprints_data <- read_output_files(
    output_dir = output_dir,
    pattern = "^footprints_.*\\.csv$",
    verbose = verbose
  )

  footprints_data <- footprints_data %>%
    mutate(
      currency = "CPEUR"
    ) %>%
    select(serie_id, country, industry, aggregate, year, currency, value, flag, lastupdate)

  if (!is.null(series)) {
    footprints_data <- footprints_data %>%
      filter(serie_id %in% series)

    if (nrow(footprints_data) == 0) {
      stop("No footprint data found for selected series.")
    }
  }

  upload_table_data(
    data = footprints_data,
    schema = "macrodata",
    table = "macro_fpt",
    verbose = verbose
  )
}

# -------------------------------------------------------------------
# FIGARO data

upload_figaro_data <- function(
  years = 2010:2030,
  data_dir = "data_figaro",
  schema = "models",
  table = "figaro_main_aggregates_extended",
  verbose = FALSE
) {
  # if (missing(years) || length(years) == 0) {
  #   stop("Argument 'years' is required.")
  # }

  years <- as.character(years)

  files <- file.path(data_dir, paste0("figaro_main_aggregates_", years, ".parquet"))
  missing_files <- files[!file.exists(files)]

  if (length(missing_files) > 0) {
    stop(
      "Missing FIGARO main aggregate file(s): ",
      paste(missing_files, collapse = ", ")
    )
  }

  if (verbose) {
    message("Files to upload:")
    print(basename(files))
  }

  figaro_data <- purrr::map_dfr(
    files,
    function(file) {
      arrow::read_parquet(file) %>%
        mutate(year = as.character(year))
    }
  )

  required_columns <- c("industry", "country", "aggregate", "year", "value")
  missing_columns <- setdiff(required_columns, names(figaro_data))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in FIGARO data: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!"flag" %in% names(figaro_data)) {
    figaro_data$flag <- NA_character_
  }

  figaro_data <- figaro_data %>%
    mutate(
      industry = as.character(industry),
      country = as.character(country),
      aggregate = as.character(aggregate),
      year = as.character(year),
      value = format(
        round(as.numeric(value), digits = 3),
        nsmall = 3,
        scientific = FALSE,
        trim = TRUE
      ),
      flag = as.character(flag),
      lastupdate = Sys.Date()
    ) %>%
    select(industry, country, aggregate, year, value, flag, lastupdate)

  duplicated_keys <- figaro_data %>%
    count(industry, country, aggregate, year, name = "n") %>%
    filter(n > 1)

  if (nrow(duplicated_keys) > 0) {
    stop(
      "Duplicate primary keys found in FIGARO data before upload. ",
      "First duplicated key: ",
      paste(unlist(duplicated_keys[1, c("industry", "country", "aggregate", "year")]), collapse = " / ")
    )
  }

  conn <- get_connection_db()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (verbose) {
    message(
      "Deleting existing rows from ",
      schema, ".", table,
      " for year(s): ",
      paste(years, collapse = ", ")
    )
  }

  DBI::dbExecute(
    conn,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(conn, schema),
      ".",
      DBI::dbQuoteIdentifier(conn, table),
      " WHERE year IN (",
      paste(DBI::dbQuoteString(conn, years), collapse = ", "),
      ")"
    )
  )

  if (verbose) {
    message("Uploading ", nrow(figaro_data), " rows into ", schema, ".", table, "...")
  }

  DBI::dbWriteTable(
    conn = conn,
    name = DBI::Id(schema = schema, table = table),
    value = figaro_data,
    append = TRUE
  )

  if (verbose) {
    message("Upload complete.")
  }

  invisible(figaro_data)
}
