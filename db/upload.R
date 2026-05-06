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
    table = "directs_impacts",
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
