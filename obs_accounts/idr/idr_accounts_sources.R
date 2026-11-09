# La Societe Nouvelle

# Sources partagees : les conversions et repartitions finales restent au builder.

# =============================================================================
# 1. ILOSTAT : parts de remuneration par decile
# =============================================================================

load_ilostat_idr_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  years <- tibble(year = as.character(years))

  # --- Lecture et validation du cache partage --------------------------------

  raw_data <- read_accounts_source_cache(
    file.path(download_dir, "LAP_2LID_QTL_RT_A.csv"),
    function() {
      get_ilostat("LAP_2LID_QTL_RT_A", quiet = TRUE)
    },
    c("classif1", "time", "ref_area", "obs_value"),
    use_temp_data,
    allow_download,
    read_cache = read.csv,
    numeric_columns = character()
  )

  # --- Preparation commune au producteur et au controle -----------------------

  ilostat_data <- raw_data %>%
    filter(
      classif1 %in% c("DCL_DECILE_01", "DCL_DECILE_02", "DCL_DECILE_09", "DCL_DECILE_10")
    ) %>%
    crossing(years) %>%
    mutate(
      time_num = as.integer(time),
      ecart = abs(time_num - as.integer(year))
    ) %>%
    group_by(year) %>%
    filter(ecart == min(ecart, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      year = time,
      country = countrycode(ref_area, "iso3c", "iso2c", nomatch = NULL),
      decile = classif1,
      value = obs_value
    ) %>%
    select(year, country, decile, value)

  ilostat_data
}

# =============================================================================
# 2. Insee BTS : remunerations et heures salariees en France
# =============================================================================

load_bts_idr_source <- function(
  years,
  use_temp_data = TRUE,
  verbose = FALSE,
  allow_download = TRUE
) {

  files <- read_delim(
    "obs_accounts/idr/bts_files_ids.csv",
    delim = ";",
    show_col_types = FALSE
  )

  metadata_trnneto <- read_delim(
    "obs_accounts/idr/metadata_trnneto.csv",
    delim = ";",
    show_col_types = FALSE
  )

  figaro_industries <- read_delim(
    "metadata/metadata_figaro_industries.csv",
    delim = ";",
    show_col_types = FALSE
  ) %>% filter(code != "TOTAL")

  # --- Charger chaque annee avec le meme lecteur et le meme schema ------------

  bts_raw_data <- map_dfr(as.character(years), function(
    year_i
  ) {
    reference <- files %>% filter(as.character(year) == year_i)
    if (nrow(reference) != 1L) stop("No unique BTS file reference for ", year_i)
    path <- file.path(download_dir, reference$file_name)

    data <- read_accounts_file_source(
      path,
      fetch = function(
        destination
      ) {
        archive <- tempfile(".bts-", tmpdir = dirname(path), fileext = ".zip")
        on.exit(unlink(archive), add = TRUE)
        curl_download(
          paste0("https://www.insee.fr/fr/statistiques/fichier/", reference$file_id, "/", reference$zip_name),
          archive,
          quiet = !verbose
        )
        members <- unzip(archive, list = TRUE)$Name
        member <- members[tolower(basename(members)) == tolower(reference$file_name)]
        if (length(member) != 1L) stop("Expected BTS file not found in archive")
        # Copier uniquement le fichier declare vers le cache temporaire.
        # La copie par blocs evite de charger tout le fichier BTS en memoire.
        connection <- unz(
          archive,
          member,
          open = "rb"
        )
        on.exit(close(connection), add = TRUE, after = FALSE)

        output <- file(destination, open = "wb")
        on.exit(close(output), add = TRUE, after = FALSE)

        repeat {
          buffer <- readBin(connection, what = "raw", n = 1024L * 1024L)
          if (length(buffer) == 0L) break
          writeBin(buffer, output)
        }
      },
      read_file = function(
        path
      ) read.csv(path, sep = ";"),
      columns = c("A38", "NBHEUR_TOT", "TRNNETO"),
      use_temp_data = use_temp_data,
      allow_download = allow_download
    )

    mutate(data, year = year_i)
  })

  # --- Preparation statistique conservee du producteur ------------------------

  bts_data <- bts_raw_data %>%
    merge(metadata_trnneto) %>%
    # Format
    mutate(
      country = "FR",
      branch = A38,
      working_hours = NBHEUR_TOT,
      net_pay = NETO # Milieu de la tranche de rémunération nette
    ) %>%
    filter(
      branch %in% figaro_industries$branch
    ) %>%
    select(year, country, branch, working_hours, net_pay)

  bts_data
}
