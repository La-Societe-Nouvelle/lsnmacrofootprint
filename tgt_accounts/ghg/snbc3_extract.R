# La Société Nouvelle

#' SNBC 3 detailed GHG trajectories to snbc3_detail.csv (MtCO2e), run manually:
#'   Rscript tgt_accounts/ghg/snbc3_extract.R <SNBC-Tableur.xlsx>
#' Source: https://www.ecologie.gouv.fr/sites/default/files/documents/SNBC-Tableur-Organisations-Juillet2026_2.xlsx

extract_snbc3_detail <- function(
  xlsx_path,
  sheet = "Résultats détaillés GES"
) {
  raw <- readxl::read_xlsx(xlsx_path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  labels <- trimws(as.character(raw[[1]]))
  header_rows <- which(grepl("^Emissions de CO2e", labels))

  blocks <- lapply(header_rows, function(h) {
    years <- suppressWarnings(as.integer(unlist(raw[h, -1])))
    # Block title: closest non-empty label above the year header.
    title <- tail(labels[seq_len(h - 1)][!is.na(labels[seq_len(h - 1)]) & labels[seq_len(h - 1)] != ""], 1)
    end <- c(header_rows[header_rows > h], nrow(raw) + 1)[1] - 1
    rows <- (h + 1):end
    rows <- rows[!is.na(labels[rows]) & labels[rows] != ""]

    do.call(rbind, lapply(rows, function(r) {
      values <- suppressWarnings(as.numeric(unlist(raw[r, -1])))
      keep <- !is.na(years) & !is.na(values)
      if (!any(keep)) return(NULL)
      data.frame(block = title, line = labels[r], year = years[keep], value = values[keep])
    }))
  })

  detail <- do.call(rbind, blocks)

  # Duplicated lines within a block (repeated totals) keep their first occurrence.
  detail[!duplicated(detail[c("block", "line", "year")]), ]
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  detail <- extract_snbc3_detail(args[1])
  write.table(
    detail,
    "tgt_accounts/ghg/snbc3_detail.csv",
    sep = ";",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  message("snbc3_detail.csv: ", nrow(detail), " rows, ", length(unique(detail$block)), " blocks")
}
