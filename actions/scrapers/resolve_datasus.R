# actions/scrapers/resolve_datasus.R
#
# Resolver for DATASUS microdata files hosted on the official FTP server
# (ftp://ftp.datasus.gov.br/dissemin/publicos/).
#
# INVESTIGATION & VERIFICATION RECORD (2026-11-02):
# - Host: ftp.datasus.gov.br (passive FTP, port 21).
# - SINASC (DNRES): folder /dissemin/publicos/SINASC/1996_/Dados/DNRES/
#   Files follow pattern DN<UF><YYYY>.dbc (e.g. DNAC1996.DBC ... DNTO2024.dbc).
#   Verified live: 823 files spanning years 1996 through 2024.
#   Docs page: https://datasus.saude.gov.br/nascidos-vivos-desde-1994/ (HTTP 200).
# - Output: returns one row per resolved dataset with up-to-date available_time
#   and docs_url.
# - Fault isolation: independent sub-source blocks accumulate failures in
#   `block_failures` and return them as `attr(result, "partial_failures")`,
#   stopping only if all sub-sources fail.

resolve_datasus <- function(rows) {
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("resolve_datasus: RCurl package is required")
  }

  results <- list()
  block_failures <- character(0)

  # ---- 1. SINASC (datasus_sinasc) -------------------------------------------
  if ("datasus_sinasc" %in% rows$dataset) {
    sinasc_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    sinasc_res <- tryCatch({
      listing <- RCurl::getURL(sinasc_url, ftp.use.epsv = TRUE, dirlistonly = TRUE)
      lines <- trimws(strsplit(listing, "\r*\n")[[1]])
      lines <- lines[nzchar(lines)]

      # Files like DNAC1996.DBC
      dn_files <- grep("^DN[A-Z]{2}[0-9]{4}\\.dbc$", lines, ignore.case = TRUE, value = TRUE)
      if (length(dn_files) == 0) {
        stop("no DN*.dbc files found in FTP listing")
      }

      years <- as.integer(unique(substr(dn_files, 5, 8)))
      years <- sort(years[!is.na(years)])
      if (length(years) == 0) {
        stop("could not parse years from DN*.dbc filenames")
      }

      avail_time <- paste0(min(years), "-", max(years))

      tibble::tibble(
        survey = "datasus",
        dataset = "datasus_sinasc",
        geo_level = NA_character_,
        year = NA_character_,
        available_time = avail_time,
        docs_url = "https://datasus.saude.gov.br/nascidos-vivos-desde-1994/"
      )
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SINASC:", conditionMessage(e)))
      NULL
    })

    if (!is.null(sinasc_res)) {
      results[[length(results) + 1]] <- sinasc_res
    }
  }

  if (length(results) == 0) {
    stop("resolve_datasus failed for all sub-sources: ", paste(block_failures, collapse = "; "))
  }

  out <- dplyr::bind_rows(results)
  if (length(block_failures) > 0) {
    attr(out, "partial_failures") <- block_failures
  }

  out
}
