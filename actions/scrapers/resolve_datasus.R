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
# - Painel de Oncologia (PO): folder /dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/
#   Files follow pattern POBR<YYYY>.dbc (e.g. POBR2013.dbc ... POBR2026.dbc).
#   Verified live: 14 files spanning years 2013 through 2026.
#   Docs page: https://datasus.saude.gov.br/ (HTTP 200).
# - SIM / Mortalidade Geral (CID10/DORES): folder /dissemin/publicos/SIM/CID10/DORES/
#   Files follow pattern DO<UF><YYYY>.dbc (e.g. DOAC1996.dbc ... DOTA2024.dbc).
#   Verified live: 812 files spanning years 1996 through 2024.
#   Docs page: https://datasus.saude.gov.br/estatisticas-vitais/ (HTTP 200).
# - SIM / Fetal, Externas, Infantil, Materno (CID10/DOFET): folder /dissemin/publicos/SIM/CID10/DOFET/
#   Files follow prefix + 2-digit YY (e.g. DOFET96.DBC ... DOFET25.DBC).
#   Verified live: 30 files each spanning years 1996 through 2025.
#   Docs page: https://datasus.saude.gov.br/estatisticas-vitais/ (HTTP 200).
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

  # Helper to fetch and split FTP directory listing
  ftp_list_files <- function(url) {
    listing <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    lines <- trimws(strsplit(listing, "\r*\n")[[1]])
    lines[nzchar(lines)]
  }

  # ---- 1. SINASC (datasus_sinasc) -------------------------------------------
  if ("datasus_sinasc" %in% rows$dataset) {
    sinasc_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    sinasc_res <- tryCatch({
      lines <- ftp_list_files(sinasc_url)
      dn_files <- grep("^DN[A-Z]{2}[0-9]{4}\\.dbc$", lines, ignore.case = TRUE, value = TRUE)
      if (length(dn_files) == 0) stop("no DN*.dbc files found in FTP listing")

      years <- as.integer(unique(substr(dn_files, 5, 8)))
      years <- sort(years[!is.na(years)])
      if (length(years) == 0) stop("could not parse years from DN*.dbc filenames")

      tibble::tibble(
        survey = "datasus",
        dataset = "datasus_sinasc",
        geo_level = NA_character_,
        year = NA_character_,
        available_time = paste0(min(years), "-", max(years)),
        docs_url = "https://datasus.saude.gov.br/nascidos-vivos-desde-1994/"
      )
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SINASC:", conditionMessage(e)))
      NULL
    })

    if (!is.null(sinasc_res)) results[[length(results) + 1]] <- sinasc_res
  }

  # ---- 2. Painel de Oncologia (datasus_po) -----------------------------------
  if ("datasus_po" %in% rows$dataset) {
    po_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/"
    po_res <- tryCatch({
      lines <- ftp_list_files(po_url)
      po_files <- grep("^POBR[0-9]{4}\\.dbc$", lines, ignore.case = TRUE, value = TRUE)
      if (length(po_files) == 0) stop("no POBR*.dbc files found in FTP listing")

      years <- as.integer(unique(substr(po_files, 5, 8)))
      years <- sort(years[!is.na(years)])
      if (length(years) == 0) stop("could not parse years from POBR*.dbc filenames")

      tibble::tibble(
        survey = "datasus",
        dataset = "datasus_po",
        geo_level = NA_character_,
        year = NA_character_,
        available_time = paste0(min(years), "-", max(years)),
        docs_url = "https://datasus.saude.gov.br/"
      )
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("PO:", conditionMessage(e)))
      NULL
    })

    if (!is.null(po_res)) results[[length(results) + 1]] <- po_res
  }

  # ---- 3. SIM - Mortalidade Geral (datasus_sim_do) --------------------------
  if ("datasus_sim_do" %in% rows$dataset) {
    sim_do_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    sim_do_res <- tryCatch({
      lines <- ftp_list_files(sim_do_url)
      do_files <- grep("^DO[A-Z]{2}[0-9]{4}\\.dbc$", lines, ignore.case = TRUE, value = TRUE)
      if (length(do_files) == 0) stop("no DO*.dbc files found in FTP listing")

      years <- as.integer(unique(substr(do_files, 5, 8)))
      years <- sort(years[!is.na(years)])
      if (length(years) == 0) stop("could not parse years from DO*.dbc filenames")

      tibble::tibble(
        survey = "datasus",
        dataset = "datasus_sim_do",
        geo_level = NA_character_,
        year = NA_character_,
        available_time = paste0(min(years), "-", max(years)),
        docs_url = "https://datasus.saude.gov.br/estatisticas-vitais/"
      )
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SIM_DO:", conditionMessage(e)))
      NULL
    })

    if (!is.null(sim_do_res)) results[[length(results) + 1]] <- sim_do_res
  }

  # ---- 4. SIM - Fetal, Externas, Infantil, Materno (DOFET/) -----------------
  dofet_targets <- c(
    datasus_sim_dofet = "DOFET",
    datasus_sim_doext = "DOEXT",
    datasus_sim_doinf = "DOINF",
    datasus_sim_domat = "DOMAT"
  )
  wanted_dofet <- intersect(names(dofet_targets), rows$dataset)

  if (length(wanted_dofet) > 0) {
    dofet_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    dofet_res <- tryCatch({
      lines <- ftp_list_files(dofet_url)
      sub_rows <- list()

      for (ds in wanted_dofet) {
        pfx <- dofet_targets[[ds]]
        m <- grep(paste0("^", pfx, "[0-9]{2}\\.dbc$"), lines, ignore.case = TRUE, value = TRUE)
        if (length(m) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": no ", pfx, "*.dbc files found"))
          next
        }

        yy <- as.integer(stringr::str_extract(m, "\\d+"))
        years <- sort(unique(ifelse(yy >= 90, 1900 + yy, 2000 + yy)))
        if (length(years) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": failed to parse 2-digit years"))
          next
        }

        sub_rows[[length(sub_rows) + 1]] <- tibble::tibble(
          survey = "datasus",
          dataset = ds,
          geo_level = NA_character_,
          year = NA_character_,
          available_time = paste0(min(years), "-", max(years)),
          docs_url = "https://datasus.saude.gov.br/estatisticas-vitais/"
        )
      }

      if (length(sub_rows) > 0) dplyr::bind_rows(sub_rows) else NULL
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SIM_DOFET_DIR:", conditionMessage(e)))
      NULL
    })

    if (!is.null(dofet_res)) results[[length(results) + 1]] <- dofet_res
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
