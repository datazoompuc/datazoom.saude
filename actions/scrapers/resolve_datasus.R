# actions/scrapers/resolve_datasus.R
#
# Unified resolver for all DATASUS microdata files hosted on the official FTP server
# (ftp://ftp.datasus.gov.br/dissemin/publicos/).
#
# INVESTIGATION & VERIFICATION RECORD (2026-11-02):
# - Host: ftp.datasus.gov.br (passive FTP, port 21).
# - SINASC (DNRES): folder /dissemin/publicos/SINASC/1996_/Dados/DNRES/
#   Files: DN<UF><YYYY>.dbc (1996-2024).
#   Docs page: https://datasus.saude.gov.br/nascidos-vivos-desde-1994/ (HTTP 200).
# - Painel de Oncologia (PO): folder /dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/
#   Files: POBR<YYYY>.dbc (2013-2026).
#   Docs page: https://datasus.saude.gov.br/ (HTTP 200).
# - SIM / Mortalidade Geral (CID10/DORES): folder /dissemin/publicos/SIM/CID10/DORES/
#   Files: DO<UF><YYYY>.dbc (1996-2024).
#   Docs page: https://datasus.saude.gov.br/estatisticas-vitais/ (HTTP 200).
# - SIM / Fetal, Externas, Infantil, Materno (CID10/DOFET): folder /dissemin/publicos/SIM/CID10/DOFET/
#   Files: DOFET<YY>.dbc, DOEXT<YY>.dbc, DOINF<YY>.dbc, DOMAT<YY>.dbc (1996-2025).
#   Docs page: https://datasus.saude.gov.br/estatisticas-vitais/ (HTTP 200).
# - CNES (13 bases): folders /dissemin/publicos/CNES/200508_/Dados/<PREFIX>/
#   Files: <PREFIX><UF><YY><MM>.dbc.
#   Docs page: https://datasus.saude.gov.br/cnes/ (HTTP 200).
# - SIHSUS (4 bases): folder /dissemin/publicos/SIHSUS/200801_/Dados/
#   Files: <PREFIX><UF><YY><MM>.dbc (RD, RJ, SP: 2008-2026; ER: 2011-2026).
#   Docs page: https://datasus.saude.gov.br/acesso-a-informacao/producao-hospitalar-sih-sus/ (HTTP 200).
# - SIASUS (12 bases): folder /dissemin/publicos/SIASUS/200801_/Dados/
#   Files: <PREFIX><UF><YY><MM>.dbc (2008-2026, varies by base).
#   Docs page: https://datasus.saude.gov.br/acesso-a-informacao/producao-ambulatorial-sia-sus/ (HTTP 200).

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

  # ---- 5. CNES (13 bases) ---------------------------------------------------
  cnes_targets <- c(
    datasus_cnes_lt = "LT", datasus_cnes_st = "ST", datasus_cnes_dc = "DC",
    datasus_cnes_eq = "EQ", datasus_cnes_sr = "SR", datasus_cnes_hb = "HB",
    datasus_cnes_pf = "PF", datasus_cnes_ep = "EP", datasus_cnes_rc = "RC",
    datasus_cnes_in = "IN", datasus_cnes_ee = "EE", datasus_cnes_ef = "EF",
    datasus_cnes_gm = "GM"
  )
  wanted_cnes <- intersect(names(cnes_targets), rows$dataset)

  if (length(wanted_cnes) > 0) {
    for (ds in wanted_cnes) {
      pfx <- cnes_targets[[ds]]
      u <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/", pfx, "/")
      cnes_res <- tryCatch({
        lines <- ftp_list_files(u)
        m <- grep(paste0("^", pfx, "[A-Z]{2}[0-9]{4}\\.dbc$"), lines, ignore.case = TRUE, value = TRUE)
        if (length(m) == 0) stop("no files matching prefix ", pfx)

        yy <- as.integer(substr(m, 5, 6))
        years <- sort(unique(ifelse(yy >= 90, 1900 + yy, 2000 + yy)))
        if (length(years) == 0) stop("could not parse years for ", pfx)

        tibble::tibble(
          survey = "datasus",
          dataset = ds,
          geo_level = NA_character_,
          year = NA_character_,
          available_time = paste0(min(years), "-", max(years)),
          docs_url = "https://datasus.saude.gov.br/cnes/"
        )
      }, error = function(e) {
        block_failures <<- c(block_failures, paste0("CNES_", pfx, ": ", conditionMessage(e)))
        NULL
      })

      if (!is.null(cnes_res)) results[[length(results) + 1]] <- cnes_res
    }
  }

  # ---- 6. SIHSUS (4 bases) --------------------------------------------------
  sih_targets <- c(
    datasus_sih_rd = "RD", datasus_sih_rj = "RJ", datasus_sih_sp = "SP", datasus_sih_er = "ER"
  )
  wanted_sih <- intersect(names(sih_targets), rows$dataset)

  if (length(wanted_sih) > 0) {
    sih_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    sih_res <- tryCatch({
      lines <- ftp_list_files(sih_url)
      sub_rows <- list()

      for (ds in wanted_sih) {
        pfx <- sih_targets[[ds]]
        m <- grep(paste0("^", pfx, "[A-Z]{2}[0-9]{4}\\.dbc$"), lines, ignore.case = TRUE, value = TRUE)
        if (length(m) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": no ", pfx, "*.dbc files found"))
          next
        }

        yy <- as.integer(substr(m, 5, 6))
        years <- sort(unique(ifelse(yy >= 90, 1900 + yy, 2000 + yy)))
        if (length(years) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": could not parse years"))
          next
        }

        sub_rows[[length(sub_rows) + 1]] <- tibble::tibble(
          survey = "datasus_sih",
          dataset = ds,
          geo_level = NA_character_,
          year = NA_character_,
          available_time = paste0(min(years), "-", max(years)),
          docs_url = "https://datasus.saude.gov.br/acesso-a-informacao/producao-hospitalar-sih-sus/"
        )
      }

      if (length(sub_rows) > 0) dplyr::bind_rows(sub_rows) else NULL
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SIHSUS_DIR:", conditionMessage(e)))
      NULL
    })

    if (!is.null(sih_res)) results[[length(results) + 1]] <- sih_res
  }

  # ---- 7. SIASUS (12 bases) -------------------------------------------------
  siasus_targets <- c(
    datasus_siasus_ab = "AB", datasus_siasus_ad = "AD", datasus_siasus_am = "AM",
    datasus_siasus_an = "AN", datasus_siasus_aq = "AQ", datasus_siasus_ar = "AR",
    datasus_siasus_pa = "PA", datasus_siasus_ps = "PS", datasus_siasus_abo = "ABO",
    datasus_siasus_acf = "ACF", datasus_siasus_atd = "ATD", datasus_siasus_sad = "SAD"
  )
  wanted_siasus <- intersect(names(siasus_targets), rows$dataset)

  if (length(wanted_siasus) > 0) {
    siasus_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    siasus_res <- tryCatch({
      lines <- ftp_list_files(siasus_url)
      sub_rows <- list()

      for (ds in wanted_siasus) {
        pfx <- siasus_targets[[ds]]
        m <- grep(paste0("^", pfx, "[A-Z]{2}[0-9]{4}\\.dbc$"), lines, ignore.case = TRUE, value = TRUE)
        if (length(m) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": no ", pfx, "*.dbc files found"))
          next
        }

        p_len <- nchar(pfx)
        yy_start <- p_len + 3
        yy <- as.integer(substr(m, yy_start, yy_start + 1))
        years <- sort(unique(ifelse(yy >= 90, 1900 + yy, 2000 + yy)))
        if (length(years) == 0) {
          block_failures <<- c(block_failures, paste0(ds, ": could not parse years"))
          next
        }

        sub_rows[[length(sub_rows) + 1]] <- tibble::tibble(
          survey = "datasus_siasus",
          dataset = ds,
          geo_level = NA_character_,
          year = NA_character_,
          available_time = paste0(min(years), "-", max(years)),
          docs_url = "https://datasus.saude.gov.br/acesso-a-informacao/producao-ambulatorial-sia-sus/"
        )
      }

      if (length(sub_rows) > 0) dplyr::bind_rows(sub_rows) else NULL
    }, error = function(e) {
      block_failures <<- c(block_failures, paste("SIASUS_DIR:", conditionMessage(e)))
      NULL
    })

    if (!is.null(siasus_res)) results[[length(results) + 1]] <- siasus_res
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
