# R/sipni.R
#
# SI-PNI (Sistema de Informacao do Programa Nacional de Imunizacoes) Web Scraper
#
# Downloads consolidated municipal vaccination data from DATASUS SI-PNI Web portal:
# https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf
#
# Architecture:
# - Connects directly via plain HTTP/1.1 requests using 'curl'
# - Maintains JSF/PrimeFaces session state (javax.faces.ViewState and JSF IDs)
# - Performs sequential AJAX partial requests: UF -> IBGE totals -> Strategy -> Product -> Doses -> Search -> Export CSV
# - Parses returned ISO-8859-1 / UTF-8 CSV or binary Excel into harmonized tibbles

#' @keywords internal
parse_pni_table <- function(dat, state, year, strategy, product, dose_labels) {
  if (is.null(dat) || nrow(dat) == 0) {
    return(tibble::tibble(
      state = character(0), year = integer(0), munic_code = numeric(0),
      munic_name = character(0), strategy = character(0), product = character(0),
      month = character(0), dose = character(0), quantity = numeric(0)
    ))
  }

  df <- dat %>%
    dplyr::rename(municipio = 1) %>%
    dplyr::mutate(
      munic_code = stringr::str_extract(municipio, "^[0-9]+"),
      munic_name = stringr::str_remove(municipio, "^[0-9]+ - "),
      .before = municipio
    ) %>%
    dplyr::select(-municipio)

  # FRAGILE: The portal exports monthly columns ordered 1:12 with dose columns nested.
  # If the table header sequence cannot be resolved, doses match sorted checkbox labels.
  ndoses <- length(dose_labels)
  nmeses <- 12
  meses <- rep(month.name[1:nmeses], each = ndoses)
  doses <- rep(dose_labels, nmeses)

  if ((ncol(df) - 2) == length(meses)) {
    names(df)[-(1:2)] <- sprintf("%s_%s", meses, doses)
  }

  dat_final <- df %>%
    tidyr::pivot_longer(
      cols = -c(munic_code, munic_name),
      names_to = c("mes", "dose"),
      names_sep = "_",
      values_to = "quantity"
    ) %>%
    dplyr::mutate(
      munic_code = as.numeric(munic_code),
      munic_name = stringr::str_to_lower(munic_name),
      month      = stringr::str_to_lower(mes),
      quantity   = suppressWarnings(as.numeric(quantity)),
      state      = state,
      year       = as.integer(year),
      strategy   = strategy,
      product    = product
    ) %>%
    dplyr::select(state, year, munic_code, munic_name, strategy, product, month, dose, quantity)

  return(dat_final)
}

load_pni <- function(year, state, strategy, product, language = "eng") {
  target_url <- "https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf"
  form_id <- "dosesAplicadasMensalForm"

  f <- function(x) paste0(form_id, ":", x)

  enc <- function(fields) {
    paste(
      paste0(curl::curl_escape(names(fields)), "=", curl::curl_escape(unlist(fields))),
      collapse = "&"
    )
  }

  fetch_retry <- function(url, handle, max_retries = 3) {
    for (i in seq_len(max_retries)) {
      res <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(e) e
      )
      if (!inherits(res, "error") && res$status_code %in% c(200, 302)) {
        return(res)
      }
      Sys.sleep(1.5 * i)
    }
    if (inherits(res, "error")) {
      stop("Failed to connect to SI-PNI portal: ", conditionMessage(res))
    }
    stop("SI-PNI portal returned HTTP status ", res$status_code)
  }

  message("Retrieving SI-PNI vaccination data from DATASUS via HTTP...")

  # Handle environment isolation for temporary session cookies
  withr::with_tempfile("cookie_jar", {

    # Initialize curl handle with session cookies and forced HTTP/1.1 (prevents ALPN resets)
    h <- curl::new_handle(
      cookiefile = "",
      followlocation = TRUE,
      timeout = 90,
      connecttimeout = 30,
      http_version = 2
    )
    curl::handle_setheaders(h,
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    )

    # 1. Initial GET to obtain session cookie and ViewState
    resp_init <- fetch_retry(target_url, h)
    init_html <- rawToChar(resp_init$content)
    Encoding(init_html) <- "UTF-8"

    vs_match <- regmatches(init_html, regexpr('(?<=javax\\.faces\\.ViewState" value=")[^"]+', init_html, perl = TRUE))
    if (length(vs_match) == 0) {
      vs_match <- sub('.*value="', "", regmatches(init_html, regexpr('javax\\.faces\\.ViewState[^>]*value="[^"]+', init_html)))
    }
    if (length(vs_match) == 0 || !nzchar(vs_match)) {
      stop("Could not extract javax.faces.ViewState from SI-PNI portal.")
    }
    curr_vs <- vs_match

    # Dynamic extraction of State (UF) code
    state_opt <- regmatches(init_html, regexpr(sprintf('<option\\s+value="([^"]+)">\\s*%s\\s*</option>', state), init_html, ignore.case = TRUE))
    if (length(state_opt) == 0) {
      stop("State '", state, "' not found in SI-PNI dropdown.")
    }
    uf_val <- sub('.*value="([^"]+)".*', "\\1", state_opt)

    # Dynamic extraction of Strategy code (strictly require exact match, avoiding silent mis-selection)
    strat_clean <- tolower(stringr::str_squish(strategy))
    strat_opts <- regmatches(init_html, gregexpr('<option\\s+value="([^"]+)">([^<]+)</option>', init_html))[[1]]
    strat_labels <- sub('.*>([^<]+)<.*', "\\1", strat_opts)
    strat_idx <- which(tolower(stringr::str_squish(strat_labels)) == strat_clean)
    if (length(strat_idx) == 0) {
      avail_strats <- strat_labels[nzchar(trimws(strat_labels)) & !grepl("^selecione", strat_labels, ignore.case = TRUE)]
      stop("Strategy '", strategy, "' not found in SI-PNI options. Available: ", paste(avail_strats, collapse = ", "))
    }
    strat_val <- sub('.*value="([^"]+)".*', "\\1", strat_opts[strat_idx[1]])

    base_form <- function(vs_val) {
      l <- list()
      l[[form_id]] <- form_id
      l[["javax.faces.ViewState"]] <- vs_val
      l[[f("ufPesquisa_input")]] <- uf_val
      l[[f("chkTotalizarIBGE_input")]] <- "on"
      l[[f("ano")]] <- ""
      l
    }

    ajax_req <- function(source_el, render_els, extra = list()) {
      fl <- c(list(
        "javax.faces.partial.ajax" = "true",
        "javax.faces.source" = f(source_el),
        "javax.faces.partial.execute" = f(source_el),
        "javax.faces.partial.render" = paste(f(render_els), collapse = " "),
        "javax.faces.behavior.event" = "change",
        "javax.faces.partial.event" = "change"
      ), base_form(curr_vs), extra)

      curl::handle_setopt(h, copypostfields = enc(fl), post = TRUE)
      curl::handle_setheaders(h,
        "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
        "Faces-Request" = "partial/ajax",
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
      )
      res <- fetch_retry(target_url, h)
      txt_res <- rawToChar(res$content)
      Encoding(txt_res) <- "UTF-8"

      new_vs <- regmatches(txt_res, regexpr('(?<=ViewState"><!\\[CDATA\\[)[^\\]]+', txt_res, perl = TRUE))
      if (length(new_vs) > 0 && nzchar(new_vs)) {
        curr_vs <<- new_vs
      }
      txt_res
    }

    # 2. Select UF
    ajax_req("ufPesquisa", c("municipioPesquisa", "easPesquisa"))

    # 3. Mark IBGE municipality totalization
    ajax_req("chkTotalizarIBGE", c("municipioPesquisa", "ufPesquisa", "easPesquisa", "chkTotalizarEAS"))

    # 4. Select Strategy & retrieve product options
    extra_strat <- list()
    extra_strat[[f("estrategiaPesquisa_input")]] <- strat_val
    resp_strat <- ajax_req("estrategiaPesquisa", c("produtoPesquisa", "dosePesquisa"), extra_strat)

    # 5. Find matching product in strategy options (strictly require exact match, avoiding silent mis-selection)
    prod_opts <- regmatches(resp_strat, gregexpr('<option\\s+value="([^"]+)"[^>]*>([^<]+)</option>', resp_strat))[[1]]
    if (length(prod_opts) == 0) {
      stop("No products returned for strategy '", strategy, "'.")
    }
    prod_labels <- sub('.*>([^<]+)<.*', "\\1", prod_opts)
    prod_clean <- tolower(stringr::str_squish(product))
    prod_idx <- which(tolower(stringr::str_squish(prod_labels)) == prod_clean)
    if (length(prod_idx) == 0) {
      avail_prods <- prod_labels[nzchar(trimws(prod_labels)) & !grepl("^selecione", prod_labels, ignore.case = TRUE)]
      stop(
        "Product '", product, "' not available for strategy '", strategy, "'. ",
        "Available products: ", paste(avail_prods, collapse = ", ")
      )
    }
    prod_val <- sub('.*value="([^"]+)".*', "\\1", prod_opts[prod_idx[1]])

    # 6. Select Product & retrieve available doses
    extra_prod <- extra_strat
    extra_prod[[f("produtoPesquisa_input")]] <- prod_val
    resp_prod <- ajax_req("produtoPesquisa", "dosePesquisa", extra_prod)

    dose_inputs <- regmatches(resp_prod, gregexpr('<input[^>]*name="[^"]*dosePesquisa"[^>]*>', resp_prod))[[1]]
    dose_vals <- unique(sub('.*value="([^"]+)".*', "\\1", dose_inputs))

    dose_lbl_tags <- regmatches(resp_prod, gregexpr('<label[^>]*for="[^"]*dosePesquisa[^"]*"[^>]*>[^<]+</label>', resp_prod))[[1]]
    dose_labels_raw <- trimws(gsub("<[^>]+>", "", dose_lbl_tags))
    dose_labels <- unique(dose_labels_raw[dose_labels_raw != "REF" & !grepl("^REFOR", dose_labels_raw, ignore.case = TRUE)])
    dose_labels <- sort(dose_labels)

    if (length(dose_labels) == 0 && length(dose_vals) > 0) {
      dose_labels <- sort(unique(dose_labels_raw))
    }

    # 7. Submit Search Query
    submit_match <- regmatches(init_html, regexpr('<input[^>]*type=[\'"]submit[\'"][^>]*value=[\'"]Pesquisar[\'"][^>]*name=[\'"]([^\'"]+)[\'"]', init_html, ignore.case = TRUE))
    submit_name <- if (length(submit_match) > 0) sub('.*name=[\'"]([^\'"]+)[\'"].*', "\\1", submit_match) else f("j_idt123")

    fl_search <- base_form(curr_vs)
    fl_search[[f("ano")]] <- as.character(year)
    fl_search <- c(fl_search, extra_prod)
    for (dv in dose_vals) {
      fl_search <- c(fl_search, setNames(list(dv), f("dosePesquisa")))
    }
    fl_search <- c(fl_search, list(
      "javax.faces.source" = submit_name,
      "javax.faces.partial.event" = "click",
      "javax.faces.partial.execute" = paste(submit_name, form_id),
      "javax.faces.partial.render" = form_id,
      "javax.faces.behavior.event" = "click",
      "javax.faces.partial.ajax" = "true"
    ))

    curl::handle_setopt(h, copypostfields = enc(fl_search), post = TRUE)
    curl::handle_setheaders(h,
      "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "Faces-Request" = "partial/ajax",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    )
    res_search <- fetch_retry(target_url, h)
    txt_search <- rawToChar(res_search$content)
    Encoding(txt_search) <- "UTF-8"

    if (grepl("Nenhum Registro Encontrado", txt_search, ignore.case = TRUE)) {
      message("No records found for the specified parameters.")
      return(tibble::tibble())
    }

    vs_search <- regmatches(txt_search, regexpr('(?<=ViewState"><!\\[CDATA\\[)[^\\]]+', txt_search, perl = TRUE))
    if (length(vs_search) > 0 && nzchar(vs_search)) {
      curr_vs <- vs_search
    }

    # Inspect the search response table header (<thead>) to verify dose columns order
    thead_match <- regmatches(txt_search, regexpr("<thead[^>]*>([\\s\\S]*?)</thead>", txt_search, perl = TRUE))
    if (length(thead_match) > 0) {
      th_matches <- regmatches(thead_match, gregexpr("<th[^>]*>([\\s\\S]*?)</th>", thead_match, perl = TRUE))[[1]]
      th_texts <- trimws(gsub("<[^>]+>", "", th_matches))
      th_candidates <- unique(th_texts[th_texts %in% dose_labels_raw])
      if (length(th_candidates) == length(dose_labels)) {
        dose_labels <- th_candidates
      }
    }

    # 8. Export CSV
    csv_id <- NULL
    csv_links <- regmatches(txt_search, gregexpr('mojarra\\.jsfcljs\\([^,]+,\\{\'([^:]+:j_idt[0-9]+)\'', txt_search))[[1]]
    if (length(csv_links) >= 2) {
      csv_id <- sub('.*\\{\'([^:]+:j_idt[0-9]+)\'.*', "\\1", csv_links[2])
    } else if (length(csv_links) == 1) {
      csv_id <- sub('.*\\{\'([^:]+:j_idt[0-9]+)\'.*', "\\1", csv_links[1])
    }
    if (is.null(csv_id)) csv_id <- f("j_idt1449")

    ex_fields <- base_form(curr_vs)
    ex_fields[[f("ano")]] <- as.character(year)
    ex_fields <- c(ex_fields, extra_prod)
    for (dv in dose_vals) {
      ex_fields <- c(ex_fields, setNames(list(dv), f("dosePesquisa")))
    }
    ex_fields[[csv_id]] <- csv_id

    curl::handle_setopt(h, copypostfields = enc(ex_fields), post = TRUE)
    curl::handle_setheaders(h,
      "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "Faces-Request" = "",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    )
    res_export <- fetch_retry(target_url, h)

    raw_content <- res_export$content
    is_xls <- length(raw_content) >= 4 && identical(raw_content[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))

    if (is_xls) {
      tmp_xls <- tempfile(fileext = ".xls")
      on.exit(unlink(tmp_xls), add = TRUE)
      writeBin(raw_content, tmp_xls)
      dat <- suppressMessages(readxl::read_excel(tmp_xls))
    } else {
      dat <- suppressMessages(
        readr::read_csv(
          raw_content,
          show_col_types = FALSE,
          locale = readr::locale(encoding = "ISO-8859-1")
        )
      )
    }
  })

  return(parse_pni_table(dat, state, year, strategy, product, dose_labels))
}
