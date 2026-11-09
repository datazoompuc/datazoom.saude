load_pni <- function(year, state, strategy, product, dose = NULL, data = NULL) {

  # Check if a manual local file path was provided
  if (!is.null(data)) {

    dat <- suppressMessages(
      readxl::read_excel(data)
    )

    # Extract dose labels directly from the spreadsheet structure
    dose_labels <- toupper(dose)
    dose_labels <- sort(dose_labels)

  } else {

    # Automated retrieval from legacy SI-PNI Web portal via plain HTTP
    # FRAGILE: Assumes legacy SI-PNI Web portal (JSF/PrimeFaces) endpoint and DOM structure remain unchanged.
    target_url <- dataset_url("datasus", "datasus_sipni")
    if (is.na(target_url) || !nzchar(target_url)) {
      stop("SI-PNI endpoint is not available in the manifest. The dataset may be temporarily disabled or unavailable.")
    }

    message("Retrieving SI-PNI vaccination data from DATASUS via HTTP...")

    form_id <- "dosesAplicadasMensalForm"
    f <- function(x) paste0(form_id, ":", x)
    enc <- function(fields) {
      paste(paste0(curl::curl_escape(names(fields)), "=", curl::curl_escape(unlist(fields))), collapse = "&")
    }

    # Helper for robust HTTP fetch with retries (handles DATASUS transient drops)
    fetch_retry <- function(url, handle, max_retries = 4, delay = 2) {
      for (att in seq_len(max_retries)) {
        res <- tryCatch(
          curl::curl_fetch_memory(url, handle = handle),
          error = function(e) e
        )
        if (!inherits(res, "error") && res$status_code %in% c(200, 302)) {
          return(res)
        }
        if (att < max_retries) {
          Sys.sleep(delay * att)
        }
      }
      if (inherits(res, "error")) {
        stop("Failed to connect to SI-PNI portal: ", conditionMessage(res))
      }
      stop("SI-PNI portal returned HTTP error ", res$status_code)
    }

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

    # Dynamic extraction of Strategy code
    strat_clean <- stringr::str_squish(strategy)
    strat_opts <- regmatches(init_html, gregexpr('<option\\s+value="([^"]+)">([^<]+)</option>', init_html))[[1]]
    strat_labels <- sub('.*>([^<]+)<.*', "\\1", strat_opts)
    strat_idx <- which(tolower(stringr::str_squish(strat_labels)) == tolower(strat_clean))
    if (length(strat_idx) == 0) {
      strat_idx <- grep(strat_clean, strat_labels, ignore.case = TRUE)
    }
    if (length(strat_idx) == 0) {
      stop("Strategy '", strategy, "' not found in SI-PNI options.")
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

    # 5. Find matching product in strategy options
    prod_opts <- regmatches(resp_strat, gregexpr('<option\\s+value="([^"]+)"[^>]*>([^<]+)</option>', resp_strat))[[1]]
    if (length(prod_opts) == 0) {
      stop("No products returned for strategy '", strategy, "'.")
    }
    prod_labels <- sub('.*>([^<]+)<.*', "\\1", prod_opts)
    prod_clean <- tolower(stringr::str_squish(product))
    prod_idx <- which(tolower(stringr::str_squish(prod_labels)) == prod_clean)
    if (length(prod_idx) == 0) {
      first_term <- strsplit(product, "[-–]")[[1]][1]
      prod_idx <- grep(stringr::str_squish(first_term), prod_labels, ignore.case = TRUE)
    }
    if (length(prod_idx) == 0) {
      stop("Product '", product, "' not available for strategy '", strategy, "'.")
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
      "Content-Type" = "application/x-www-form-urlencoded",
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
  }

  # Data Wrangling and Column Renaming
  df <- dat %>%
    dplyr::rename(municipio = 1) %>% # The first column contains municipality info
    dplyr::mutate(
      munic_code  = stringr::str_extract(municipio, "^[0-9]+"), # Extract IBGE code
      munic_name  = stringr::str_remove(municipio, "^[0-9]+ - "), # Extract city name
      .before = municipio
    ) %>%
    dplyr::select(-municipio)

  # Calculate column mappings based on months and extracted dose labels
  ndoses  <- length(dose_labels)
  nmeses  <- 12
  meses   <- rep(month.name[1:nmeses], each = ndoses)
  doses   <- rep(dose_labels, nmeses)

  # Dynamically assign meaningful names to data columns
  if ((ncol(df) - 2) == length(meses)) {
    names(df)[-(1:2)] <- sprintf("%s_%s", meses, doses)
  }

  # Final transformation: pivot to long format and clean up data types
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
