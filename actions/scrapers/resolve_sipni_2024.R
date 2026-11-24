# actions/scrapers/resolve_sipni_2024.R
#
# Resolver for SI-PNI 2024+ (Qlik Sense dashboard at infoms.saude.gov.br).
#
# Standalone CI script (must NOT load the package):
# - Parses entry URL and current Qlik App ID from the manifest row.
# - Fetches index.html and its mashup JS to detect if the Ministry of Health
#   republished the dashboard with a new App ID (`qlik.openApp("<id>")`).
# - Connects to the Qlik Engine WebSocket JSON-RPC API, verifies the App opens,
#   confirms all required fields are present, and reads available `Ano Vacina`
#   years to keep `available_time` and `url` up to date in the manifest.

SIPNI_2024_REQUIRED_FIELDS <- c(
  "Ano Vacina",
  "UF",
  "Estrat\u00e9gia Vacina\u00e7\u00e3o",
  "Imunobiol\u00f3gico",
  "C\u00f3d. Munic\u00edpio",
  "Munic\u00edpio",
  "M\u00eas Vacina",
  "Tipo de Dose"
)

sipni_2024_raw_to_text <- function(x, max_bytes) {
  if (is.null(x) || length(x) == 0) return("")
  x <- x[x != as.raw(0)]
  x <- x[seq_len(min(length(x), max_bytes))]
  iconv(rawToChar(x), from = "UTF-8", to = "UTF-8", sub = "?")
}

sipni_2024_failed_evidence <- function(resp, fetch_error = NULL, max_body_chars = 600L) {
  if (is.null(resp)) {
    return(paste0(
      "no HTTP response was received -- curl error: ",
      if (is.null(fetch_error) || !nzchar(fetch_error)) "(none captured)" else fetch_error
    ))
  }
  header_text <- sipni_2024_raw_to_text(resp$headers, 16384L)
  header_lines <- trimws(strsplit(header_text, "\r?\n")[[1]])
  fields <- header_lines[grepl("^[^:[:space:]]+:", header_lines)]
  field_names <- tolower(sub(":.*$", "", fields))
  field_values <- trimws(sub("^[^:]+:", "", fields))
  keep <- grepl("^(server|cf-|x-|content-type|content-length|retry-after|location|via)", field_names)
  kept <- sprintf("  %s: %s", field_names[keep], substr(field_values[keep], 1, 200))
  squish <- function(s) trimws(gsub("[[:space:]]+", " ", gsub("[[:cntrl:]]", " ", s)))
  body <- squish(sipni_2024_raw_to_text(resp$content, 4096L))
  shown <- substr(body, 1, max_body_chars)
  paste(c(
    sprintf("HTTP %s from %s", resp$status_code, if (is.null(resp$url)) "(unknown url)" else resp$url),
    if (length(kept) > 0) kept else "  (no relevant headers)",
    if (nzchar(shown)) sprintf("body (%d bytes; excerpt): %s", length(resp$content), shown) else "body: (empty)"
  ), collapse = "\n")
}

resolve_sipni_2024 <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop("resolve_sipni_2024(): no manifest rows tagged resolver == 'sipni_2024'")
  }
  for (pkg in c("curl", "websocket", "later", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("resolve_sipni_2024(): ", pkg, " package is required")
    }
  }

  manifest_url <- rows$url[1]
  if (is.na(manifest_url) || !nzchar(manifest_url)) {
    stop("resolve_sipni_2024(): target row has no url")
  }

  base_url <- sub("^(https?://[^/]+).*$", "\\1", manifest_url)
  entry_url <- sub("\\?app=.*$", "", manifest_url)
  app_id <- if (grepl("[?&]app=([^&]+)", manifest_url)) {
    sub(".*[?&]app=([^&]+).*$", "\\1", manifest_url)
  } else {
    ""
  }

  h <- curl::new_handle(cookiefile = "", followlocation = TRUE, http_version = 2, timeout = 30)
  curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

  r_entry <- tryCatch(curl::curl_fetch_memory(entry_url, handle = h), error = function(e) e)
  if (inherits(r_entry, "error")) {
    stop("resolve_sipni_2024(): could not reach ", entry_url, "\n", sipni_2024_failed_evidence(NULL, conditionMessage(r_entry)))
  }
  if (r_entry$status_code != 200) {
    stop("resolve_sipni_2024(): ", entry_url, " returned HTTP ", r_entry$status_code, "\n", sipni_2024_failed_evidence(r_entry))
  }

  # Inspect mashup JS for live qlik.openApp("<app_id>")
  entry_html <- sipni_2024_raw_to_text(r_entry$content, 131072L)
  js_match <- regmatches(entry_html, regexpr('src="([^"]*SEIDIGI_DEMAS_VACINACAO[^"]*\\.js[^"]*)"', entry_html))
  if (length(js_match) > 0 && nzchar(js_match)) {
    js_rel <- sub('^src="([^"]+)".*$', "\\1", js_match)
    js_url <- if (grepl("^https?://", js_rel)) js_rel else paste0(dirname(entry_url), "/", js_rel)
    r_js <- tryCatch(curl::curl_fetch_memory(js_url, handle = h), error = function(e) e)
    if (!inherits(r_js, "error") && r_js$status_code == 200) {
      js_txt <- sipni_2024_raw_to_text(r_js$content, 262144L)
      # Match active (non-commented) qlik.openApp("uuid", config)
      lines <- strsplit(js_txt, "\r?\n")[[1]]
      active_lines <- lines[!grepl("^\\s*//", lines)]
      app_hit <- regmatches(active_lines, regexpr('qlik\\.openApp\\(["\']([0-9a-f-]{36})["\']', active_lines))
      app_hit <- app_hit[nzchar(app_hit)]
      if (length(app_hit) > 0) {
        live_app_id <- sub('.*qlik\\.openApp\\(["\']([0-9a-f-]{36})["\'].*', "\\1", app_hit[1])
        if (nzchar(live_app_id)) app_id <- live_app_id
      }
    }
  }

  if (!nzchar(app_id)) {
    stop("resolve_sipni_2024(): could not determine Qlik App ID from manifest or mashup JS")
  }

  r_csrf <- tryCatch(curl::curl_fetch_memory(paste0(base_url, "/qps/csrftoken"), handle = h), error = function(e) e)
  if (inherits(r_csrf, "error")) {
    stop("resolve_sipni_2024(): failed to fetch /qps/csrftoken\n", sipni_2024_failed_evidence(NULL, conditionMessage(r_csrf)))
  }
  csrf <- curl::parse_headers_list(r_csrf$headers)[["qlik-csrf-token"]]
  if (is.null(csrf) || !nzchar(csrf)) {
    stop("resolve_sipni_2024(): missing qlik-csrf-token header\n", sipni_2024_failed_evidence(r_csrf))
  }

  cks <- curl::handle_cookies(h)
  cookie_hdr <- paste0(cks$name, "=", cks$value, collapse = "; ")
  ws_host <- sub("^https?://", "", base_url)
  ws_url <- paste0("wss://", ws_host, "/app/", app_id, "?qlik-csrf-token=", curl::curl_escape(csrf))

  ws <- websocket::WebSocket$new(
    ws_url,
    headers = list(Cookie = cookie_hdr, Origin = base_url, "User-Agent" = "Mozilla/5.0"),
    autoConnect = FALSE
  )
  on.exit(try(ws$close(), silent = TRUE), add = TRUE)

  responses <- list()
  next_id <- 1L
  ws_err <- NULL
  opened <- FALSE
  ws$onOpen(function(event) { opened <<- TRUE })
  ws$onError(function(event) { ws_err <<- event$message })
  ws$onMessage(function(event) {
    parsed <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
    if (!is.null(parsed$id)) responses[[as.character(parsed$id)]] <<- parsed
  })
  ws$connect()

  for (i in 1:300) {
    later::run_now(0.05)
    if (opened || !is.null(ws_err)) break
  }
  if (!is.null(ws_err)) stop("resolve_sipni_2024(): WebSocket error: ", ws_err)
  if (!opened) stop("resolve_sipni_2024(): WebSocket connection timed out")

  send_rpc <- function(handle, method, params = list()) {
    id <- next_id
    next_id <<- next_id + 1L
    ws$send(jsonlite::toJSON(list(
      jsonrpc = "2.0", id = id, handle = handle, method = method, params = params
    ), auto_unbox = TRUE))
    for (i in 1:400) {
      later::run_now(0.05)
      if (!is.null(ws_err)) stop("resolve_sipni_2024(): WebSocket error: ", ws_err)
      if (!is.null(responses[[as.character(id)]])) return(responses[[as.character(id)]])
    }
    stop("resolve_sipni_2024(): timeout waiting for ", method)
  }

  doc_res <- send_rpc(-1L, "OpenDoc", list(app_id))
  if (!is.null(doc_res$error)) {
    stop("resolve_sipni_2024(): OpenDoc failed for ", app_id, ": ", doc_res$error$message)
  }
  doc_h <- doc_res$result$qReturn$qHandle
  send_rpc(doc_h, "ClearAll", list(TRUE))

  # Verify required fields exist in the live Qlik app
  fl_obj <- send_rpc(doc_h, "CreateSessionObject", list(list(
    qInfo = list(qType = "FieldList"),
    qFieldListDef = list(qShowSystem = FALSE, qShowHidden = FALSE, qShowSemantic = TRUE)
  )))
  fl_lay <- send_rpc(fl_obj$result$qReturn$qHandle, "GetLayout", list())
  live_fields <- vapply(fl_lay$result$qLayout$qFieldList$qItems, function(x) x$qName, character(1))
  missing_fields <- setdiff(SIPNI_2024_REQUIRED_FIELDS, live_fields)
  if (length(missing_fields) > 0) {
    stop(
      "resolve_sipni_2024(): Qlik app ", app_id, " is missing expected field(s): ",
      paste(missing_fields, collapse = ", ")
    )
  }

  # Read live available years from `Ano Vacina`
  yr_obj <- send_rpc(doc_h, "CreateSessionObject", list(list(
    qInfo = list(qType = "ListObject"),
    qListObjectDef = list(
      qDef = list(qFieldDefs = list("Ano Vacina")),
      qInitialDataFetch = list(list(qTop = 0L, qLeft = 0L, qHeight = 100L, qWidth = 1L))
    )
  )))
  yr_lay <- send_rpc(yr_obj$result$qReturn$qHandle, "GetLayout", list())
  yr_vals <- suppressWarnings(as.integer(vapply(
    yr_lay$result$qLayout$qListObject$qDataPages[[1]]$qMatrix,
    function(r) r[[1]]$qText,
    character(1)
  )))
  yr_vals <- sort(unique(yr_vals[!is.na(yr_vals)]))
  if (length(yr_vals) == 0) {
    stop("resolve_sipni_2024(): no numeric years found in 'Ano Vacina'")
  }

  avail_time <- if (length(yr_vals) == 1) {
    as.character(yr_vals)
  } else if (identical(yr_vals, seq(min(yr_vals), max(yr_vals)))) {
    sprintf("%d-%d", min(yr_vals), max(yr_vals))
  } else {
    paste(yr_vals, collapse = ", ")
  }

  resolved_url <- paste0(entry_url, "?app=", app_id)

  tibble::tibble(
    survey = rows$survey,
    dataset = rows$dataset,
    geo_level = rows$geo_level,
    year = rows$year,
    url = resolved_url,
    docs_url = rows$docs_url,
    available_time = avail_time,
    resolver = "sipni_2024"
  )
}
