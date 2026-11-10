# actions/scrapers/resolve_sipni.R
#
# Resolver for SI-PNI (Sistema de Informação do Programa Nacional de Imunizações).
#
# SI-PNI is a historical series published on the legacy web portal (1994-2023).
# Following the resolve_degrad.R / resolve_seeg.R verification models (datazoom.amazonia):
# - Its role is verification of the legacy portal URL and JSF form availability.
# - It never disables SSL/TLS verification (no ssl_verifypeer = FALSE).
# - It does not write available_time (the dataset is marked AVAILABLE_TIME_EXEMPT
#   in actions/scripts/manifest_validate.R, leaving the committed "1994-2023" intact).
# - Gathers detailed HTTP diagnostic evidence (headers, status, title, body excerpt)
#   when network or server refusal occurs, ensuring actionable CI failure reports.

sipni_raw_to_text <- function(x, max_bytes) {
  if (is.null(x) || length(x) == 0) return("")
  x <- x[x != as.raw(0)]
  x <- x[seq_len(min(length(x), max_bytes))]
  iconv(rawToChar(x), from = "UTF-8", to = "UTF-8", sub = "?")
}

sipni_failed_fetch_evidence <- function(resp, fetch_error = NULL, max_body_chars = 600L) {
  if (is.null(resp)) {
    return(paste0(
      "no HTTP response was received -- curl error: ",
      if (is.null(fetch_error) || !nzchar(fetch_error)) "(none captured)" else fetch_error
    ))
  }

  header_text <- sipni_raw_to_text(resp$headers, 16384L)
  header_blocks <- strsplit(header_text, "\r?\n\r?\n")[[1]]
  header_blocks <- header_blocks[nzchar(trimws(header_blocks))]
  header_lines <- if (length(header_blocks) > 0) {
    trimws(strsplit(header_blocks[length(header_blocks)], "\r?\n")[[1]])
  } else {
    character(0)
  }
  fields <- header_lines[grepl("^[^:[:space:]]+:", header_lines)]
  field_names <- tolower(sub(":.*$", "", fields))
  field_values <- trimws(sub("^[^:]+:", "", fields))

  keep <- grepl("^(server|cf-|x-|content-type|content-length|retry-after|location|via)", field_names)
  kept <- sprintf("  %s: %s", field_names[keep], substr(field_values[keep], 1, 200))
  cookie_names <- unique(sub("=.*$", "", field_values[field_names == "set-cookie"]))

  squish <- function(s) trimws(gsub("[[:space:]]+", " ", gsub("[[:cntrl:]]", " ", s)))
  body <- squish(sipni_raw_to_text(resp$content, 4096L))
  title <- regmatches(body, regexpr("<title[^>]*>[^<]*</title>", body, ignore.case = TRUE))
  title <- if (length(title) > 0) squish(gsub("<[^>]*>", "", title)) else ""
  shown <- substr(body, 1, max_body_chars)

  paste(c(
    sprintf(
      "HTTP %s from %s", resp$status_code,
      if (is.null(resp$url)) "(unknown url)" else resp$url
    ),
    "response headers (only server, cf-*, x-*, content-type/length, retry-after, location, via):",
    if (length(kept) > 0) kept else "  (none of those headers present)",
    if (length(cookie_names) > 0) paste0("  set-cookie (names only): ", paste(cookie_names, collapse = ", ")),
    if (nzchar(title)) paste0("html <title>: ", title),
    if (length(resp$content) == 0 || !nzchar(shown)) {
      "body: (empty)"
    } else {
      sprintf(
        "body (%d bytes; first %d chars, whitespace collapsed): %s",
        length(resp$content), nchar(shown), shown
      )
    }
  ), collapse = "\n")
}

resolve_sipni <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    stop("resolve_sipni(): no manifest rows tagged resolver == 'sipni'")
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_sipni(): curl package is required")
  }

  target_url <- rows$url[1]
  if (is.na(target_url) || !nzchar(target_url)) {
    stop("resolve_sipni(): target row has no url to verify")
  }

  h <- curl::new_handle(
    timeout = 30,
    connecttimeout = 15,
    followlocation = TRUE,
    http_version = 2 # Enforce HTTP/1.1 to prevent ALPN connection resets
  )
  curl::handle_setheaders(h,
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  )

  fetch_res <- tryCatch(
    curl::curl_fetch_memory(target_url, handle = h),
    error = function(e) e
  )

  if (inherits(fetch_res, "error")) {
    evidence <- sipni_failed_fetch_evidence(NULL, fetch_error = conditionMessage(fetch_res))
    stop("resolve_sipni(): could not reach ", target_url, " -- network failure:\n", evidence)
  }

  if (fetch_res$status_code != 200) {
    evidence <- sipni_failed_fetch_evidence(fetch_res)
    stop("resolve_sipni(): ", target_url, " returned HTTP status ", fetch_res$status_code, ":\n", evidence)
  }

  html_content <- sipni_raw_to_text(fetch_res$content, 65536L)
  if (!grepl("javax\\.faces\\.ViewState", html_content)) {
    evidence <- sipni_failed_fetch_evidence(fetch_res)
    stop("resolve_sipni(): portal returned HTTP 200 but did not contain javax.faces.ViewState:\n", evidence)
  }

  # Return verified row echoing existing keys and URLs, exempt from rewriting available_time
  tibble::tibble(
    survey = rows$survey,
    dataset = rows$dataset,
    geo_level = rows$geo_level,
    year = rows$year,
    url = rows$url,
    docs_url = rows$docs_url,
    resolver = "sipni"
  )
}
