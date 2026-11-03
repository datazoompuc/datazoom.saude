# actions/scrapers/resolve_sipni.R
#
# Resolver for SI-PNI (Sistema de Informação do Programa Nacional de Imunizações).
#
# SI-PNI is a historical series published on the legacy web portal (1994-2023).
# Following the resolve_degrad.R verification model (datazoom.amazonia):
# - Its role is pure verification (liveness check of the legacy portal URL).
# - It never disables SSL/TLS verification (no ssl_verifypeer = FALSE).
# - It does not write available_time (the dataset is marked AVAILABLE_TIME_EXEMPT
#   in actions/scripts/manifest_validate.R, leaving the committed "1994-2023" intact).

sipni_transport_error <- function(url, curl_msg) {
  reason <- if (grepl("certificate has expired|CERT_EXPIRED|certificate expired", curl_msg, ignore.case = TRUE)) {
    "the server's TLS certificate has expired"
  } else if (grepl("issuer certificate|UNTRUSTED|unable to verify|incomplete|handshake", curl_msg, ignore.case = TRUE)) {
    paste0(
      "this runner cannot verify the server's TLS certificate chain. ",
      "Never turn verification off; add the missing intermediate CA to the runner's trust store"
    )
  } else if (grepl("timed? ?out", curl_msg, ignore.case = TRUE)) {
    "connection timed out reaching the server"
  } else {
    "no HTTP response (DNS, connection, timeout or TLS failure)"
  }
  sprintf("could not reach %s -- %s. curl said: %s", url, reason, curl_msg)
}

sipni_peek_portal <- function(url, timeout_s = 30) {
  h <- curl::new_handle(timeout = timeout_s, followlocation = TRUE)
  curl::handle_setheaders(h,
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  )
  con <- curl::curl(url, handle = h)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  open(con, "rb")
  d <- curl::handle_data(h)
  list(status = d$status_code, type = d$type)
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

  peek <- tryCatch(sipni_peek_portal(target_url, timeout_s = 30), error = function(e) e)

  if (inherits(peek, "error")) {
    stop("resolve_sipni(): ", sipni_transport_error(target_url, conditionMessage(peek)))
  }

  if (length(peek$status) != 1 || is.na(peek$status) || peek$status != 200) {
    stop(sprintf("resolve_sipni(): %s returned HTTP %s instead of 200", target_url, peek$status))
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
