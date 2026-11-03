# actions/scrapers/resolve_sipni.R
#
# Resolver for SI-PNI (Sistema de Informação do Programa Nacional de Imunizações)
# Web portal consolidated monthly applied doses report.
#
# INVESTIGATION & VERIFICATION RECORD (2026-11-02):
# - Host: sipni.datasus.gov.br (HTTPS / JSF PrimeFaces portal)
# - Endpoint: https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf
# - Verified live: HTTP 200, serves dosesAplicadasMensalForm and ViewState.
# - Target range: 1994-2022 (historical series; 2023+ uses local standardized files).
# - Docs page: https://datasus.saude.gov.br/acesso-a-informacao/imunizacoes-desde-1994/ (HTTP 200).
# - Flaky host protection: sipni server has periodic high latency; retries up to 2 times
#   with 30s timeout and relaxed SSL verification.

resolve_sipni <- function(rows) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("resolve_sipni: curl package is required")
  }

  target_url <- "https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf"
  docs_url <- "https://datasus.saude.gov.br/acesso-a-informacao/imunizacoes-desde-1994/"

  fetch_portal <- function() {
    h <- curl::new_handle(
      timeout = 45,
      connecttimeout = 30,
      followlocation = TRUE,
      ssl_verifypeer = FALSE,
      ssl_verifyhost = FALSE
    )
    curl::handle_setheaders(h,
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    )
    curl::curl_fetch_memory(target_url, handle = h)
  }

  resp <- NULL
  last_err <- NULL

  for (attempt in 1:2) {
    resp <- tryCatch(fetch_portal(), error = function(e) {
      last_err <<- conditionMessage(e)
      NULL
    })
    if (!is.null(resp) && resp$status_code == 200) break
    Sys.sleep(2)
  }

  if (is.null(resp)) {
    stop("resolve_sipni: failed to connect to SI-PNI portal after 2 attempts: ", last_err)
  }

  if (resp$status_code != 200) {
    stop("resolve_sipni: SI-PNI portal returned HTTP status ", resp$status_code)
  }

  html_txt <- rawToChar(resp$content)
  if (!grepl("dosesAplicadasMensalForm", html_txt, fixed = TRUE)) {
    stop("resolve_sipni: SI-PNI portal response does not contain 'dosesAplicadasMensalForm'")
  }

  tibble::tibble(
    survey = "datasus",
    dataset = "datasus_sipni",
    geo_level = NA_character_,
    year = NA_character_,
    url = target_url,
    docs_url = docs_url,
    available_time = "1994-2022",
    available_geo = "State",
    resolver = "sipni"
  )
}
