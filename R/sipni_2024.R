# R/sipni_2024.R
#
# SI-PNI (Sistema de Informacao do Programa Nacional de Imunizacoes) Scraper (2024+)
#
# Retrieves consolidated municipal vaccination data from the new DATASUS
# interactive Qlik Sense dashboard (infoms.saude.gov.br) via the Qlik Engine
# JSON-RPC WebSocket API without requiring a browser.
#
# Produces the exact same 9-column schema as load_pni() (1994-2023) so users
# can combine historical and recent years seamlessly.

PNI_2024_BASE_URL <- "https://infoms.saude.gov.br"
PNI_2024_APP_ID <- "eb8f8efd-114c-4c45-9c23-6faff35e28fd"
PNI_2024_ENTRY_URL <- paste0(
  PNI_2024_BASE_URL,
  "/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA/index.html"
)

# FRAGILE: Qlik field names and app/object IDs depend on the upstream dashboard
# deployment (SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA).
PNI_2024_FIELDS <- list(
  year = "Ano Vacina",
  state = "UF",
  strategy = "Estrat\u00e9gia Vacina\u00e7\u00e3o",
  product = "Imunobiol\u00f3gico",
  munic_code = "C\u00f3d. Munic\u00edpio",
  munic_name = "Munic\u00edpio",
  month = "M\u00eas Vacina",
  dose = "Tipo de Dose"
)

pni_2024_map_month <- function(x) {
  month_map <- stats::setNames(
    tolower(month.name),
    c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  )
  x_clean <- tolower(trimws(as.character(x)))
  out <- unname(month_map[x_clean])
  ifelse(is.na(out), x_clean, out)
}

pni_2024_map_dose <- function(x) {
  x <- as.character(x)
  # Strip leading numeric code ("1 - 1a Dose" -> "1a Dose") before matching
  x_lbl <- trimws(sub("^[0-9]+\\s*-\\s*", "", x))

  dplyr::case_when(
    stringr::str_detect(x_lbl, "Dose Zero") ~ "D0",
    stringr::str_detect(x_lbl, "Dose Inicial") ~ "Inicial",
    stringr::str_detect(x_lbl, "Dose Adicional") ~ "Adicional",
    stringr::str_detect(x_lbl, "Fracionada") ~ "Fracionada",
    stringr::str_detect(x_lbl, "Revacina\u00e7\u00e3o") ~ "REV",
    stringr::str_detect(x_lbl, "Refor\u00e7o") ~ "REF",
    stringr::str_detect(x_lbl, "\u00danica") ~ "DU",
    stringr::str_detect(x_lbl, "1\u00aa Dose|^Dose$") ~ "D1",
    stringr::str_detect(x_lbl, "2\u00aa Dose") ~ "D2",
    stringr::str_detect(x_lbl, "3\u00aa Dose") ~ "D3",
    stringr::str_detect(x_lbl, "4\u00aa Dose") ~ "D4",
    stringr::str_detect(x_lbl, "5\u00aa Dose") ~ "D5",
    stringr::str_detect(x_lbl, "Profilaxia") ~ "Profilaxia",
    stringr::str_detect(x_lbl, "Tratamento") ~ "Tratamento",
    TRUE ~ x_lbl
  )
}

pni_2024_map_strategy <- function(strategy) {
  if (identical(strategy, "Monitoramento R\u00e1pido de Cobertura Vacinal")) {
    return("Monitoramento das Estrat\u00e9gias de Vacina\u00e7\u00e3o")
  }
  strategy
}

pni_2024_map_product <- function(product) {
  legacy_map <- c(
    "BCG - BCG" = "15 - vacina BCG",
    "C\u00f3lera oral - C\u00f3lera" = "75 - vacina c\u00f3lera oral",
    "DTP/HB/Hib - Penta" = "42 - vacina penta (DTP/HB/Hib)",
    "DTP/Hib - Tetra" = "39 - vacina DTP/Hib",
    "DTPa/Hib/Polio Inativa - PENTAinativada" = "29 - vacina penta acelular (DTPa/VIP/Hib)",
    "Dupla adulto - dT" = "25 - vacina difteria e t\u00e9tano adulto",
    "Dupla infantil - DT" = "5 - vacina difteria e t\u00e9tano infantil",
    "Dupla viral - SR" = "36 - vacina dupla viral",
    "Febre amarela - FA" = "14 - vacina febre amarela",
    "Febre Amarela-Dose fracionada (0,1 ml) - FA(0,1 ml)" = "84 - vacina febre amarela fracionada",
    "Febre tif\u00f3ide (atenuada) - Fta" = "51 - vacina febre tif\u00f3ide (atenuada)",
    "Febre tif\u00f3ide (polissacar\u00eddica) - FTp" = "30 - vacina febre tif\u00f3ide",
    "Haemophilus tipo b - Hib" = "17 - vacina Hib",
    "Hepatite A - HA" = "83 - vacina hepatite A adulto",
    "Hepatite A Pedi\u00e1trica - HAped" = "55 - vacina hepatite A infantil",
    "Hepatite AeB(pedi\u00e1trica) - HAeHBped" = "62 - vacina hepatite A e B infantil",
    "Hepatite AeB(uso adulto) - HAeHB" = "63 - vacina hepatite A e B adulto",
    "Hepatite B - HB" = "9 - vacina hepatite B",
    "Herpez Zoster - VHZ" = "76 - vacina herpes-z\u00f3ster (atenuada)",
    "Hexavalente - HEXA" = "43 - vacina hexa acelular (DTPa/HB/VIP/Hib)",
    "HPV Bivalente - HPV" = "60 - vacina HPV bivalente",
    "HPV Quadrivalente - HPV Quadri" = "67 - vacina HPV quadrivalente",
    "Imunoglobulina anti hepatite B - IGHB" = "20 - imunoglobulina humana anti-hepatite B",
    "Imunoglobulina anti r\u00e1bica - IGRH" = "23 - imunoglobulina humana antir\u00e1bica",
    "Imunoglobulina anti tet\u00e2nica - IGTH" = "1 - imunoglobulina humana antit\u00e9tano",
    "Imunoglobulina anti varicela zoster - IGVZ" = "19 - imunoglobulina humana antivaricela",
    "Influenza H1N1 - H1N1" = "44 - vacina influenza A (H1N1) (inativada, fragmentada)",
    "Influenza ID - FLU ID" = "64 - vacina influenza trivalente ID",
    "Influenza Tetravalente (Quadrivalente) - FLU4V" = "77 - vacina influenza tetravalente",
    "Influenza Trivalente - FLU3V" = "33 - vacina influenza trivalente",
    "Meningoc\u00f3cica A C W Y135 - Meningo ACWY135" = "74 - vacina meningo ACWY",
    "Meningoc\u00f3cica AC - Meningo AC" = "13 - vacina meningo AC",
    "Meningoc\u00f3cica B - MEN B" = "78 - vacina meningo B",
    "Meningoc\u00f3cica B/C - MEN B/C" = "66 - vacina meningo BC",
    "Meningoc\u00f3cica conjugada C - Men Conj C" = "41 - vacina meningo C",
    "Pneumoc\u00f3cica 10V - Pncc10V" = "26 - vacina pneumo 10",
    "Pneumoc\u00f3cica 13V - Pncc13V" = "59 - vacina pneumo 13",
    "Pneumoc\u00f3cica 23V - Pncc23V" = "21 - vacina pneumo 23",
    "Pneumoc\u00f3cica 7V - Pncc7V" = "40 - vacina pneumo 7",
    "Poliomielite inativada - VIP" = "22 - vacina inativada poliomielite",
    "Poliomielite oral (Bivalente) - VOP" = "28 - vacina oral poliomielite bivalente",
    "Raiva em cultivo celular (Embri\u00e3o) - Embri\u00e3o" = "18 - vacina raiva embri\u00e3o de galinha",
    "Raiva em cultivo celular Vero - Vero" = "37 - vacina raiva (inativada)",
    "Rotav\u00edrus pentavalente - ROTA penta" = "65 - vacina rotav\u00edrus pentavalente",
    "Rub\u00e9ola - Rub\u00e9ola" = "71 - vacina rub\u00e9ola",
    "Sarampo - Sarampo" = "70 - vacina sarampo",
    "soro antiaracn\u00eddico - SARC" = "3 - soro antiaracn\u00eddico (Loxosceles, Phoneutria, Tityus)",
    "Soro botr\u00f3pico - SBOTR" = "8 - soro antibotr\u00f3pico",
    "Soro botr\u00f3pico/crot\u00e1lico - SBOCR" = "11 - soro antibotr\u00f3pico e anticrot\u00e1lico",
    "Soro botr\u00f3pico/laqu\u00e9tico - SBOLAQ" = "12 - soro antibotr\u00f3pico e antilaqu\u00e9tico",
    "Soro botul\u00ednico trivalente - SBOTULTRI" = "38 - soro antibotul\u00ednico (trivalente)",
    "Soro crot\u00e1lico - SCROT" = "16 - soro anticrot\u00e1lico",
    "Soro dift\u00e9rico - SAD" = "10 - soro antidift\u00e9rico",
    "Soro elap\u00eddico - SELAP" = "6 - soro antielap\u00eddico",
    "Soro escorpi\u00f4nico - SESCOR" = "4 - soro antiescorpi\u00f4nico",
    "Soro lon\u00f4mico - SLONO" = "32 - soro antilon\u00f4mico",
    "Soro loxosc\u00e9lico - SLOXO" = "31 - soro antiloxosc\u00e9lico",
    "Soro r\u00e1bico humano - SARH" = "7 - soro antir\u00e1bico",
    "Soro tet\u00e2nico - SAT" = "2 - soro antitet\u00e2nico",
    "Tetra Viral - Tetra Viral" = "56 - vacina tetraviral",
    "Tox\u00f3ide Tet\u00e2nico - TT" = "61 - vacina t\u00e9tano",
    "Tr\u00edplice acelular infantil - DTPa" = "47 - vacina DTPa infantil",
    "Tr\u00edplice acelular/poliomelite inativada - DTPaVIP" = "58 - vacina tetra acelular DTPa/VIP",
    "Tr\u00edplice bacteriana - DTP" = "46 - vacina DTP",
    "Tr\u00edplice bacteriana acelular (adulto)- dTpa - dTpa adulto" = "57 - vacina dTpa adulto",
    "Tr\u00edplice viral - SCR" = "24 - vacina tr\u00edplice viral",
    "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue" = "82 - vacina dengue (recombinante e atenuada)",
    "Vacina Herpes-Zoster (recombinante) - VZR" = "101 - vacina herpes-z\u00f3ster (recombinante)",
    "Vacina rotav\u00edrus humano - VRH" = "45 - vacina rotav\u00edrus",
    "Varicela(atenuada) - Varc" = "34 - vacina varicela"
  )

  idx <- match(tolower(stringr::str_squish(product)), tolower(names(legacy_map)))
  if (!is.na(idx)) {
    return(unname(legacy_map[idx]))
  }
  product
}

#' @keywords internal
pni_2024_parse_rows <- function(df, state, year, strategy, product) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(
      state = character(0), year = integer(0), munic_code = numeric(0),
      munic_name = character(0), strategy = character(0), product = character(0),
      month = character(0), dose = character(0), quantity = numeric(0)
    ))
  }

  tibble::tibble(
    state = toupper(state),
    year = as.integer(year),
    munic_code = suppressWarnings(as.numeric(df$munic_code)),
    munic_name = stringr::str_to_lower(stringr::str_remove(df$munic_raw, "^[0-9]+\\s*-\\s*")),
    strategy = strategy,
    product = product,
    month = pni_2024_map_month(df$month_pt),
    dose = pni_2024_map_dose(df$dose_raw),
    quantity = suppressWarnings(as.numeric(df$quantity))
  )
}

pni_2024_fetch_qlik_once <- function(year, state, strategy, product) {
  h <- curl::new_handle(cookiefile = "", followlocation = TRUE, http_version = 2, timeout = 30)
  invisible(curl::curl_fetch_memory(PNI_2024_ENTRY_URL, handle = h))

  r_csrf <- curl::curl_fetch_memory(paste0(PNI_2024_BASE_URL, "/qps/csrftoken"), handle = h)
  csrf <- curl::parse_headers_list(r_csrf$headers)[["qlik-csrf-token"]]
  if (is.null(csrf) || !nzchar(csrf)) {
    stop("Failed to obtain qlik-csrf-token from infoms.saude.gov.br")
  }

  cks <- curl::handle_cookies(h)
  cookie_hdr <- paste0(cks$name, "=", cks$value, collapse = "; ")
  ws_url <- paste0(
    "wss://infoms.saude.gov.br/app/", PNI_2024_APP_ID,
    "?qlik-csrf-token=", curl::curl_escape(csrf)
  )

  ws <- websocket::WebSocket$new(
    ws_url,
    headers = list(
      Cookie = cookie_hdr,
      Origin = PNI_2024_BASE_URL,
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    ),
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
  if (!is.null(ws_err)) stop("WebSocket error: ", ws_err)
  if (!opened) stop("WebSocket connection timed out")

  send_rpc <- function(handle, method, params = list()) {
    id <- next_id
    next_id <<- next_id + 1L
    ws$send(jsonlite::toJSON(list(
      jsonrpc = "2.0", id = id, handle = handle, method = method, params = params
    ), auto_unbox = TRUE))
    for (i in 1:600) {
      later::run_now(0.05)
      if (!is.null(ws_err)) stop("WebSocket error: ", ws_err)
      if (!is.null(responses[[as.character(id)]])) {
        resp <- responses[[as.character(id)]]
        if (!is.null(resp$error)) {
          stop("Qlik RPC error (", method, "): ", resp$error$message)
        }
        return(resp)
      }
    }
    stop("Timeout waiting for Qlik RPC method ", method)
  }

  doc_res <- send_rpc(-1L, "OpenDoc", list(PNI_2024_APP_ID))
  doc_h <- doc_res$result$qReturn$qHandle
  send_rpc(doc_h, "ClearAll", list(TRUE))

  list_field_values <- function(field_name, limit = 200L) {
    obj_h <- send_rpc(doc_h, "CreateSessionObject", list(list(
      qInfo = list(qType = "ListObject"),
      qListObjectDef = list(
        qDef = list(qFieldDefs = list(field_name)),
        qInitialDataFetch = list(list(qTop = 0L, qLeft = 0L, qHeight = limit, qWidth = 1L))
      )
    )))$result$qReturn$qHandle
    lay <- send_rpc(obj_h, "GetLayout", list())
    pages <- lay$result$qLayout$qListObject$qDataPages
    if (length(pages) == 0) return(character(0))
    vapply(pages[[1]]$qMatrix, function(r) r[[1]]$qText, character(1))
  }

  select_exact <- function(field_name, value) {
    fh <- send_rpc(doc_h, "GetField", list(field_name))$result$qReturn$qHandle
    avail <- list_field_values(field_name, 250L)
    match_idx <- which(tolower(stringr::str_squish(avail)) == tolower(stringr::str_squish(as.character(value))))
    if (length(match_idx) == 0) {
      stop(
        "Value '", value, "' not found in SI-PNI field '", field_name, "'. ",
        "Available: ", paste(utils::head(avail, 20), collapse = ", "),
        if (length(avail) > 20) ", ..." else ""
      )
    }
    exact_val <- avail[match_idx[1]]
    res <- send_rpc(fh, "SelectValues", list(list(list(qText = exact_val)), FALSE, FALSE))
    if (!isTRUE(res$result$qReturn)) {
      stop("Failed to select '", exact_val, "' in SI-PNI field '", field_name, "'.")
    }
    exact_val
  }

  qlik_strategy <- pni_2024_map_strategy(strategy)
  qlik_product <- pni_2024_map_product(product)

  select_exact(PNI_2024_FIELDS$year, as.character(year))
  select_exact(PNI_2024_FIELDS$state, toupper(state))
  select_exact(PNI_2024_FIELDS$strategy, qlik_strategy)
  select_exact(PNI_2024_FIELDS$product, qlik_product)

  hc_h <- send_rpc(doc_h, "CreateSessionObject", list(list(
    qInfo = list(qType = "HyperCube"),
    qHyperCubeDef = list(
      qDimensions = list(
        list(qDef = list(qFieldDefs = list(PNI_2024_FIELDS$munic_code)), qNullSuppression = TRUE),
        list(qDef = list(qFieldDefs = list(PNI_2024_FIELDS$munic_name)), qNullSuppression = TRUE),
        list(qDef = list(qFieldDefs = list(PNI_2024_FIELDS$month)), qNullSuppression = TRUE),
        list(qDef = list(qFieldDefs = list(PNI_2024_FIELDS$dose)), qNullSuppression = TRUE)
      ),
      qMeasures = list(list(qDef = list(qDef = "sum(Total_Vacina)"))),
      qSuppressZero = TRUE,
      qSuppressMissing = TRUE,
      qInitialDataFetch = list(list(qTop = 0L, qLeft = 0L, qHeight = 2000L, qWidth = 5L))
    )
  )))$result$qReturn$qHandle

  lay <- send_rpc(hc_h, "GetLayout", list())
  total_rows <- lay$result$qLayout$qHyperCube$qSize$qcy
  if (total_rows == 0L) {
    message("No records found for the specified parameters.")
    return(tibble::tibble())
  }

  pages <- lay$result$qLayout$qHyperCube$qDataPages
  fetched <- length(pages[[1]]$qMatrix)
  all_mats <- list(pages[[1]]$qMatrix)

  while (fetched < total_rows) {
    pg_res <- send_rpc(hc_h, "GetHyperCubeData", list(
      "/qHyperCubeDef",
      list(list(qTop = as.integer(fetched), qLeft = 0L, qHeight = 2000L, qWidth = 5L))
    ))
    mat <- pg_res$result$qDataPages[[1]]$qMatrix
    if (length(mat) == 0) break
    all_mats[[length(all_mats) + 1L]] <- mat
    fetched <- fetched + length(mat)
  }

  flat <- unlist(all_mats, recursive = FALSE)
  tibble::tibble(
    munic_code = vapply(flat, function(r) r[[1]]$qText, character(1)),
    munic_raw = vapply(flat, function(r) r[[2]]$qText, character(1)),
    month_pt = vapply(flat, function(r) r[[3]]$qText, character(1)),
    dose_raw = vapply(flat, function(r) r[[4]]$qText, character(1)),
    quantity = vapply(flat, function(r) {
      if (!is.null(r[[5]]$qNum) && is.numeric(r[[5]]$qNum)) {
        as.numeric(r[[5]]$qNum)
      } else {
        suppressWarnings(as.numeric(r[[5]]$qText))
      }
    }, numeric(1))
  )
}

load_pni_2024 <- function(year, state, strategy, product, max_retries = 3L) {
  message("Retrieving SI-PNI vaccination data (2024+) from DATASUS...")

  last_err <- NULL
  raw_df <- NULL
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(
      pni_2024_fetch_qlik_once(year = year, state = state, strategy = strategy, product = product),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      raw_df <- res
      break
    }
    last_err <- conditionMessage(res)
    # Do not retry deterministic validation errors (missing value in Qlik)
    if (grepl("not found in SI-PNI field", last_err, fixed = TRUE)) {
      stop(last_err, call. = FALSE)
    }
    if (attempt < max_retries) {
      Sys.sleep(1.5 * attempt)
    }
  }

  if (is.null(raw_df)) {
    stop("Failed to retrieve SI-PNI (2024+) data after ", max_retries, " attempts: ", last_err)
  }

  pni_2024_parse_rows(raw_df, state = state, year = year, strategy = strategy, product = product)
}
