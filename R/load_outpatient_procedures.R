#' @title Load Outpatient Procedures (SIASUS)
#'
#' @description Downloads and processes outpatient procedure data from DATASUS SIASUS.
#'
#' @param dataset A dataset name from SIASUS, e.g. "datasus_siasus_ab", "datasus_siasus_pa", etc.
#' @param time_period A numeric year or vector of years, e.g. 2015:2020.
#' @param states A vector of state abbreviations or "all" (default).
#' @param raw_data Logical. If TRUE, returns the raw list. If FALSE (default), returns a cleaned tibble.
#' @param language Language for labels and column names: "eng" (default) or "pt".
#'
#' @return A tibble.
#' @export
load_outpatient_procedures <- function(dataset,
                                       time_period,
                                       states = "all",
                                       raw_data = FALSE,
                                       language = "eng") {

  if (!requireNamespace("foreign", quietly = TRUE)) stop("Package 'foreign' required.", call. = FALSE)
  if (!requireNamespace("RCurl", quietly = TRUE)) stop("Package 'RCurl' required.", call. = FALSE)

  . <- abbrev_state <- code_muni <- code_muni_6 <- code_state <- codmunocor <- NULL
  codufmun <- file_name <- value <- year <- month <- NULL

  # Create param list with specific parameters for SIASUS
  param <- list()

  param$source <- "datasus"
  param$dataset <- paste0("datasus_siasus_",dataset)
  param$raw_data <- raw_data
  param$language <- language
  param$suffix <- toupper(dataset)

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

  #############################
  ## Downloading SIASUS Data ##
  #############################

  # Get dataset source URL
  dat_url <- datasets_link()

  url <- dat_url %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    base::unlist() %>%
    as.character()

  # Use RCurl to extract the names of all files stored in the server
  filenames <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    stringr::str_split("\r*\n") %>%
    unlist()

  siasus_two_digits <- c("datasus_siasus_ab","datasus_siasus_ad","datasus_siasus_am","datasus_siasus_an",
                         "datasus_siasus_aq","datasus_siasus_ar","datasus_siasus_pa","datasus_siasus_ps")
  siasus_two_digits_alt <- c("datasus_siasus_abo","datasus_siasus_acf","datasus_siasus_atd","datasus_siasus_sad")

  if (param$dataset %in% siasus_two_digits) {
    file_years_yy <- substr(filenames, 5, 6)
    file_state <- substr(filenames, 3, 4)
  } else if (param$dataset %in% siasus_two_digits_alt) {
    file_years_yy <- substr(filenames, 6, 7)
    file_state <- substr(filenames, 4, 5)
  } else {
    stop("Unsupported SIASUS dataset.")
  }

  filenames <- filenames[file_years_yy %in% param$time_period_yy]
  if (paste0(param$states, collapse = "") != "all") {
    filenames <- filenames[file_state %in% param$states]
  }

  suffix <- stringr::str_remove(param$dataset, "datasus_siasus_") %>% toupper()
  filenames <- filenames[stringr::str_starts(filenames, suffix)]
  param$filenames <- filenames

  dat <- param$filenames %>% purrr::imap(function(file_name, iteration) {
    message(paste0("Downloading ", file_name, " (", iteration, "/", length(filenames), ")"))
    external_download(
      source = param$source,
      dataset = param$dataset,
      skip_rows = NULL,
      file_name = file_name
    )
  })

  names(dat) <- filenames
  if (param$raw_data) return(dat)

  dat <- dat %>% purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>% dplyr::bind_rows() %>% janitor::clean_names()
  dat <- dat %>% dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))

  tem_zero_a_esquerda <- function(x) any(grepl("^0", enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))))
  coluna_numerica_valida <- function(x) all(grepl("^\\d+$", enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))))

  dat <- dat %>% dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ {
    if (!tem_zero_a_esquerda(.x) && coluna_numerica_valida(.x)) suppressWarnings(as.numeric(.x)) else .x
  }))

  geo <- datazoom.saude::municipalities %>%
    dplyr::select(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
    dplyr::mutate(code_muni_6 = as.integer(code_muni / 10))

  suffix <- if (param$dataset == "datasus_siasus_pa") {
    "pa_ufmun"
  } else if (param$dataset %in% c("datasus_siasus_ps", "datasus_siasus_sad")) {
    "ufmun"
  } else {
    "ap_ufmun"
  }

  dat <- dat %>% dplyr::left_join(geo, by = stats::setNames("code_muni_6", suffix)) %>%
    dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
    tibble::as_tibble()

  dic <- load_dictionary(param$dataset)
  var_names <- if (param$language == "pt") dic$name_pt else dic$name_eng
  names(var_names) <- dic$var_code

  dat <- dat %>% dplyr::rename_with(~ dplyr::recode(., !!!var_names))

  return(dat)
}
