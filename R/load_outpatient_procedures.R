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

  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop("Package 'foreign' required.", call. = FALSE)
  }

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Package 'RCurl' required.", call. = FALSE)
  }

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

  ### filtering by suffix
  filenames <- filenames[stringr::str_detect(filenames, paste0("^", param$suffix, "[A-Z]{2}\\d{4}\\.dbc$"))]

  siasus_two_digits <- c("datasus_siasus_ab","datasus_siasus_ad","datasus_siasus_am","datasus_siasus_an",
                         "datasus_siasus_aq","datasus_siasus_ar","datasus_siasus_pa","datasus_siasus_ps")
  siasus_two_digits_alt <- c("datasus_siasus_abo","datasus_siasus_acf","datasus_siasus_atd","datasus_siasus_sad")

  ### Filtering by year

  file_years_yy <- NULL
  file_state <- NULL

  if (param$dataset %in% siasus_two_digits) {
    file_years_yy <- substr(filenames, 5, 6)
  } else if (param$dataset %in% siasus_two_digits_alt) {
    file_years_yy <- substr(filenames, 6, 7)
  }

  filenames <- filenames[file_years_yy %in% param$time_period_yy]

  filenames <- filenames[file_state %in% param$states]

  if (!is.null(file_state) & paste0(param$states, collapse = "") != "all") {
    filenames <- filenames[file_state %in% param$states]
  }

  param$filenames <- filenames

  ### Downloading each file in filenames
  dat <- param$filenames %>%
    purrr::imap(
      function(file_name, iteration) {
        base::message(paste0("Downloading file ", file_name, " (", iteration, " out of ", length(filenames), ")"))

        external_download(
          source = param$source,
          dataset = param$dataset,
          file_name = file_name
        )
      }
    )

  names(dat) <- filenames

  names(dat) <- filenames
  if (param$raw_data) return(dat)

  dat <- dat %>% purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>% dplyr::bind_rows() %>% janitor::clean_names()
  dat <- dat %>% dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))

  tem_zero_a_esquerda <- function(x) any(grepl("^0", enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))))
  coluna_numerica_valida <- function(x) all(grepl("^\\d+$", enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))))

  dat <- dat %>% dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ {
    if (!tem_zero_a_esquerda(.x) && coluna_numerica_valida(.x)) suppressWarnings(as.numeric(.x)) else .x
  }))

  dic <- load_dictionary(param$dataset)
  var_names <- if (param$language == "pt") dic$name_pt else dic$name_eng
  names(var_names) <- dic$var_code

  dat <- dat %>% dplyr::rename_with(~ dplyr::recode(., !!!var_names))

  return(dat)
}
