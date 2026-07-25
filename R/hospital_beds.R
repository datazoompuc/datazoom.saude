#' Load monthly data on hospital beds from Brazil's National Health Facilities Registry
#'
#' Retrieves and processes structured microdata on hospital beds from the 'CNES' (Cadastro Nacional de Estabelecimentos de Saude),
#' which is part of the 'DATASUS' system, maintained by the Brazilian Ministry of Health. This dataset includes
#' information on the availability and distribution of hospital beds in Brazil.
#'
#' The data is sourced from the 'CNES-LT' subsystem (Leitos - Beds), which provides monthly information
#' on beds by establishment and type, and can be filtered by state and time period.
#'
#' File downloads are performed via the official DATASUS FTP server. Users can choose to retrieve
#' raw or pre-processed data, and label variables in English or Portuguese.
#'
#' @param time_period A character vector of years (e.g., \code{c("2019", "2020")}) for which data should be loaded.
#' @param states A character vector of state abbreviations (e.g., \code{c("RJ", "SP")}) to filter the data.
#'        Use \code{"all"} to include all states. Defaults to \code{"all"}.
#' @param raw_data Logical. If \code{TRUE}, returns a list of raw data frames (one per file). If \code{FALSE},
#'        returns a cleaned and labeled data frame. Default is \code{FALSE}.
#' @param language Character string indicating the variable label language. Options are \code{"pt"} for Portuguese
#'        or \code{"eng"} for English. Default is \code{"eng"}.
#'
#' @return A tibble (if \code{raw_data = FALSE}) containing harmonized monthly hospital bed data from Brazil,
#'         or a list of raw data frames (if \code{raw_data = TRUE}).
#'
#' @details
#' This function downloads and processes data from the CNES-LT subsystem, which is part of the
#' Brazilian National Health Facilities Registry. It uses the DATASUS FTP server and handles compressed
#' data in DBF format, using packages such as \code{foreign} and \code{RCurl}.
#'
#' The function labels variables using a built-in dictionary and offers harmonized column names
#' depending on the selected language. A filter by state and year is applied to reduce file size and processing time.
#'
#' @note
#' The \code{foreign} and \code{RCurl} packages must be installed to use this function. They are listed in \code{Suggests}
#' to avoid installing them by default.
#'
#' @examples
#' \donttest{
#' # Load beds data for RO and AM in 2020
#' beds <- load_hospital_beds(time_period = "2020",
#'                            states = c("RO", "AM"),
#'                            raw_data = FALSE,
#'                            language = "eng")
#'
#' # Load raw data for AC in 2019 and 2020
#' raw <- load_hospital_beds(time_period = c("2019", "2020"),
#'                           states = "AC",
#'                           raw_data = TRUE)
#' }
#'
#' @export

load_hospital_beds <- function(time_period,
                               states = "all",
                               raw_data = FALSE,
                               language = "eng") {

  # Check required packages
  if (!requireNamespace("foreign", quietly = TRUE)) stop("Package 'foreign' required.")
  if (!requireNamespace("RCurl", quietly = TRUE)) stop("Package 'RCurl' required.")

  # Declare global variables to avoid check notes

  . <- file_name <- link <- name_eng <- label_eng <- dataset <- NULL
  name_pt <- label_pt <- var_code <- setNames <- NULL
  regsaude <- rename <- where <- NULL

  # Prepare parameters
  param <- list()
  param$source <- "datasus"
  param$dataset <- "datasus_cnes_lt"
  param$raw_data <- raw_data
  param$language <- language

  param$time_period <- as.character(time_period)

  param$states <- if (length(states) == 1 && tolower(states) == "all") {
    param$states <- "ALL"
  } else {
    param$states <- toupper(states)
  }

  param$filenames <- NULL

  # Check if dataset and time_period are valid
  check_params(param)

  ###########################
  ## Downloading CNES Data ##
  ###########################

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

  # Filter by requested states
  file_state <- substr(filenames, 3, 4)   # state code (UF)
  if (paste0(param$states, collapse = "") != "ALL") {
    filenames <- filenames[file_state %in% param$states]
  }

  # Filter by requested years
  file_year <- substr(filenames, 5, 6)   # year (2 digits)
  file_year <- as.integer(file_year)
  file_year <- ifelse(file_year >= 90, 1900 + file_year, 2000 + file_year)
  filenames <- filenames[file_year %in% param$time_period]

  param$filenames <- filenames

  # Download each file
  dat <- purrr::imap(filenames, function(file_name, i) {
    message(paste0("Downloading file ", file_name, " (", i, " of ", length(filenames), ")"))
    external_download(source = param$source, dataset = param$dataset, file_name = file_name)
  })
  names(dat) <- filenames

  dat <- purrr::imap(dat, ~ {
    tibble::as_tibble(.x) %>%
      dplyr::mutate(file_name = .y)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(where(is.character),
                    ~ iconv(.x, from = "latin1", to = "UTF-8", sub = ""))
    )


  # Return raw data if requested
  if (raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  # Clean and label data
  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      year = as.numeric(paste0("20", substr(file_name, 5, 6))),
      month = as.numeric(substr(file_name, 7, 8))
    )

  if ("regsaude" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        regsaude = stringr::str_trim(regsaude),
        regsaude = dplyr::case_when(
          is.na(regsaude) | regsaude == "" ~ NA_character_,
          stringr::str_detect(regsaude, "(?i)AP|,|\\.") ~ regsaude,
          stringr::str_detect(regsaude, "^\\d+$") ~ stringr::str_pad(regsaude, width = 4, pad = "0"),
          TRUE ~ regsaude
        )
      )
  }


  ###############
  ## Labelling ##
  ###############

  # Load dictionary for labeling
  dic <- load_dictionary(param$dataset)
  if (language == "pt") {
    labels_lookup <- setNames(dic$label_pt, dic$var_code)
    var_names <- setNames(dic$name_pt, dic$var_code)
  } else {
    labels_lookup <- setNames(dic$label_eng, dic$var_code)
    var_names <- setNames(dic$name_eng, dic$var_code)
  }

  # Apply labels
  row_numbers <- match(names(dat), names(labels_lookup))
  labels_full <- character(ncol(dat))
  labels_full[!is.na(row_numbers)] <- labels_lookup[!is.na(row_numbers)]
  Hmisc::label(dat) <- as.list(labels_full)

  # Harmonize variable names
  dat <- dat %>% tibble::as_tibble() %>%
    dplyr::rename_with(~ dplyr::recode(., !!!var_names))

  #Set order

  if (param$language == "pt") {
    dat <- dat %>%
      dplyr::select(dplyr::any_of(
        c(
          "competencia",
          "regsaude",
          "micr_reg",
          "distrsan",
          "distradm",
          "codufmun",
          "cnes",
          "nat_jur",
          "cpf_cnpj",
          "cnpj_man",
          "tpgestao",
          "pf_pj",
          "niv_dep",
          "esfera_a",
          "natureza",
          "atividad",
          "retencao",
          "clientel",
          "tp_unid",
          "turno_atendimento",
          "niv_hier",
          "terceiro",
          "tipo_leito",
          "cod_leito",
          "n_leitos_existentes",
          "n_leitos_sus",
          "n_leitos_nao_sus",
          "qt_contr"
        )
      ),-dplyr::any_of("file_name"))
  } else{
    dat <- dat %>%
      dplyr::select(dplyr::any_of(
        c(
          "competence",
          "regsaude",
          "micr_reg",
          "distrsan",
          "distradm",
          "codufmun",
          "cnes",
          "nat_jur",
          "cpf_cnpj",
          "cnpj_man",
          "tpgestao",
          "pf_pj",
          "niv_dep",
          "esfera_a",
          "natureza",
          "atividad",
          "retencao",
          "clientel",
          "tp_unid",
          "turno_atendimento",
          "niv_hier",
          "terceito",
          "tipo_leito",
          "cod_leito",
          "n_existing_beds",
          "n_beds_sus",
          "n_beds_not_sus",
          "qt_contr"
        )
      ),-dplyr::any_of("file_name"))
  }

  return(dat)
}
