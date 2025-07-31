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
#' \dontrun{
#' # Load beds data for RJ and SP in 2020 and 2021
#' beds <- load_hospital_beds(time_period = c("2020", "2021"), states = c("RJ", "SP"))
#'
#' # Load raw data for all states in 2019
#' raw <- load_hospital_beds(time_period = "2019", raw_data = TRUE)
#' }
#'
#' @export

load_hospital_beds <- function(time_period,
                               states = "all",
                               raw_data = FALSE,
                               language = "eng") {

  # Checking for foreign package (in Suggests)

  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop(
      "Package \"foreign\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Checking for RCurl package (in Suggests)

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop(
      "Package \"RCurl\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Declare global variables to avoid check notes

  . <- file_name <- dataset <- link <- name_eng <- label_eng <- NULL
   name_pt <- label_pt <- var_code <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$source <- "datasus"
  param$dataset <- "datasus_cnes_lt"
  param$raw_data <- raw_data
  param$language <- language

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

  # Auxiliary parameters to be passed to external_download

  param$filenames <- NULL

  # check if dataset and time_period are valid

  check_params(param)

  ##############################
  ## Downloading CNES_lt Data ##
  ##############################

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

  ### Filtering by year

  file_years <- NULL
  file_years_yy <- NULL

  file_years_yy <- filenames %>%
    substr(5, 6)

  if (!is.null(file_years_yy)) {
    filenames <- filenames[file_years_yy %in% param$time_period_yy]
  }

  ### Filtering for chosen states when possible

  file_state <- NULL

  file_state <- filenames %>%
    substr(3, 4)

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


  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()

  dat <- dat %>%
    dplyr::mutate(
      year = as.numeric(paste0("20", substr(file_name, 5, 6))),
      month = as.numeric(substr(file_name, 7, 8))
    )

  ###############
  ## Labelling ##
  ###############

  dic <- load_dictionary(param$dataset)

  row_numbers <- match(names(dat), dic$var_code)

  if (param$language == "pt") {
    dic <- dic %>%
      dplyr::select(label_pt)
  }
  if (param$language == "eng") {
    dic <- dic %>%
      dplyr::select(label_eng)
  }

  labels <- dic %>%
    dplyr::slice(row_numbers) %>%
    unlist()

  # Making sure 'labels' is the same length as the number of columns

  labels_full <- character(length = ncol(dat))

  labels_full[which(!is.na(row_numbers))] <- labels

  Hmisc::label(dat) <- as.list(labels_full)

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat_mod <- dat %>%
    tibble::as_tibble()

  dic <- load_dictionary(param$dataset)

  if (param$language == "pt") {
    var_names <- dic$name_pt
  }
  if (param$language == "eng") {
    var_names <- dic$name_eng
  }

  names(var_names) <- dic$var_code

  dat_mod <- dat_mod %>%
    dplyr::rename_with(
      ~ dplyr::recode(., !!!var_names)
    )

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
