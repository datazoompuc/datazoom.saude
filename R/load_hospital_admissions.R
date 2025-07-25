#' Loads hospital admission data from Brazil provided by DATASUS (SIHSUS)
#'
#' This function downloads and organizes data from SIHSUS (Hospital Information System),
#' part of DATASUS, used in public health and hospital care analyses.
#'
#' @param dataset A string indicating the type of SIHSUS dataset to download. Accepted values are:
#' "RD", "SP", "RJ", or "ER". See the 'Details' section for explanations.
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded.
#' For example, `2020` or `2015:2020`.
#' @param states A string or vector of strings indicating the Brazilian state(s) for which the data should be downloaded.
#' Use `"all"` to download data for the entire country. For specific states, use abbreviations like `"SP"`, `"RJ"`, or `c("SP", "RJ")`.
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default),
#' returns a cleaned and standardized version of the dataset.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#'
#' @return A data frame containing hospital admission records from SIHSUS for the specified period and states.
#' @details
#' SIHSUS provides several datasets related to hospital admissions in Brazil:
#'
#' \describe{
#'   \item{RD – Reduced AIH (Hospital Admission Authorization)}{
#'   A simplified database with the main information from approved and processed AIHs.
#'   It is the most commonly used dataset for statistical and epidemiological analyses, including
#'   data on the main procedure, diagnoses, and total values of each admission.}
#'
#'   \item{SP – Professional Services}{
#'   A stratified dataset containing details about services provided during the hospital stay,
#'   such as medical procedures, professional identification (CBO/CNS), and values related to
#'   professional and hospital services.}
#'
#'   \item{RJ – Rejected AIHs}{
#'   Contains rejected AIHs and summarizes the reasons for rejection. It is useful for analyzing
#'   the volume and impact of rejected records but does not include detailed information on each rejection.}
#'
#'   \item{ER – Rejected AIHs with Error Code}{
#'   Includes AIHs rejected due to inconsistencies identified during processing. These records contain
#'   specific error codes indicating why the rejection occurred (e.g., patient data inconsistency,
#'   procedure incompatibility).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_hospital_admissions(dataset = "SP",
#'                          time_period = 2020,
#'                          states = "AC")
#' }

load_hospital_admissions <- function(dataset,
                        time_period,
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

  # Argument checks (basic)
  if (!is.numeric(time_period)) {
    stop("time_period must be a numeric value or vector of years.")
  }

  if (!is.character(states)) {
    stop("states must be a character vector (e.g., 'SP' or c('SP', 'RJ')). By default, it is all.")
  }

  if (!is.logical(raw_data)) {
    stop("raw_data must be TRUE or FALSE. By default, it is FALSE.")
  }

  if (!language %in% c("eng", "pt")) {
    stop("the language must be 'eng' or 'pt'. By default it is 'eng'.")
  }

  # Create param list with specific parameters for SINASC
  param <- list()

  param$source <- "datasus"
  param$dataset <- paste0("datasus_sih_",dataset)
  param$raw_data <- raw_data
  param$language <- language
  param$suffix <- toupper(dataset)

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

  #############################
  ## Downloading SIHSUS Data ##
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

  filenames <- filenames[stringr::str_starts(filenames, param$suffix)]

  ### Filtering by year
  file_years_yy <- NULL
  file_years_yy <- filenames %>%
    substr(5, 6)

  filenames <- filenames[file_years_yy %in% param$time_period_yy]

  ### Filtering for chosen states
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

  ## Return Raw Data if requested
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

  dat_mod <- dat %>%
    dplyr::select(tidyselect::where(~ !(all(is.na(.)) || all(. == 0, na.rm = TRUE)))) %>% # Remove colunas que so possuem 0 e NA
    tibble::as_tibble()

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

  dat_mod <- dat %>% tibble::as_tibble()

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
