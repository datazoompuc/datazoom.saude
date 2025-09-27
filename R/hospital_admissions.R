#' Loads hospital admission data from Brazil provided by DATASUS (SIHSUS)
#'
#' This function downloads and organizes data from SIHSUS (Hospital Information System),
#' part of DATASUS, used in public health and hospital care analyses.
#'
#' @param dataset A string indicating the type of SIHSUS dataset to download. Accepted values are:
#' `"reduced_aih"`, `"professional_services"`, `"rejected_aih"`, or `"rejected_aih_error"`.
#' See the 'Details' section for explanations.
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
#'   \item{reduced_aih (RD) - Reduced AIH (Hospital Admission Authorization)}{
#'   A simplified database with the main information from approved and processed AIHs.
#'   It is the most commonly used dataset for statistical and epidemiological analyses, including
#'   data on the main procedure, diagnoses, and total values of each admission.}
#'
#'   \item{professional_services (SP) - Professional Services}{
#'   A stratified dataset containing details about services provided during the hospital stay,
#'   such as medical procedures, professional identification (CBO/CNS), and values related to
#'   professional and hospital services.}
#'
#'   \item{rejected_aih (RJ) - Rejected AIHs}{
#'   Contains rejected AIHs and summarizes the reasons for rejection. It is useful for analyzing
#'   the volume and impact of rejected records but does not include detailed information on each rejection.}
#'
#'   \item{rejected_aih_error (ER) - Rejected AIHs with Error Code}{
#'   Includes AIHs rejected due to inconsistencies identified during processing. These records contain
#'   specific error codes indicating why the rejection occurred (e.g., patient data inconsistency,
#'   procedure incompatibility).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_hospital_admissions(dataset = "professional_services",
#'                          time_period = 2020,
#'                          states = "AC")
#' }
load_hospital_admissions <- function(dataset,
                                     time_period,
                                     states = "all",
                                     raw_data = FALSE,
                                     language = "eng") {

  # Check if foreign package is installed
  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop(
      "Package \"foreign\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Check if RCurl package is installed
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop(
      "Package \"RCurl\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Basic argument checks
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

  # Declare global variables to avoid check notes
  . <- file_name <- link <- name_eng <- label_eng <- NULL
  name_pt <- label_pt <- var_code <- NULL

  # Map dataset names to SIHSUS codes
  dataset_map <- c(
    "reduced_aih" = "rd",
    "professional_services" = "sp",
    "rejected_aih" = "rj",
    "rejected_aih_error" = "er"
  )
  normalized_dataset <- tolower(dataset)

  if (!normalized_dataset %in% names(dataset_map)) {
    stop("Invalid dataset name. Use one of: ", paste(names(dataset_map), collapse = ", "))
  }

  # Create param list with specific parameters for SIHSUS
  param <- list()

  param$source <- "datasus_sih"
  param$dataset <- paste0("datasus_sih_", dataset_map[normalized_dataset])
  param$raw_data <- raw_data
  param$language <- language
  param$suffix <- toupper(dataset_map[normalized_dataset])

  param$time_period <- time_period

  param$states <- if (length(states) == 1 && tolower(states) == "all") {
    param$states <- "ALL"
  } else {
    param$states <- toupper(states)
  }

  param$filenames <- NULL

  # Check if dataset and time_period are valid
  check_params(param)

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

  # Filter by dataset suffix (RD, SP, RJ, ER)
  filenames <- filenames[stringr::str_starts(filenames, param$suffix)]

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
  dat <- param$filenames %>%
    purrr::imap(
      function(file_name, iteration) {
        base::message(paste0("Downloading file ", file_name, " (", iteration, " out of ", length(param$filenames), ")"))
        external_download(
          source = param$source,
          dataset = param$dataset,
          file_name = file_name
        )
      }
    )

  names(dat) <- filenames

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows()

  # Return raw data if requested
  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names() %>%
    dplyr::select(tidyselect::where(~ !(all(is.na(.)) || all(. == 0, na.rm = TRUE)))) # Remove columns with only 0 or NA

  ###############
  ## Labelling ##
  ###############

  dic <- load_dictionary(param$dataset)

  # Select labels based on language
  if (param$language == "pt") {
    labels_lookup <- dic$label_pt
  } else {
    labels_lookup <- dic$label_eng
  }

  names(labels_lookup) <- dic$var_code
  labels_full <- labels_lookup[match(names(dat), names(labels_lookup))]
  labels_full[is.na(labels_full)] <- ""
  Hmisc::label(dat) <- as.list(labels_full)

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat_mod <- dat %>%
    tibble::as_tibble()

  # Select variable names based on language
  if (param$language == "pt") {
    var_names_lookup <- dic$name_pt
  } else {
    var_names_lookup <- dic$name_eng
  }

  names(var_names_lookup) <- dic$var_code

  dat_mod <- dat_mod %>%
    dplyr::rename_with(
      ~ dplyr::recode(., !!!var_names_lookup)
    )

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
