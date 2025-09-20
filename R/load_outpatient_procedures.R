#' Loads outpatient procedure data from Brazil provided by 'SIASUS' (Ambulatory Information System)
#'
#' This function downloads and organizes public health data from 'SIASUS' – the Ambulatory Information System from Brazil's national health database, 'DATASUS'.
#' It contains records of outpatient medical procedures performed throughout the country.
#'
#' @param dataset A string indicating the type of 'SIASUS' dataset to download. Accepted values include:
#' `"bariatric_surgery_follow_up"`, `"diverse_reports"`, `"medicines"`, `"nephrology"`, `"chemotherapy"`, `"radiotherapy"`, `"ambulatory_production"`, `"psychosocial"`, `"post_bariatric_surgery_follow_up"`, `"fistula_confection"`, `"dialytic_treatment"`, `"home_care"`.
#' See the 'Details' section for descriptions.
#'
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded.
#' For example, `2020` or `2015:2020`.
#' @param states A string or vector of strings indicating the Brazilian state(s) for which the data should be downloaded.
#' Use `"all"` to download data for the entire country. For specific states, use abbreviations like `"SP"`, `"RJ"`, or `c("SP", "RJ")`.
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default),
#' returns a cleaned and standardized version of the dataset.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#'
#' @return A data frame containing outpatient procedure records from SIASUS for the specified period and states.
#'
#' @details
#' SIASUS provides multiple datasets that cover different aspects of outpatient care in Brazil:
#'
#' \describe{
#'   \item{bariatric_surgery_follow_up}{
#'   General preventive and primary care procedures, such as check-ups and vaccinations.}
#'
#'   \item{diverse_reports}{
#'   Outpatient dental procedures including extractions, restorations, and preventive services.}
#'
#'   \item{medicines}{
#'   Higher complexity procedures including specialized consultations and diagnostic exams.}
#'
#'   \item{nephrology}{
#'   Procedures related to early-life screening tests, including metabolic and sensory testing.}
#'
#'   \item{chemotherapy}{
#'   Outpatient procedures provided through structured home care programs.}
#'
#'   \item{radiotherapy}{
#'   Procedures focused on physical, cognitive, and functional rehabilitation.}
#'
#'   \item{ambulatory_production}{
#'   High-cost procedures that require prior authorization, such as cancer treatment or dialysis.}
#'
#'   \item{psychosocial}{
#'   Aggregated or simplified records of outpatient procedures with limited detail.}
#'
#'   \item{post_bariatric_surgery_follow_up}{
#'   Records from specialized dental centers providing oral health care.}
#'
#'   \item{fistula_confection}{
#'   Data on medication dispensing and pharmaceutical consultations.}
#'
#'   \item{dialytic_treatment}{
#'   Procedures related to therapies such as chemotherapy and radiotherapy.}
#'
#'   \item{home_care}{
#'   Home-based specialized care such as oxygen therapy and related services.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_outpatient_procedures(dataset = "ambulatory_production",
#'                            time_period = 2021,
#'                            states = c("SP", "RJ"))
#'
#' load_outpatient_procedures(dataset = "diverse_reports",
#'                            time_period = 2016,
#'                            states = "PI",
#'                            language = "pt")
#' }
load_outpatient_procedures <- function(dataset,
                                       time_period,
                                       states = "all",
                                       raw_data = FALSE,
                                       language = "eng") {

  if (!requireNamespace("foreign", quietly = TRUE)){
    stop("Package 'foreign' required.", call. = FALSE)
  }

  if (!requireNamespace("RCurl", quietly = TRUE)){
    stop("Package 'RCurl' required.", call. = FALSE)
  }

  # Declare global variables to avoid check notes

  . <- file_name <- link <- name_eng <- label_eng <- NULL
  name_pt <- label_pt <- var_code <- NULL

  ap_mvm <- ap_cmp <- ap_dtinic <- ap_dtfim <- ap_dtocor <- NULL
  ap_dtsolic <- ap_dtaut <- ap_vl_ap <- ap_nuidade <- ap_mndif <- NULL
  ap_tpapac <- ap_motsai <- NULL

  # Map dataset names to SIASUS codes
  dataset_map <- c(
    "bariatric_surgery_follow_up"     = "ab",
    "diverse_reports"                 = "ad",
    "medicines"                       = "am",
    "nephrology"                      = "an",
    "chemotherapy"                    = "aq",
    "radiotherapy"                    = "ar",
    "ambulatory_production"           = "pa",
    "psychosocial"                    = "ps",
    "post_bariatric_surgery_follow_up"= "abo",
    "fistula_confection"              = "acf",
    "dialytic_treatment"              = "atd",
    "home_care"                       = "sad"
  )

  # Checking if the entered dataset is correct

  normalized_dataset <- tolower(dataset)

  if (!normalized_dataset %in% names(dataset_map)){
    stop("Invalid dataset name. Use one of: ", paste(names(dataset_map), collapse = ", "))
  }

  # Create param list with specific parameters for SIASUS

  param <- list()

  param$source   <- "datasus_siasus"
  param$dataset  <- paste0("datasus_siasus_", dataset_map[normalized_dataset])
  param$origin_dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$suffix   <- toupper(dataset_map[[normalized_dataset]])

  # Auxiliary parameters to be passed to external_download

  param$time_period    <- time_period
  param$time_period_yy <- substr(param$time_period, 3,4)

  param$states <- if(length(states) == 1 && tolower(states) == "all") {
    "all"
  } else {
      toupper(states)}


  param$filenames <- NULL

  # check if dataset and time_period are valid

  check_params(param)

  #############################
  ## Downloading SIASUS Data ##
  #############################

  dat_url <- datasets_link()

  url <- dat_url %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    base::unlist() %>%
    as.character()

  filenames <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    stringr::str_split("\r*\n") %>%
    unlist()

  ### filtering by suffix

  filenames <- filenames[stringr::str_detect(filenames, paste0("^", param$suffix, "[A-Z]{2}\\d{4}\\.dbc$"))]

  ### filtering by states and years
  siasus_two_digits <- c("datasus_siasus_ab","datasus_siasus_ad","datasus_siasus_am","datasus_siasus_an",
                         "datasus_siasus_aq","datasus_siasus_ar","datasus_siasus_pa","datasus_siasus_ps")
  siasus_three_digits <- c("datasus_siasus_abo","datasus_siasus_acf","datasus_siasus_atd","datasus_siasus_sad")

  file_years_yy <- NULL
  file_state <- NULL

  if (param$dataset %in% siasus_two_digits) {
    file_state <- filenames %>% substr(3, 4)
    file_years_yy <- substr(filenames, 5, 6)

  } else if (param$dataset %in% siasus_three_digits) {
    file_state <- filenames %>% substr(4, 5)
    file_years_yy <- substr(filenames, 6, 7)
  }

  # Criar um índice lógico combinando os dois filtros
  idx <- file_years_yy %in% param$time_period_yy & file_state %in% param$states

  filenames   <- filenames[idx]
  file_state  <- file_state[idx]
  file_years_yy <- file_years_yy[idx]

  filenames <- filenames[file_years_yy %in% param$time_period_yy]

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

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows()

  ## Return Raw Data if requested
  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names()

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.factor), as.character)
    )

  # Formatting data

  dat <- dat %>%
    dplyr::mutate(
      ap_mvm = lubridate::ym(as.character(ap_mvm)),
      ap_cmp = lubridate::ym(as.character(ap_cmp)),
      ap_dtinic = lubridate::ymd(as.character(ap_dtinic)),
      ap_dtfim = lubridate::ymd(as.character(ap_dtfim)),
      ap_dtocor = lubridate::ymd(as.character(ap_dtocor)),
      ap_dtsolic = lubridate::ymd(as.character(ap_dtsolic)),
      ap_dtaut = lubridate::ymd(as.character(ap_dtaut)),

      ap_vl_ap = as.numeric(ap_vl_ap),
      ap_nuidade = as.numeric(ap_nuidade),
      ap_mndif = as.numeric(ap_mndif),
      ap_tpapac = as.numeric(ap_tpapac),
      ap_motsai = as.numeric(ap_motsai)
    )

  tem_zero_a_esquerda <- function(x) {
    # Força o encoding como latin1 → UTF-8 para evitar warnings
    x <- enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))
    any(grepl("^0", x))
  }

  coluna_numerica_valida <- function(x) {
    x <- enc2utf8(iconv(x, from = "latin1", to = "UTF-8"))
    all(grepl("^\\d+$", x))
  }

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        ~ {
          if (!tem_zero_a_esquerda(.x) && coluna_numerica_valida(.x)) {
            suppressWarnings(as.numeric(.x))
          } else {
            .x
          }
        }
      )
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
