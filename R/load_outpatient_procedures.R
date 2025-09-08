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
#' @examples
#' \dontrun{
#' load_outpatient_procedures(dataset = "ambulatory_production",
#'                            time_period = 2021,
#'                            states = c("SP", "RJ"))
#' }
load_outpatient_procedures <- function(dataset,
                                       time_period,
                                       states = "all",
                                       raw_data = FALSE,
                                       language = "eng") {

  ####################
  ## Check Packages ##
  ####################
  if (!requireNamespace("foreign", quietly = TRUE)) stop("Package 'foreign' required.", call. = FALSE)
  if (!requireNamespace("RCurl", quietly = TRUE)) stop("Package 'RCurl' required.", call. = FALSE)

  ####################
  ## Declare Globals ##
  ####################
  . <- file_name <- link <- name_eng <- label_eng <- NULL
  name_pt <- label_pt <- var_code <- NULL

  ####################
  ## Normalize Dataset ##
  ####################
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

  normalized_dataset <- tolower(dataset)
  if (!normalized_dataset %in% names(dataset_map)) stop(
    "Invalid dataset name. Use one of: ", paste(names(dataset_map), collapse = ", ")
  )

  param <- list()
  param$source   <- "datasus"
  param$dataset  <- paste0("datasus_siasus_", dataset_map[normalized_dataset])
  param$raw_data <- raw_data
  param$language <- language
  param$suffix   <- toupper(dataset_map[normalized_dataset])

  ####################
  ## Normalize States and Years ##
  ####################
  param$time_period    <- as.character(time_period)
  param$time_period_yy <- substr(param$time_period, 3,4)

  param$states <- if(length(states) == 1 && tolower(states) == "all") "all" else toupper(states)
  param$filenames <- NULL

  ####################
  ## Check Parameters ##
  ####################
  check_params(param)

  ####################
  ## Download File List ##
  ####################
  dat_url <- datasets_link()
  url <- dat_url %>% dplyr::filter(dataset == param$dataset) %>% dplyr::pull(link) %>% as.character()
  if(length(url) == 0) stop("Dataset URL not found in datasets_link().")

  filenames_all <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    stringr::str_split("\r*\n") %>%
    unlist()

  ####################
  ## Filter Files by Year and State ##
  ####################
  years_filter  <- sprintf("%02d", as.numeric(param$time_period_yy))
  states_filter <- if(param$states[1] == "all") unique(substr(filenames_all,3,4)) else param$states
  dataset_code  <- toupper(dataset_map[normalized_dataset])

  filenames <- filenames_all[
    stringr::str_starts(filenames_all, dataset_code) &
      substr(filenames_all, nchar(dataset_code) + 1, nchar(dataset_code) + 2) %in% states_filter &
      substr(filenames_all, nchar(dataset_code) + 3, nchar(dataset_code) + 4) %in% years_filter
  ]

  if(length(filenames) == 0) stop("No files found for the specified dataset, time period, and states.")
  param$filenames <- filenames

  ####################
  ## Download Files ##
  ####################
  dat <- param$filenames %>% purrr::imap(function(file_name, iteration){
    message(paste0("Downloading file ", file_name, " (", iteration, " of ", length(filenames), ")"))
    external_download(source=param$source, dataset=param$dataset, file_name=file_name)
  }) %>% purrr::imap(~dplyr::mutate(.x, file_name=.y)) %>% dplyr::bind_rows()
  names(dat) <- filenames

  ####################
  ## Return Raw Data ##
  ####################
  if(param$raw_data) return(dat)

  ####################
  ## Clean Columns ##
  ####################
  dat <- dat %>% janitor::clean_names()
  dat <- dat %>% dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))
  dat <- dat %>% dplyr::mutate(
    dplyr::across(tidyselect::where(is.character), ~{
      x <- enc2utf8(iconv(.x, from="latin1", to="UTF-8"))
      if(all(grepl("^\\d+$", x)) & !any(grepl("^0", x))) suppressWarnings(as.numeric(x)) else .x
    })
  )

  ####################
  ## Load Dictionary ##
  ####################
  dic <- load_dictionary(param$dataset)
  if(param$language=="pt") {
    labels_lookup    <- dic$label_pt
    var_names_lookup <- dic$name_pt
  } else {
    labels_lookup    <- dic$label_eng
    var_names_lookup <- dic$name_eng
  }
  names(labels_lookup)    <- dic$var_code
  names(var_names_lookup) <- dic$var_code

  labels_full <- labels_lookup[match(names(dat), names(labels_lookup))]
  labels_full[is.na(labels_full)] <- ""
  Hmisc::label(dat) <- as.list(labels_full)

  ####################
  ## Harmonize Names ##
  ####################
  dat_mod <- dat %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ dplyr::recode(., !!!var_names_lookup))

  ####################
  ## Returning Data ##
  ####################
  return(dat_mod)
}
