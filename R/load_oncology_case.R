#' Loads oncology case data from Brazil provided by DATASUS
#'
#' This function downloads and organizes data from the Oncology Panel (Painel de Oncologia),
#' part of DATASUS, used in public health and epidemiological analyses.
#'
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded. For example, `2020` or `2015:2020`.
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default), returns a cleaned and standardized version of the dataset.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#'
#' @return A data table containing oncology case records from the Oncology Panel of all Brazilian states for the specified period.
#' @export
#'
#' @examples
#' \dontrun{
#' load_oncology_case(time_period = 2023)
#' }


load_oncology_case <- function(time_period,
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

  if (!is.logical(raw_data)) {
    stop("raw_data must be TRUE or FALSE. By default, it is FALSE.")
  }

  if (!language %in% c("eng", "pt")) {
    stop("the language must be 'eng' or 'pt'. By default it is 'eng'.")
  }

  # Create param list with specific parameters for SINASC
  param <- list()

  param$source <- "datasus"
  param$dataset <- "datasus_po"
  param$raw_data <- raw_data
  param$language <- language
  param$keep_all <- FALSE  # Default for births data

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)


  #############################
  ## Downloading SINASC Data ##
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

  ### Filtering by year
  file_years <- filenames %>%
    substr(5, 8)

  filenames <- filenames[file_years %in% param$time_period]

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

  labels <- tibble::tribble(
    ~ var_code, ~ value, ~ label_pt, ~ label_eng,
    "tratamento", "1", "cirurgia", "surgery",
    "tratamento", "2", "quimioterapia", "chemotherapy",
    "tratamento", "3", "radioterapia", "radiotherapy",
    "tratamento", "4", "quimioterapia + radioterapia", "chemotherapy + radiotherapy",
    "tratamento", "5", "sem informacao de tratamento", "no treatment information",
    "diagnostic", "1", "neoplasias malignas (lei no 12.732/12)", "malignant neoplasms (law no. 12.732/12)",
    "diagnostic", "2", "neoplasias in situ", "neoplasms in situ",
    "diagnostic", "3", "neoplasias de comportamento incerto ou desconhecido", "neoplasms of uncertain or unknown behavior",
    "diagnostic", "4", "C44 e C73", "C44 e C73",
    "sexo", "F", "feminino", "female",
    "sexo", "M", "masculino", "masculine",
    "estadiam", "0", "0", "0",
    "estadiam", "1", "I", "I",
    "estadiam", "2", "II", "II",
    "estadiam", "3", "III", "III",
    "estadiam", "4", "IV", "IV",
    "estadiam", "5", "nao se aplica", "not applicable",
    "estadiam", "9", "ignorado", "ignored",
  )

  # adicionando factor labels

  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(unique(labels$var_code)),
        function(x) {
          # linhas do dict correspondentes a cada variavel
          dic <- labels %>%
            dplyr::filter(var_code == dplyr::cur_column())

          # vetor de levels
          lev <- dic$value

          # vetor de labels
          if (param$language == "pt") {
            lab <- dic$label_pt
          }
          else {
            lab <- dic$label_eng
          }

          # transforma em factor

          factor(x, levels = lev, labels = lab)
        }
      )
    )

  # formatando dados

  dat <- dat %>%
    dplyr::mutate(
      dt_diag = lubridate::dmy((dt_diag)),
      dt_trat = lubridate::dmy((dt_trat)),
      dt_nasc = lubridate::dmy((dt_nasc)),
      mun_diag = as.integer(as.character(mun_diag)))

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
