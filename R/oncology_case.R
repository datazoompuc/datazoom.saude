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
#' # Download processed oncology data for the year 2023.
# This will return data from the Oncology Panel for all Brazilian states.
#'oncology_cases_treated <- load_oncology_case(time_period = 2023,
#'                                             raw_data = FALSE,
#'                                             language = "eng"
#'                                             )
#'
#' # Download raw oncology data for the years 2021 to 2022 with labels in portuguese.
#'oncology_cases_raw <- load_oncology_case(time_period = 2021:2022,
#'                                         raw_data = TRUE,
#'                                         language = "pt"
#'                                         )
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

  # Basic argument checks
  if (!is.numeric(time_period)) {
    stop("time_period must be a numeric value or vector of years.")
  }

  if (!is.logical(raw_data)) {
    stop("raw_data must be TRUE or FALSE. By default, it is FALSE.")
  }

  if (!language %in% c("eng", "pt")) {
    stop("the language must be 'eng' or 'pt'. By default it is 'eng'.")
  }

  # Declare global variables to avoid check notes
  . <- file_name <- dataset <- link <- name_eng <- label_eng <- NULL
  name_pt <- label_pt <- var_code <- NULL
  dt_diag <- dt_trat <- dt_nasc <- mun_diag <- NULL

  # Create param list with specific parameters for Oncology Panel
  param <- list()

  param$source <- "datasus"
  param$dataset <- "datasus_po"
  param$raw_data <- raw_data
  param$language <- language
  param$keep_all <- FALSE  # Default is FALSE

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- "ALL" # Required for the check_params function

  # Auxiliary parameters to be passed to external_download
  param$filenames <- NULL

  # Check if dataset and time_period are valid
  check_params(param)

  #############################
  ## Downloading PO Data     ##
  #############################

  # Get dataset source URL
  dat_url <- datasets_link()

  url <- dat_url %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    base::unlist() %>%
    as.character()

  # Use RCurl to extract the names of all files stored on the server
  filenames <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    stringr::str_split("\r*\n") %>%
    unlist()

  # Filter by year
  file_years <- filenames %>%
    substr(5, 8)

  filenames <- filenames[file_years %in% param$time_period]

  param$filenames <- filenames

  # Download each file in filenames
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

  # Return raw data if requested
  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  dat <- dat %>%
    janitor::clean_names()

  labels <- tibble::tribble(
    ~ var_code, ~ value, ~ label_pt, ~ label_eng,
    "tratamento", "1", "cirurgia", "surgery",
    "tratamento", "2", "quimioterapia", "chemotherapy",
    "tratamento", "3", "radioterapia", "radiotherapy",
    "tratamento", "4", "quimioterapia + radioterapia", "chemotherapy + radiotherapy",
    "tratamento", "5", "sem informacao de tratamento", "no treatment information",
    "diagnostic", "01", "neoplasias malignas (lei no 12.732/12)", "malignant neoplasms (law no. 12.732/12)",
    "diagnostic", "02", "neoplasias in situ", "neoplasms in situ",
    "diagnostic", "03", "neoplasias de comportamento incerto ou desconhecido", "neoplasms of uncertain or unknown behavior",
    "diagnostic", "04", "C44 e C73", "C44 and C73",
    "sexo", "F", "feminino", "female",
    "sexo", "M", "masculino", "male",
    "estadiam", "0", "0", "0",
    "estadiam", "1", "I", "I",
    "estadiam", "2", "II", "II",
    "estadiam", "3", "III", "III",
    "estadiam", "4", "IV", "IV",
    "estadiam", "5", "nao se aplica", "not applicable",
    "estadiam", "9", "ignorado", "ignored",
  )

  # Adding factor labels
  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(unique(labels$var_code)),
        function(x) {
          # Dictionary rows corresponding to each variable
          dic <- labels %>%
            dplyr::filter(var_code == dplyr::cur_column())

          # Vector of levels
          lev <- dic$value

          # Vector of labels
          if (param$language == "pt") {
            lab <- dic$label_pt
          } else {
            lab <- dic$label_eng
          }

          # Transform into factor
          factor(x, levels = lev, labels = lab)
        }
      )
    )

  # Formatting data
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

  if (param$language == "pt") {
    labels_lookup <- dic$label_pt
  } else {
    labels_lookup <- dic$label_eng
  }

  names(labels_lookup) <- dic$var_code

  labels_full <- labels_lookup[match(names(dat), names(labels_lookup))]

  # Making sure 'labels' has the same length as the number of columns
  labels_full[is.na(labels_full)] <- ""

  Hmisc::label(dat) <- as.list(labels_full)

  ################################
  ## Harmonizing Variable Names ##
  ################################

  dat_mod <- dat %>% tibble::as_tibble()

  dic <- load_dictionary(param$dataset)
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

  #####################
  ## Sorting Columns ##
  #####################

  if (param$language == "pt"){

    dat_mod = dat_mod %>%
      dplyr::select(

        # Pacient ID
        sexo_paciente,
        idade_paciente,
        data_nascimento,
        uf_residencia,
        municipio_residencia,

        # Diagnosis
        ano_diagnostico,
        ano_mes_diagnostico,
        data_diagnostico,
        categoria_diagnostico,
        cid_detalhado,
        estadiamento,
        cnes_diagnostico,
        uf_diagnostico,
        municipio_diagnostico,

        # Treatment
        ano_tratamento,
        ano_mes_tratamento,
        data_tratamento,
        tipo_tratamento,
        cnes_tratamento,
        uf_tratamento,
        municipio_tratamento,
        intervalo_tratamento

      )

  } else{

    dat_mod = dat_mod %>%
      dplyr::select(

        # Pacient ID
        patient_sex,
        patient_age,
        date_birth,
        state_residence,
        mun_residence,

        # Diagnosis
        year_diagnosis,
        year_month_diagnosis,
        date_diagnosis,
        diagnosis_category,
        detailed_icd,
        staging,
        cnes_diagnosis,
        state_diagnosis,
        mun_diagnosis,

        # Treatment
        year_treatment,
        year_month_treatment,
        date_treatment,
        treatment_type,
        cnes_treatment,
        state_treatment,
        mun_treatment,
        treatment_interval

      )

  }

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
