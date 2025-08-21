#' Load Brazilian mortality data from the SIM system via DATASUS
#'
#' Retrieves mortality records from Brazil is official Mortality Information System (SIM),
#' made available through DATASUS (Department of Informatics of the Brazilian Unified Health System).
#' This dataset includes detailed information on deaths by cause, location, sex, age, and other demographic characteristics.
#' Data is useful for public health research, epidemiology, and demographic analysis.
#'
#' @param dataset A string identifying the specific mortality dataset to download.
#' Accepted values are:
#' - `"do"`: General death records
#' - `"doext"`: Deaths by external causes
#' - `"doinf"`: Infant deaths
#' - `"domat"`: Maternal deaths
#' - `"dofet"`: Fetal deaths
#'
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded.
#' For example, `2020` or `2015:2020`.
#' @param states A string or vector of strings indicating the Brazilian state(s) for which the data should be downloaded.
#' Use `"all"` to download data for the entire country. For specific states, use abbreviations like `"SP"`, `"RJ"`, or `c("SP", "RJ")`.
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default),
#' returns a cleaned and standardized version of the dataset.
#' @param keep_all A \code{boolean} choosing whether to aggregate the data by municipality,
#' losing individual-level variables (\code{FALSE}) or to keep all original variables (\code{TRUE}).
#' Only applies when \code{raw_data} is \code{FALSE}.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#'
#' @return A data frame containing the mortality records.
#'
#' @examples
#' \dontrun{
#' load_mortality(dataset = "do",
#'                time_period = 2022,
#'                states = "RJ")
#'
#' load_mortality(dataset = "domat",
#'                time_period = 2020,
#'                raw_data = FALSE,
#'                language = "pt")
#' }
#'
#' @export
load_mortality <- function(dataset,
                           time_period,
                           states = "all",
                           raw_data = FALSE,
                           keep_all = FALSE,
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
  . <- file_name <- link <- name_eng <- label_eng <- ano <- NULL
  name_pt <- label_pt <- var_code <- codmunocor <- causabas <- NULL
  dtobito <- is_cid_code <- code_muni_6 <- value <- NULL

  #############################
  ### Define Basic Parameters #
  #############################

  param <- list()

  param$source <- "datasus"
  param$dataset <- paste0("datasus_sim_", dataset)
  param$raw_data <- raw_data
  param$language <- language
  param$suffix <- toupper(dataset)
  param$keep_all <- keep_all

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)
  param$states <- ifelse(states == "all", "all", toupper(states))

  param$filenames <- NULL

  # check if dataset and time_period are valid
  check_params(param)

  #############################
  ### Downloading SIM Data ###
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

  # Filtring by year and states when is possible for each dataset

  file_years <- NULL
  file_years_yy <- NULL

  # DO
  if (param$suffix == "DO") {
    file_years <- filenames %>%
      substr(5, 8)

    if (!is.null(file_years)) {
      filenames <- filenames[file_years %in% param$time_period]
    }

    file_state <- filenames %>%
      substr(3, 4)

    if (param$states == "all") {
      filenames <- filenames[file_state == "BR"]
    } else {
      filenames <- filenames[file_state %in% param$states]
    }
  }

  # DOEXT, DOINF, DOMAT, DOFET
  if (param$suffix %in% c("DOEXT", "DOINF", "DOMAT", "DOFET")) {
    file_years_yy <- filenames %>%
      stringr::str_extract("\\d+")

    if (!is.null(file_years_yy)) {
      filenames <- filenames[file_years_yy %in% param$time_period_yy]
    }
  }

  param$filenames <- filenames

  # Downloading each file
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

  # Return Raw Data if requested
  if (param$raw_data) {
    return(dat)
  }

  ############################
  ### Data Engineering      #
  ############################

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()

  # Making sure all columns that will be processed exist before processing
  if ("codmunocor" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        codmunocor = as.numeric(as.character(codmunocor))
      )
  }

  if ("causabas" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        causabas = as.character(causabas)
      )
  }

  if ("dtobito" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        dtobito = as.Date(as.character(dtobito), format = "%d%m%Y")
      )
  }

  # This part was causing the error, it is now conditional
  if ("idade" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3)),
          TRUE ~ NA_character_
        )
      )
  }

  # This part was also causing the error if 'causabas' didn't exist
  if ("causabas" %in% names(dat)) {
    dat <- dat %>%
      dplyr::mutate(
        causabas = dplyr::case_when(
          nchar(causabas) == 4 ~ causabas,
          nchar(causabas) == 3 ~ paste0(
            stringr::str_extract(causabas, "[a-zA-Z]"),
            0,
            stringr::str_extract(causabas, "\\d+")
          ),
          TRUE ~ causabas
        )
      )
  }

  dic_cid_codes <- load_dictionary(param$dataset) %>%
    dplyr::filter(is_cid_code)

  dat <- dic_cid_codes %>%
    purrr::transpose() %>%
    purrr::map_dfc(
      function(dic_row) {
        dat %>%
          dplyr::mutate(value = dplyr::case_when(
            causabas %in% expand_cid_code(dic_row$var_code) ~ 1,
            TRUE ~ 0
          )) %>%
          dplyr::select(value) %>%
          dplyr::rename_with(~ dic_row$var_code)
      }
    ) %>%
    dplyr::bind_cols(dat)

  # Making sure this column exists before trying to rename it
  if ("codmunocor" %in% names(dat)) {
    dat <- dat %>%
      dplyr::rename("code_muni_6" = "codmunocor")
  }

  ############################
  ### Aggregating           #
  ############################

  # Esta secao so e executada se o usuario escolher keep_all = FALSE
  if (!param$keep_all) {

    # Adicionando uma verificacao para garantir que a coluna 'code_muni_6' existe
    if ("code_muni_6" %in% names(dat)) {

      # Carrega as variaveis CID do dicionario
      cid_vars <- load_dictionary(param$dataset) %>%
        dplyr::filter(is_cid_code) %>%
        dplyr::select(var_code) %>%
        unlist()

      names(cid_vars) <- NULL

      # Adicionamos uma nova coluna 'ano' para agrupar
      dat <- dat %>%
        dplyr::mutate(
          ano = lubridate::year(dtobito)
        ) %>%
        # Agrupa por municipio e ano e soma as variaveis CID
        dplyr::group_by(code_muni_6, ano) %>%
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::all_of(cid_vars),
            .fns = sum,
            .names = "{.col}_sum"
          ),
          .groups = "drop" # Remove o agrupamento apos a sumarizacao
        )
    } else {
      # Se a coluna de municipio nao existir, retorna uma mensagem de aviso e os dados nao agregados
      message("Warning: Column 'code_muni_6' not found for aggregation. Returning non-aggregated data.")
    }
  }

  ############################
  ### Labelling             #
  ############################

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

  ############################
  ### Harmonizing Variable  #
  ############################

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

  ############################
  ### Returning Data        #
  ############################

  return(dat_mod)
}
