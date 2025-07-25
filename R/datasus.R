#' @title DATASUS - Mortality, hospitalizations and hospital beds
#'
#' @description Loads DATASUS data on health establishments, mortality, access to health services and several health indicators.
#'
#' @param dataset A dataset name, can be one of ("datasus_sim_do", "datasus_cnes_lt","datasus_siasus"), or more. For more details, try \code{vignette("DATASUS")}.
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded. For example, `2020` or `2015:2020`.
#' @param states A \code{string} specifying for which states to download the data. It is "all" by default, but can be a single state such as "AC" or any vector such as c("AC", "AM").
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default), returns a cleaned and standardized version of the dataset.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#' @param keep_all A \code{boolean} choosing whether to aggregate the data by municipality, in turn losing individual-level variables (\code{FALSE}) or to keep all the original variables. Only applies when raw_data is \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # download raw data for the year 2010 in the state of AM.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = "AM",
#'   raw_data = TRUE
#' )
#'
#' # download treated data with the number of deaths by cause in AM and PA.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = c("AM", "PA"),
#'   raw_data = FALSE
#' )
#'
#' # download treated data with the number of deaths by cause in AM and PA
#' # keeping all individual variables.
#' data <- load_datasus(
#'   dataset = "datasus_sim_do",
#'   time_period = 2010,
#'   states = c("AM", "PA"),
#'   raw_data = FALSE,
#'   keep_all = TRUE
#' )
#' }
#'
#' @return A \code{tibble}.
#' @export

load_datasus <- function(dataset,
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

  ##############################
  ## Binding Global Variables ##
  ##############################

  . <- abbrev_state <- code_muni <- code_muni_6 <- code_state <- codmunocor <- NULL
  codufmun <- dtobito <- file_name <- is_cid_code <- label_eng <- label_pt <- NULL
  legal_amazon <- link <- month <- name_muni <- qt_exist <- qt_nsus <- value <- NULL
  var_code <- year <- qt_sus <- causabas <- available_time <- NULL

  origem <- locnasc <- estcivmae <- escmae <- semagestac <- gravidez <- parto <- NULL
  consprenat <- sexo <- racacor <- idanomal <- escmae2010 <- dtnascmae <- NULL
  racacormae <- dtultmenst <- tpmetestim <- tpapresent <- sttrabpart <- NULL
  stcesparto <- tpnascassi <- codmunnasc <- dataset_prefix_map <- munic_res <- NULL
  mun_res <- sp_m_hosp <- dt_diag <- dt_trat <- dt_nasc <- mun_diag <- NULL


  #############################
  ## Define Basic Parameters ##
  #############################

  param <- list()

  param$source <- "datasus"
  param$dataset <- dataset
  param$raw_data <- raw_data
  param$language <- language
  param$keep_all <- keep_all

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

  # Auxiliary parameters to be passed to external_download

  param$skip_rows <- NULL
  param$filenames <- NULL

  # check if dataset and time_period are valid

  check_params(param)

  ######################
  ## Downloading Data ##
  ######################

  # For each dataset, a filenames object is generated with the list of file names to be pasted at the end of each URL

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

  if (param$dataset == "datasus_sim_do") {
    file_years <- filenames %>%
      substr(5, 8)
  }
  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
    file_years_yy <- filenames %>%
      stringr::str_extract("\\d+")
    # In this case, the position varies
  }
  if (param$dataset == "datasus_cnes_lt") {
    file_years_yy <- filenames %>%
      substr(5, 6)
  }

  if (stringr::str_detect(param$dataset, "datasus_siasus")) {

    siasus_two_digits <- c(
      "datasus_siasus_ab",
      "datasus_siasus_ad",
      "datasus_siasus_am",
      "datasus_siasus_an",
      "datasus_siasus_aq",
      "datasus_siasus_ar",
      "datasus_siasus_pa",
      "datasus_siasus_ps"
    )

    siasus_two_digits_alt <- c(
      "datasus_siasus_abo",
      "datasus_siasus_acf",
      "datasus_siasus_atd",
      "datasus_siasus_sad"
    )

    if (param$dataset %in% siasus_two_digits) {
      file_years_yy <- substr(filenames, 5, 6)
    } else if (param$dataset %in% siasus_two_digits_alt) {
      file_years_yy <- substr(filenames, 6, 7)
    }
  }


  # Only files whose name's year matches a chosen one are kept
  if (!is.null(file_years)) {
    filenames <- filenames[file_years %in% param$time_period]
  }
  if (!is.null(file_years_yy)) {
    filenames <- filenames[file_years_yy %in% param$time_period_yy]
  }

  ### Filtering for chosen states when possible

  file_state <- NULL

  if (param$dataset %in% c("datasus_sim_do") | stringr::str_detect(param$dataset, "datasus_cnes|datasus_siasus")) {
    file_state <- filenames %>%
      substr(3, 4)
  } else if (paste0(param$states, collapse = "") != "all") {
    base::message("Filtering by state not supported for all datasets. Data for other states will be included.")
  }

  if (stringr::str_detect(param$dataset, "datasus_cnes|datasus_siasus")) {
    if (param$dataset %in% siasus_two_digits) {
      file_state <- filenames %>%
        substr(3, 4)
    } else if (param$dataset %in% siasus_two_digits_alt) {
      file_state <- filenames %>%
        substr(4, 5)
    }
  }

  if (!is.null(file_state) & paste0(param$states, collapse = "") != "all") {
    filenames <- filenames[file_state %in% param$states]
  }

  ### Filtering for the chosen dataset (they come clumped together in the same folder)

  if (param$dataset %in% c("datasus_sim_doext", "datasus_sim_doinf", "datasus_sim_domat", "datasus_sim_dofet")) {
    suffix <- stringr::str_remove(param$dataset, "datasus_sim_") %>%
      toupper()

    filenames <- filenames[stringr::str_detect(filenames, suffix)]
  }

  if(stringr::str_detect(param$dataset, "datasus_siasus_")) {
    suffix <- stringr::str_remove(param$dataset, "datasus_siasus_") %>%
      toupper()

    filenames <- filenames[stringr::str_starts(filenames, suffix)]
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
          skip_rows = param$skip_rows,
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


  if (stringr::str_detect(param$dataset, "datasus_sim")) {
    dat <- dat %>%
      dplyr::mutate(
        codmunocor = as.numeric(as.character(codmunocor)),
        causabas = as.character(causabas),
        dtobito = as.Date(dtobito, format = "%d%m%Y"),
        idade_anos = dplyr::case_when(
          substr(idade, 1, 1) == "0" ~ NA_character_,
          substr(idade, 1, 1) %in% as.character(1:3) ~ "0",
          substr(idade, 1, 1) == "4" ~ substr(idade, 2, 3),
          substr(idade, 1, 1) == "5" ~ paste0(1, substr(idade, 2, 3))
        )
      )

    # The cid codes have no leading 0 when the numbers are only two digits

    dat <- dat %>%
      dplyr::mutate(
        causabas = dplyr::case_when(
          nchar(causabas) == 4 ~ causabas,
          nchar(causabas) == 3 ~ paste0(
            stringr::str_extract(causabas, "[a-zA-Z]"),
            0,
            stringr::str_extract(causabas, "\\d+")
          )
        )
      )

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

    dat <- dat %>%
      dplyr::rename("code_muni_6" = "codmunocor")
  }

  if (param$dataset == "datasus_cnes_lt") {
    dat <- dat %>%
      dplyr::mutate(codufmun = as.numeric(as.character(codufmun))) %>%
      dplyr::rename("code_muni_6" = "codufmun")

    # Creating year and month variables

    dat <- dat %>%
      dplyr::mutate(
        year = as.numeric(paste0("20", substr(file_name, 5, 6))),
        month = as.numeric(substr(file_name, 7, 8))
      )
  }

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
    
  if(stringr::str_detect(param$dataset,"datasus_siasus")){

    dat <- dat %>%
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.factor), as.character)
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

    geo <- datazoom.saude::municipalities %>%
      dplyr::select(
        code_muni,
        name_muni,
        code_state,
        abbrev_state,
        legal_amazon
      )

    geo <- geo %>%
      dplyr::mutate(code_muni_6 = as.integer(code_muni / 10))

    suffix <- if (param$dataset == "datasus_siasus_pa") {
      "pa_ufmun"
    } else if(param$dataset %in% c("datasus_siasus_ps","datasus_siasus_sad")) {
      "ufmun"
    } else {
      "ap_ufmun"
    }

    dat <- dat %>%
      dplyr::left_join(geo, by = stats::setNames("code_muni_6", suffix))
    }


  #################
  ## Aggregating ##
  #################

  if (stringr::str_detect(param$dataset, "datasus_sim") & !param$keep_all) {
    # Obtaining the mortality variables

    cid_vars <- load_dictionary(param$dataset) %>%
      dplyr::filter(is_cid_code) %>%
      dplyr::select(var_code) %>%
      unlist()

    names(cid_vars) <- NULL # To stop summarise from renaming the variables

    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, legal_amazon, dtobito) %>%
      dplyr::summarise(dplyr::across(dplyr::any_of(cid_vars), sum))
  }

  if (param$dataset == "datasus_cnes_lt" & !param$keep_all) {
    dat <- dat %>%
      dplyr::group_by(code_muni_6, code_muni, name_muni, code_state, abbrev_state, legal_amazon, year, month) %>%
      dplyr::summarise(dplyr::across(c(qt_exist, qt_sus, qt_nsus)))
  }

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

  if (stringr::str_detect(param$dataset, "datasus_siasus")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }

  if (stringr::str_detect(param$dataset, "datasus_sim")) {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon, dtobito) %>%
      tibble::as_tibble()
  }
  if (param$dataset == "datasus_cnes_lt") {
    dat_mod <- dat %>%
      dplyr::relocate(code_muni, name_muni, code_state, abbrev_state, legal_amazon) %>%
      tibble::as_tibble()
  }

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


expand_cid_code <- function(cid) {
  # Turns a character "A050-B010" into an expanded vector c("A050", "A051", ..., "B010")
  # Also turns "A001, B001-B002" into c("A001", "B001", "B002")

  . <- NULL

  if (!(stringr::str_detect(cid, ",|-"))) {
    return(cid)
  }

  # Splitting sections separated by commas into a list
  cid <- cid %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.list()

  # Each element of the list becomes a vector with the initial and final value, separated by "-"
  cid <- cid %>%
    purrr::map(
      function(string) {
        string %>%
          stringr::str_split("-") %>%
          unlist() %>%
          stringr::str_remove_all(" ")
      }
    )

  letter_to_number <- seq_along(letters)
  names(letter_to_number) <- toupper(letters)

  # Converting letters to numbers
  cid <- cid %>%
    purrr::map(
      function(code) {
        letter <- stringr::str_extract(code, "[A-Z]") %>%
          dplyr::recode(!!!letter_to_number)

        code %>%
          stringr::str_remove("[A-Z]") %>%
          paste0(letter, .) %>%
          as.numeric()
      }
    )

  # Filling in sequence ex: A001-A005
  cid <- cid %>%
    purrr::map(
      ~ if (length(.x) == 2) .x[1]:.x[2]
    )

  # Converting back to letters

  number_to_letter <- names(letter_to_number)
  names(number_to_letter) <- seq_along(letters)

  cid <- cid %>%
    purrr::map(
      function(code) {
        letter <- as.integer(code / 1000)
        number <- code - 1000 * letter

        letter <- letter %>%
          dplyr::recode(!!!number_to_letter)

        number <- paste0("00", number) %>%
          stringr::str_sub(start = -3)

        paste0(letter, number)
      }
    )

  cid %>%
    unlist()
}
