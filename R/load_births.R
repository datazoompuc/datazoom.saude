#' Loads birth data of newborns from Brazil provided by DATASUS
#'
#' This function downloads and organizes data from SINASC (Live Birth Information System),
#' part of DATASUS, used in public health analyses.
#'
#' @param time_period A numeric value or vector indicating the year(s) of the data to be downloaded. For example, `2020` or `2015:2020`.
#' @param states A string or vector of strings indicating the Brazilian state(s) for which the data should be downloaded. Use `"all"` to download data for the entire country. For specific states, use abbreviations like `"SP"`, `"RJ"`, or `c("SP", "RJ")`.
#' @param raw_data Logical. If `TRUE`, returns the raw data exactly as provided by DATASUS. If `FALSE` (default), returns a cleaned and standardized version of the dataset.
#' @param language A string indicating the desired language of variable names and labels. Accepts `"eng"` (default) for English or `"pt"` for Portuguese.
#'
#' @return A data frame containing birth records from SINASC for the specified period and states.
#' @export
#'
#' @examples
#' \dontrun{
#' load_births(time_period = 2023,
#'             states = "RJ",
#'             raw_data = FALSE)
#' }

load_births <- function(time_period,
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
  param$dataset <- "datasus_sinasc"
  param$raw_data <- raw_data
  param$language <- language
  param$keep_all <- FALSE  # Default for births data

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- ifelse(states == "all", "all", toupper(states))

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

  ### Filtering for chosen states
  file_state <- filenames %>%
    substr(3, 4)

  if (paste0(param$states, collapse = "") != "all") {
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

  labels <- tibble::tribble(
    ~ var_code, ~ value, ~ label_pt, ~ label_eng,
    "origem", 1, "oracle", "oracle",
    "origem", 2, "ftp", "ftp",
    "origem", 3, "sead", "sead",
    "locnasc", 1, "hospital", "hospital",
    "locnasc", 2, "outros estabelecimentos de saude", "other health establishments",
    "locnasc", 3, "domicilio", "home",
    "locnasc", 4, "outros", "other",
    "locnasc", 5, "aldeia indigena", "indigenous village",
    "locnasc", 9, "ignorado", "unknown",
    "estcivmae", 1, "solteira", "single",
    "estcivmae", 2, "casada", "married",
    "estcivmae", 3, "viuva", "widowed",
    "estcivmae", 4, "divorciada", "divorced",
    "estcivmae", 5, "uniao estavel", "civil union",
    "estcivmae", 9, "ignorado", "unknown",
    "escmae", 1, "nenhuma", "none",
    "escmae", 2, "1 a 2 anos", "1 to 2 years",
    "escmae", 3, "4 a 7 anos", "4 to 7 years",
    "escmae", 4, "8 a 11 anos", "8 to 11 years",
    "escmae", 5, "12 e mais", "12 or more years",
    "escmae", 9, "ignorado", "unknown",
    "gestacao", 1, "menos de 22 semanas", "less than 22 weeks",
    "gestacao", 2, "22 a 27 semanas", "22 to 27 weeks",
    "gestacao", 3, "28 a 31 semanas", "28 to 31 weeks",
    "gestacao", 4, "32 a 36 semanas", "32 to 36 weeks",
    "gestacao", 5, "37 a 41 semanas", "37 to 41 weeks",
    "gestacao", 6, "42 semanas e mais", "42 weeks or more",
    "gestacao", 9, "ignorado", "unknown",
    "gravidez", 1, "unica", "single",
    "gravidez", 2, "dupla", "twin",
    "gravidez", 3, "tripla ou mais", "triplet or more",
    "gravidez", 9, "ignorado", "unknown",
    "parto", 1, "vaginal", "vaginal",
    "parto", 2, "cesario", "cesarean",
    "parto", 9, "ignorado", "unknown",
    "consultas", 1, "nenhuma", "none",
    "consultas", 2, "de 1 a 3", "1 to 3",
    "consultas", 3, "de 4 a 6", "4 to 6",
    "consultas", 4, "7 e mais", "7 or more",
    "consultas", 9, "ignorado", "unknown",
    "sexo", 0, "ignorado", "unknown",
    "sexo", 1, "masculino", "male",
    "sexo", 2, "feminino", "female",
    "racacor", 1, "branca", "white",
    "racacor", 2, "preta", "black",
    "racacor", 3, "amarela", "yellow",
    "racacor", 4, "parda", "brown",
    "racacor", 5, "indigena", "indigenous",
    "idanomal", 1, "nao","no",
    "idanomal", 2, "sim", "yes",
    "idanomal", 9, "ignorado", "unknown",
    "escmae2010", 0, "sem escolaridade", "no education",
    "escmae2010", 1, "fundamental 1", "elementary 1",
    "escmae2010", 2, "fundamental 2", "elementary 2",
    "escmae2010", 3, "medio", "high school",
    "escmae2010", 4, "superior incompleto", "incomplete higher education",
    "escmae2010", 5, "superior completo", "complete higher education",
    "escmae2010", 9, "ignorado", "unknown",
    "racacormae", 1, "branca", "white",
    "racacormae", 2, "preta", "black",
    "racacormae", 3, "amarela", "yellow",
    "racacormae", 4, "parda", "brown",
    "racacormae", 5, "indigena", "indigenous",
    "tpmetestim", 1, "exame fisico", "physical exam",
    "tpmetestim", 2, "outro metodo", "other method",
    "tpmetestim", 9, "ignorado", "unknown",
    "tpapresent", 1, "cefalica", "cephalic",
    "tpapresent", 2, "pelvica ou podalica", "breech or footling",
    "tpapresent", 3, "transversa", "transverse",
    "tpapresent", 9, "ignorado", "unknown",
    "sttrabpart", 1, "sim", "yes",
    "sttrabpart", 2, "nao", "no",
    "sttrabpart", 9, "ignorado", "unknown",
    "stcesparto", 1, "sim", "yes",
    "stcesparto", 2, "nao", "no",
    "stcesparto", 3, "nao se aplica", "not applicable",
    "stcesparto", 9, "ignorado", "unknown",
    "tpnascassi", 1, "medico", "doctor",
    "tpnascassi", 2, "enfermeira obstetriz", "obstetric nurse",
    "tpnascassi", 3, "parteira", "midwife",
    "tpnascassi", 4, "outros", "other",
    "tpnascassi", 9, "ignorado", "unknown"
  )

  # Adding factor labels
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

  # Formatting data
  dat <- dat %>%
    dplyr::mutate(
      dtcadastro = lubridate::dmy(as.character(dtcadastro)),
      dtrecebim = lubridate::dmy(as.character(dtrecebim)),
      dtnascmae = lubridate::dmy(as.character(dtnascmae)),
      dtultmenst = lubridate::dmy(as.character(dtultmenst)),
      dtdeclarac = lubridate::dmy(as.character(dtdeclarac)),
      dtnasc = lubridate::dmy(as.character(dtnasc)),
      codmunnasc = as.numeric(as.character(codmunnasc))
      )

  ###############
  ## Labelling ##
  ###############

  dic <- load_dictionary(param$dataset)

  row_numbers <- match(names(dat), dic$var_code)

  if (param$language == "pt") {
    dic <- dic %>%
      dplyr::select(var_code, name_pt, label_pt)
  }
  if (param$language == "eng") {
    dic <- dic %>%
      dplyr::select(var_code, name_eng, label_eng)
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
