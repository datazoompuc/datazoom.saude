#' Load Brazilian mortality data from the SIM system via DATASUS
#'
#' Retrieves mortality records from Brazil's official Mortality Information System (SIM),
#' made available through DATASUS (Department of Informatics of the Brazilian Unified Health System).
#' This dataset includes detailed information on deaths by cause, location, sex, age, and other demographic characteristics.
#' Data is useful for public health research, epidemiology, and demographic analysis.
#'
#' @param dataset A string identifying the specific mortality dataset to download.
#' Accepted values are:
#' - `"general"`: General death records
#' - `"fetal"`: Fetal deaths
#' - `"external_causes"`: Deaths by external causes
#' - `"infant"`: Infant deaths
#' - `"maternal"`: Maternal deaths
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
#' load_mortality(dataset = "general",
#'                time_period = 2022,
#'                states = "RJ")
#'
#' load_mortality(dataset = "maternal",
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

  # Binding global variables to avoid check notes
  . <- file_name <- link <- name_eng <- label_eng <- ano <- NULL
  name_pt <- label_pt <- var_code <- codmunocor <- causabas <- NULL
  dtobito <- is_cid_code <- code_muni_6 <- value <- n <- setNames <- NULL
  rename <- NULL

  ###############################
  ### Define Basic Parameters ###
  ###############################

  # Renaming and normalizing the dataset name
  param <- list()
  dataset_map <- c(
    "general" = "do",
    "external_causes" = "doext",
    "infant" = "doinf",
    "maternal" = "domat",
    "fetal" = "dofet"
  )

  normalized_dataset <- tolower(dataset)

  if (!normalized_dataset %in% names(dataset_map)) {
    stop("Invalid dataset name. Please use one of: `general`, `external_causes`, `infant`, `maternal` or `fetal`.")
  }

  param$source <- "datasus"
  param$dataset <- paste0("datasus_sim_", dataset_map[normalized_dataset])
  param$raw_data <- raw_data
  param$language <- language
  param$suffix <- toupper(dataset_map[normalized_dataset])
  param$keep_all <- keep_all

  param$time_period <- time_period
  param$time_period_yy <- substr(time_period, 3, 4)

  param$states <- if (length(states) == 1 && tolower(states) == "all") {
    param$states <- "all"
  } else {
    param$states <- toupper(states)
  }

  param$filenames <- NULL

  # check if dataset and time_period are valid
  check_params(param)

  ############################
  ### Downloading SIM Data ###
  ############################

  dat_url <- datasets_link()

  url <- dat_url %>%
    dplyr::filter(dataset == param$dataset) %>%
    dplyr::select(link) %>%
    base::unlist() %>%
    as.character()

  filenames <- RCurl::getURL(url, ftp.use.epsv = TRUE, dirlistonly = TRUE) %>%
    stringr::str_split("\r*\n") %>%
    unlist()

  # Define which datasets are only national
  national_only_datasets <- c("DOEXT", "DOINF", "DOMAT", "DOFET")

  # Check if filtering by state is requested for a national dataset
  if (identical(param$states, "all") == FALSE && param$suffix %in% national_only_datasets) {
    warning("The selected dataset is only available at the national level. Downloading data for all of Brazil.")
    param$states <- "all"
  }

  # Filtering by year and states when possible for each dataset
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

    if (identical(param$states, "all")) {
      filenames <- filenames[file_state == "BR"]
    } else {
      filenames <- filenames[file_state %in% param$states]
    }
  }

  # DOEXT, DOINF, DOMAT, DOFET
  if (param$suffix %in% national_only_datasets) {

    # Filter by the correct dataset prefix first
    filenames <- filenames[stringr::str_starts(filenames, param$suffix)]

    # Extract the years YY and filter
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

  dat <- dat %>%
    purrr::imap(~ dplyr::mutate(.x, file_name = .y)) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names()

  # Return Raw Data if requested
  if (param$raw_data) {
    return(dat)
  }

  ########################
  ### Data Engineering ###
  ########################

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

  # Renaming columns dynamically based on the chosen language
  dic <- load_dictionary("datasus_sim")

  if (param$language == "pt") {
    names_map <- setNames(dic$name_pt, dic$var_code)
  } else {
    names_map <- setNames(dic$name_eng, dic$var_code)
  }

  # Remove any NA values from the names_map to avoid errors
  names_map <- names_map[!is.na(names_map)]

  # Only rename if the original column exists in the data and in the map
  dat <- dat %>%
    dplyr::rename(dplyr::any_of(names_map))

  ###################
  ### Aggregating ###
  ###################

  # This section is only executed if the user chooses keep_all = FALSE
  if (!param$keep_all) {

    # Adding a check to ensure that the 'code_muni_6' and 'causabas' columns exist
    if (("codmunocor" %in% names(dat)) && ("causabas" %in% names(dat))) {

      rename_vars <- if (language == "pt") {
        list(cod_munic = "codmunocor", causa_de_morte = "causabas", ano = "ano", num_de_mortes = "num_de_mortes")
      } else {
        list(munic_code = "codmunocor", cause_of_death = "causabas", year = "ano", num_of_deaths = "num_de_mortes")
      }

      # Adding a new 'ano' column to group by
      dat <- dat %>%
        dplyr::mutate(
          ano = lubridate::year(dtobito)
        ) %>%
        # Grouping by municipality, year, and individual CID code
        dplyr::group_by(codmunocor, ano, causabas) %>%
        # Counting the number of deaths for each group
        dplyr::summarise(
          num_de_mortes = dplyr::n(),
          .groups = "drop" # Removes the grouping after summarization
        ) %>%
        rename(!!!rename_vars)
    } else {
      # If the required columns are not found, return a warning message and the non-aggregated data
      message("Warning: Columns 'codmunocor' or 'causabas' not found for aggregation. Returning non-aggregated data.")
    }
  }

  #################
  ### Labelling ###
  #################

  # Now use the new column names for labeling
  if (param$language == "pt") {
    labels_map <- setNames(dic$label_pt, dic$name_pt)
  } else {
    labels_map <- setNames(dic$label_eng, dic$name_eng)
  }
  labels_map <- labels_map[!is.na(labels_map)]

  # Apply labels to the dataset
  current_names <- names(dat)
  new_labels <- labels_map[current_names]

  # Convert to list and apply labels
  labels_list <- as.list(new_labels)
  names(labels_list) <- current_names
  Hmisc::label(dat) <- labels_list

  ############################
  ### Harmonizing Variable ###
  ############################

  dat_mod <- dat %>%
    tibble::as_tibble()

  ######################
  ### Returning Data ###
  ######################

  return(dat_mod)
}
