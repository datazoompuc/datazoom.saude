#' Load Brazilian vaccination data from the SI-PNI system via DATASUS
#'
#' Retrieves vaccination records from Brazil's official National Immunization Program
#' Information System (SI-PNI), made available through DATASUS. This function supports
#' two data ingestion modes. For historical data (1994-2022), it can perform automated web scraping
#' on the legacy SI-PNI Web portal to extract consolidated records of applied doses.
#' For more recent years (2023 to present), the user must provide a locally downloaded file, in which case
#' the function performs only data validation, cleaning, harmonization, and
#' standardization.
#'
#' Regardless of the ingestion mode, the returned dataset is fully harmonized and
#' consistent with the historical SI-PNI data structure.
#'
#' @param year A numeric value indicating the year of the data to be downloaded (Supported range: 1994-2022).
#' @param state A string indicating the Brazilian state abbreviation (e.g., "SP", "RJ", "AC").
#' @param strategy A string identifying the vaccination strategy (e.g., "Rotina", "Especial", "Bloqueio").
#' If NULL and in an interactive session, a menu will appear.
#' @param product A string identifying the specific vaccine product (e.g., "BCG - BCG", "Hepatite B - HB").
#' Must be a valid product for the chosen strategy. If NULL and in an interactive session, a menu will appear.
#' @param dose A character vector indicating the dose categories that were selected when downloading the data.
#' This argument is only required when using manually downloaded files up to 2022.
#' @param data An optional path to a local Excel (.xlsx) file downloaded manually from
#' the official DATASUS vaccination dashboard. If provided, web scraping is skipped and
#' the function performs only data cleaning and harmonization. This argument is mandatory
#' for data from 2023 onwards.
#' @param language A string indicating the desired language for variable names and labels.
#' Accepts "eng" (default) or "pt".
#'
#' @return A tibble (data frame) containing state, year, municipality codes,
#' municipality names, strategy, product, month, dose type, and the quantity of doses applied.
#'
#' @examples
#' \dontrun{
#' # Example: Loading Yellow Fever vaccine data for Acre in 2020
#' data <- load_vaccines(year = 2020,
#'                       state = "AC",
#'                       strategy = "Rotina",
#'                       product = "Febre amarela - FA",
#'                       language = "eng")
#' }
#'
#' @export
load_vaccines <- function(year,
                          state,
                          strategy = NULL,
                          product = NULL,
                          dose = NULL,
                          data = NULL,
                          language = "eng") {


  # -------------------------
  # Argument validation
  # -------------------------

  # Ensure data is a single value if provided
  if (!is.null(data) && length(data) != 1) {
    message("Please, input only one file by turn.")
    return(invisible(NULL))
  }

  # Ensure only a single year is processed at a time
  if (length(year) != 1) {
    message("Please, input only one year by turn.")
    return(invisible(NULL))
  }

  # Ensure only a single state is processed at a time
  if (length(state) != 1) {
    message("Please, input only one state by turn.")
    return(invisible(NULL))
  }

  # Ensure strategy is a single value if provided
  if (!is.null(strategy) && length(strategy) != 1) {
    message("Please, input only one strategy by turn.")
    return(invisible(NULL))
  }

  # Ensure product is a single value if provided
  if (!is.null(product) && length(product) != 1) {
    message("Please, input only one product by turn.")
    return(invisible(NULL))
  }


  # Consolidate parameters into a list for consistency
  param <- list(
    year     = year,
    state    = toupper(state),
    strategy = tolower(strategy),
    product  = product,
    dose     = dose,
    data     = data,
    language = language
  )


  # --- Package Verification and Loading ---
  # List of required dependencies for the function to execute
  pacotes_necessarios <- c("chromote", "httr", "readr", "dplyr", "tidyr", "stringr", "janitor")

  # Identify which required packages are NOT currently installed
  pacotes_faltando <- pacotes_necessarios[!sapply(pacotes_necessarios, requireNamespace, quietly = TRUE)]

  # If dependencies are missing, stop execution and provide installation instructions
  if (length(pacotes_faltando) > 0) {
    msg_stop <- paste(
      "Error: Required package(s) not found:",
      paste(shQuote(pacotes_faltando), collapse = ", "),
      "\n\nPlease install them before continuing:",
      sprintf("\ninstall.packages(c(%s))", paste(shQuote(pacotes_faltando), collapse = ", "))
    )
    message(msg_stop)
    return(invisible(NULL))
  }

  # Load all required packages into the session
  lapply(pacotes_necessarios, library, character.only = TRUE)


  ## 1. DATA DICTIONARY (Strategy x Product)
  # Defines valid combinations of vaccination strategies and their corresponding products
  pni_valid_combos <- list(
    "Bloqueio" = c(
      "DTP/Hib - Tetra", "Dupla adulto - dT", "Dupla viral - SR", "Febre amarela - FA",
      "Influenza Trivalente - FLU3V", "Meningoc\u00f3cica AC - Meningo AC",
      "Meningoc\u00f3cica conjugada C - Men Conj C", "Pneumoc\u00f3cica 10V - Pncc10V",
      "Poliomielite inativada - VIP", "Tetra Viral - Tetra Viral",
      "Tr\u00edplice bacteriana - DTP", "Tr\u00edplice viral - SCR", "Varicela(atenuada) - Varc"
    ),
    "Campanha Indiscriminada" = c(
      "Influenza Trivalente - FLU3V", "Poliomielite oral (Bivalente) - VOP",
      "Tr\u00edplice viral - SCR"
    ),
    "Especial" = c(
      "C\u00f3lera oral - C\u00f3lera", "DTPa/Hib/Polio Inativa - PENTAinativada",
      "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla infantil - DT",
      "Febre tif\u00f3ide (atenuada) - Fta", "Febre tif\u00f3ide (polissacar\u00eddica) - FTp",
      "Haemophilus tipo b - Hib", "Hepatite A - HA", "Hepatite A Pedi\u00e1trica - HAped",
      "Hepatite B - HB", "Hexavalente - HEXA", "HPV Quadrivalente - HPV Quadri",
      "Imunoglobulina anti hepatite B - IGHB", "Imunoglobulina anti r\u00e1bica - IGRH",
      "Imunoglobulina anti tet\u00e2nica - IGTH", "Imunoglobulina anti varicela zoster - IGVZ",
      "Influenza H1N1 - H1N1", "Influenza Trivalente - FLU3V",
      "Meningoc\u00f3cica A C W Y135 - Meningo ACWY135", "Meningoc\u00f3cica conjugada C - Men Conj C",
      "Pneumoc\u00f3cica 10V - Pncc10V", "Pneumoc\u00f3cica 13V - Pncc13V",
      "Pneumoc\u00f3cica 23V - Pncc23V", "Pneumoc\u00f3cica 7V - Pncc7V",
      "Poliomielite inativada - VIP", "Raiva em cultivo celular (Embri\u00e3o) - Embri\u00e3o",
      "Tr\u00edplice acelular infantil - DTPa", "Tr\u00edplice bacteriana acelular (adulto)- dTpa - dTpa adulto",
      "Vacina ads hepatite A (inativada, virossomal) - HAadulto", "Varicela(atenuada) - Varc"
    ),
    "Intensifica\u00e7\u00e3o" = c(
      "BCG - BCG", "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla adulto - dT",
      "Dupla viral - SR", "Febre amarela - FA", "Febre Amarela-Dose fracionada (0,1 ml) - FA(0,1 ml)",
      "Hepatite A Pedi\u00e1trica - HAped", "Hepatite B - HB", "HPV Quadrivalente - HPV Quadri",
      "Meningoc\u00f3cica conjugada C - Men Conj C", "Pneumoc\u00f3cica 10V - Pncc10V",
      "Poliomielite inativada - VIP", "Poliomielite oral (Bivalente) - VOP",
      "Tetra Viral - Tetra Viral", "Tr\u00edplice bacteriana - DTP",
      "Tr\u00edplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tr\u00edplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina rotav\u00edrus humano - VRH", "Varicela(atenuada) - Varc"
    ),
    "Monitoramento R\u00e1pido de Cobertura Vacinal" = c(
      "DTP/HB/Hib - Penta", "Febre amarela - FA", "Meningoc\u00f3cica conjugada C - Men Conj C",
      "Pneumoc\u00f3cica 10V - Pncc10V", "Poliomielite inativada - VIP",
      "Poliomielite oral (Bivalente) - VOP", "Tetra Viral - Tetra Viral",
      "Tr\u00edplice bacteriana - DTP", "Tr\u00edplice viral - SCR", "Vacina rotav\u00edrus humano - VRH"
    ),
    "Rotina" = c(
      "BCG - BCG", "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla adulto - dT",
      "Dupla viral - SR", "Febre amarela - FA", "Hepatite A Pedi\u00e1trica - HAped",
      "Hepatite B - HB", "HPV Quadrivalente - HPV Quadri",
      "Meningoc\u00f3cica A C W Y135 - Meningo ACWY135", "Meningoc\u00f3cica conjugada C - Men Conj C",
      "Pneumoc\u00f3cica 10V - Pncc10V", "Pneumoc\u00f3cica 23V - Pncc23V",
      "Pneumoc\u00f3cica 7V - Pncc7V", "Poliomielite inativada - VIP",
      "Poliomielite oral (Bivalente) - VOP", "Raiva em cultivo celular Vero - Vero",
      "Rub\u00e9ola - Rub\u00e9ola", "Sarampo - Sarampo", "Tetra Viral - Tetra Viral",
      "Tox\u00f3ide Tet\u00e2nico - TT", "Tr\u00edplice bacteriana - DTP",
      "Tr\u00edplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tr\u00edplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina rotav\u00edrus humano - VRH", "Varicela(atenuada) - Varc"
    ),
    "Servi\u00e7o Privado" = c(
      "BCG - BCG", "DTPa/Hib/Polio Inativa - PENTAinativada", "Dupla adulto - dT",
      "Febre amarela - FA", "Febre tif\u00f3ide (atenuada) - Fta",
      "Febre tif\u00f3ide (polissacar\u00eddica) - FTp", "Haemophilus tipo b - Hib",
      "Hepatite A - HA", "Hepatite A Pedi\u00e1trica - HAped", "Hepatite AeB(pedi\u00e1trica) - HAeHBped",
      "Hepatite AeB(uso adulto) - HAeHB", "Hepatite B - HB", "Herpez Zoster - VHZ",
      "Hexavalente - HEXA", "HPV Bivalente - HPV", "HPV Quadrivalente - HPV Quadri",
      "Influenza ID - FLU ID", "Influenza Tetravalente (Quadrivalente) - FLU4V",
      "Influenza Trivalente - FLU3V", "Meningoc\u00f3cica B - MEN B",
      "Meningoc\u00f3cica A C W Y135 - Meningo ACWY135", "Meningoc\u00f3cica B/C - MEN B/C",
      "Meningoc\u00f3cica conjugada C - Men Conj C", "Pneumoc\u00f3cica 13V - Pncc13V",
      "Pneumoc\u00f3cica 23V - Pncc23V", "Poliomielite inativada - VIP",
      "Rotav\u00edrus pentavalente - ROTA penta", "Tetra Viral - Tetra Viral",
      "Tox\u00f3ide Tet\u00e2nico - TT", "Tr\u00edplice acelular infantil - DTPa",
      "Tr\u00edplice acelular/poliomelite inativada - DTPaVIP", "Tr\u00edplice bacteriana - DTP",
      "Tr\u00edplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tr\u00edplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina Herpes-Zoster (recombinante) - VZR", "Varicela(atenuada) - Varc"
    ),
    "Soroterapia" = c(
      "soro antiaracn\u00eddico - SARC", "Soro botr\u00f3pico - SBOTR",
      "Soro botr\u00f3pico/crot\u00e1lico - SBOCR", "Soro botr\u00f3pico/laqu\u00e9tico - SBOLAQ",
      "Soro botul\u00ednico bivalente - SBOTULBI", "Soro botul\u00ednico trivalente - SBOTULTRI",
      "Soro crot\u00e1lico - SCROT", "Soro dift\u00e9rico - SAD", "Soro elap\u00eddico - SELAP",
      "Soro escorpi\u00f4nico - SESCOR", "Soro lon\u00f4mico - SLONO", "Soro loxosc\u00e9lico - SLOXO",
      "Soro r\u00e1bico humano - SARH", "Soro tet\u00e2nico - SAT"
    )
  )


  ## 2. INTERACTIVE SELECTION (Fallback if arguments are missing)
  # ---- STRATEGY ----
  if (is.null(strategy) || !strategy %in% names(pni_valid_combos)) {

    if (!interactive()) {
      message("Invalid or missing strategy.", call. = FALSE)
      return(invisible(NULL))
    }

    if (!is.null(strategy)) {
      message(
        paste("Invalid strategy ('", strategy, "'). Please select a valid one.", sep = ""),
        call. = FALSE
      )
    }

    estrategias_disponiveis <- names(pni_valid_combos)

    idx <- utils::menu(
      estrategias_disponiveis,
      title = "Select Strategy:"
    )

    if (idx == 0) {
      message("Selection cancelled by user.", call. = FALSE)
      return(invisible(NULL))
    }

    strategy <- estrategias_disponiveis[idx]
  }

  # ---- PRODUCT ----
  if (is.null(product) || !product %in% pni_valid_combos[[strategy]]) {

    if (!interactive()) {
      message("Invalid or missing product.", call. = FALSE)
      return(invisible(NULL))
    }

    if (!is.null(product)) {
      message(
        paste(
          "Invalid product ('", product,
          "') for strategy ('", strategy, "'). Please select a valid one.",
          sep = ""
        ),
        call. = FALSE
      )
    }

    produtos_disponiveis <- pni_valid_combos[[strategy]]

    idx <- utils::menu(
      produtos_disponiveis,
      title = paste("Select Product for '", strategy, "':", sep = "")
    )

    if (idx == 0) {
      message("Selection cancelled by user.", call. = FALSE)
      return(invisible(NULL))
    }

    product <- produtos_disponiveis[idx]
  }

  # 3. Internal Function Call and Data Processing
  # Check if year is within the supported legacy range
  if (year < 1994) {

    message("Please select a year between 1994 and 2022.")
    return(invisible(NULL))

  } else if (year >= 1994 & year < 2023) {

    # Fetch raw data using the internal load_pni function
    dat <- load_pni(year = param$year,
                    state = param$state,
                    strategy = strategy,
                    dose = param$dose,
                    product = product,
                    data = param$data)

  } else if (year >= 2023) {

    # Fetch raw data using the internal pni_after_2023 function
    dat <- pni_after_2023(year = param$year,
                          state = param$state,
                          strategy = strategy,
                          product = product,
                          data = param$data)

  }


  #################
  ### Labelling ###
  #################

  # Load metadata dictionary for PNI datasets
  dic <- load_dictionary("pni")

  # Map column names based on the user-selected language (PT vs ENG)
  if (param$language == "pt") {
    names_map <- setNames(dic$name_pt, dic$var_code)
  } else {
    names_map <- setNames(dic$name_eng, dic$var_code)
  }

  # Clean the map of any missing values
  names_map <- names_map[!is.na(names_map)]

  rename_map <- setNames(names(names_map), names_map)

  # Apply renaming only to columns present in both the data and the map
  if (!is.null(dat)) {

    dat <- dat %>%
      dplyr::rename(dplyr::any_of(rename_map))

  } else {

    return(invisible(NULL))

  }

  # Construct the label map for variable descriptions
  if (param$language == "pt") {
    labels_map <- setNames(dic$label_pt, dic$name_pt)
  } else {
    labels_map <- setNames(dic$label_eng, dic$name_eng)
  }
  labels_map <- labels_map[!is.na(labels_map)]

  # Extract labels for existing columns and apply using Hmisc
  current_names <- names(dat)
  new_labels <- labels_map[current_names]

  labels_list <- as.list(new_labels)
  names(labels_list) <- current_names
  Hmisc::label(dat) <- labels_list


  ############################
  ### Harmonizing Variable ###
  ############################

  # Convert the resulting data frame into a tibble for better printing/handling
  dat_mod <- dat %>%
    tibble::as_tibble()


  ######################
  ### Returning Data ###
  ######################

  return(dat_mod)

}

