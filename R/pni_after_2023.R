pni_after_2023 <- function(year, state, strategy, product, data) {

  # Check if the mandatory data file path is provided
  if (is.null(data)) {
    message("Please, input a 'data' file.")
    return(invisible(NULL))

  } else {

    # Load the raw Excel file provided in the 'data' argument
    raw <- readxl::read_excel(data)

    # Internal helper function to standardize dose descriptions into short codes
    map_dose_pni <- function(x) {
      x <- as.character(x)

      # Use regex matching to categorize various dose nomenclature into standard formats
      dplyr::case_when(
        stringr::str_detect(x, "Dose Zero") ~ "D0",
        stringr::str_detect(x, "Dose Inicial") ~ "Inicial",
        stringr::str_detect(x, "Dose \u00danica|\u00danica") ~ "DU",
        stringr::str_detect(x, "1\u00aa Dose|Dose") ~ "D1",
        stringr::str_detect(x, "2\u00aa Dose") ~ "D2",
        stringr::str_detect(x, "3\u00aa Dose") ~ "D3",
        stringr::str_detect(x, "4\u00aa Dose") ~ "D4",
        stringr::str_detect(x, "5\u00aa Dose") ~ "D5",
        stringr::str_detect(x, "Dose Adicional") ~ "Adicional",
        stringr::str_detect(x, "Refor\u00e7o") ~ "REF",
        stringr::str_detect(x, "Revacina\u00e7\u00e3o|Dose Revacina\u00e7\u00e3o") ~ "REV",
        stringr::str_detect(x, "Fracionada") ~ "Fracionada",
        stringr::str_detect(x, "Profilaxia") ~ "Profilaxia",
        stringr::str_detect(x, "Tratamento") ~ "Tratamento",
        TRUE ~ NA_character_ # Return NA if no pattern matches
      )
    }

    # Define standard Portuguese month abbreviations used as column names in newer SI-PNI reports
    month_cols <- c("jan", "fev", "mar", "abr", "mai", "jun",
                    "jul", "ago", "set", "out", "nov", "dez")

    # Identify which of these month columns actually exist in the imported file
    month_cols_available <- intersect(month_cols, names(raw))

    # Initial data cleaning and variable renaming
    dat <- raw %>%
      dplyr::rename(
        state      = "UF Ocorr\u00eancia",
        munic_name = "Munic\u00edpio Ocorr\u00eancia",
        product    = "Abrevia\u00e7\u00e3o Vacina",
        dose_raw   = "Tipo de Dose"
      ) %>%
      dplyr::mutate(
        year = as.integer(year),
        state = toupper(state),
        munic_name = stringr::str_to_lower(munic_name),
        strategy = strategy,
        dose = map_dose_pni(dose_raw) # Standardize the dose labels
      ) %>%
      # Ensure all month columns are characters to avoid type mismatch during pivoting
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(month_cols_available), as.character
        )
      )

    # Reshape the data from wide format (months as columns) to long format (rows)
    dat_long <- dat %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(month_cols_available),
        names_to = "month",
        values_to = "quantity"
      ) %>%
      dplyr::mutate(
        # Replace SI-PNI empty indicators ("-") with "0" before numeric conversion
        quantity = dplyr::if_else(quantity == "-", "0", quantity),
        quantity = suppressWarnings(as.numeric(quantity))
      )

    # Final selection and ordering of standardized columns
    dat_final <- dat_long %>%
      dplyr::select(state, year, munic_name, strategy, product, month, dose, quantity)

    return(dat_final)

  }
}
