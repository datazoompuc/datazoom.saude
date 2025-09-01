check_params <- function(param) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  survey <- dataset <- available_time <- available_geo <- . <- NULL

  #######################
  ## Time period check ##
  #######################

  if (!is.null(param$time_period)) {
    # constructs a vector of supported years
    supp_time_period_str <- datasets_link(source = param$source, dataset = param$dataset) %>%
      purrr::pluck("available_time")

    # separates by commas: 2003, 2007, 2014 -> c(2003, 2007, 2014)
    supp_time_period <- supp_time_period_str %>%
      stringr::str_split(",", simplify = TRUE)

    # separates by hyphens: 2003 - 2007 -> 2003:2007
    supp_time_period <- supp_time_period %>%
      purrr::map(
        function(str) {
          str %>%
            stringr::str_replace("-", ":") %>%
            parse(text = .) %>%
            eval()
        }
      ) %>%
      unlist()

    if (!all(param$time_period %in% supp_time_period)) {
      time_period_error <- paste("Option time_period must be in", supp_time_period_str)
      warning(time_period_error)
    }
  }

  #####################
  ## Geo level check ##
  #####################

  if (!(is.null(param$geo_level))) {
    # constructs a vector of supported geo_levels
    supp_geo_level_str <- datasets_link(source = param$source, dataset = param$dataset) %>%
      purrr::pluck("available_geo") %>%
      tolower()

    supp_geo_level <- supp_geo_level_str %>%
      stringr::str_split(",", simplify = TRUE) %>%
      stringr::str_trim()

    if (!(param$geo_level %in% supp_geo_level)) {
      geo_level_error <- paste("Option geo_level must be one of", supp_geo_level_str)
      if (!any(is.na(supp_geo_level))) stop(geo_level_error)
    }
  }
}
