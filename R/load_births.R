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

load_births <- function(time_period,
                        states = "all",
                        raw_data = FALSE,
                        language = "eng") {


}
