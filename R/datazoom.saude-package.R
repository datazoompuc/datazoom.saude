#' @description Functions to download, clean, and standardize Brazilian public health microdata from DATASUS.
#' @details
#' The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops connections
#' mid-transfer. All FTP loaders automatically retry each file download up to
#' 3 times before failing.
#' @useDynLib datazoom.saude, .registration = TRUE
#'
#' @keywords internal
#' @encoding UTF-8
#' @importFrom magrittr %>%

"_PACKAGE"

## usethis namespace: start
#' @importFrom stats setNames
## usethis namespace: end
NULL
