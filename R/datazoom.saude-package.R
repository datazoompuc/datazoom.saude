#' @description Functions to download, clean, and standardize Brazilian public health microdata from DATASUS.
#' @details
#' Downloaded `.dbc` files are cached on disk in `tempdir()` for the duration
#' of the current R session to avoid redundant transfers on repeated queries.
#' To disable session caching, set `options(datazoom.saude.cache = FALSE)`.
#'
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
