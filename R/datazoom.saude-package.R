#' @description Functions to download, clean, and standardize Brazilian public health microdata from DATASUS.
#' @details
#' Downloaded `.dbc` files are cached on disk in `tempdir()` for the duration
#' of the current R session to avoid redundant transfers on repeated queries.
#' To disable session caching, set `options(datazoom.saude.cache = FALSE)`.
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
