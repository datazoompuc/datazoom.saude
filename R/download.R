download_file_once <- function(url, destfile, source) {
  if (identical(source, "datasus")) {
    utils::download.file(url = url, destfile = destfile, method = "curl", quiet = TRUE)
  } else {
    utils::download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
  }
}

# DATASUS FTP intermittently drops connections mid-transfer. Retry up to
# `max_retries` times with linear backoff, cleaning up partial files between
# attempts.
perform_download <- function(path, temp, source, max_retries = 3L, retry_wait = 1.5) {
  last_err <- NULL

  for (attempt in seq_len(max_retries)) {
    if (file.exists(temp)) unlink(temp)

    res <- tryCatch(
      suppressWarnings(download_file_once(url = path, destfile = temp, source = source)),
      error = function(e) e
    )

    ok <- !inherits(res, "error") &&
      identical(as.integer(res), 0L) &&
      file.exists(temp) &&
      isTRUE(file.info(temp)$size > 0)

    if (ok) {
      return(invisible(TRUE))
    }

    last_err <- if (inherits(res, "error")) {
      conditionMessage(res)
    } else if (!identical(as.integer(res), 0L)) {
      sprintf("download.file returned status %s", as.character(res))
    } else {
      "downloaded file is missing or empty"
    }

    if (file.exists(temp)) unlink(temp)

    if (attempt < max_retries) {
      message(sprintf(
        "Download attempt %d/%d failed for %s (%s). Retrying in %.1fs...",
        attempt, max_retries, basename(path), last_err, retry_wait * attempt
      ))
      Sys.sleep(retry_wait * attempt)
    }
  }

  stop(sprintf(
    "Failed to download %s after %d attempts: %s",
    path, max_retries, last_err
  ))
}

external_download <- function(dataset = NULL, source = NULL, file_name = NULL) {
  #####################
  ## Construct Links ##
  #####################

  ## Pull URL from the manifest

  path <- datasets_link(
    source = source,
    dataset = dataset,
    url = TRUE
  )

  # DATASUS URLs are in the form ftp://.../Dados/$file_name$, where the
  # expression surrounded by $ is a placeholder for the file to download

  if (!is.null(file_name)) {
    path <- path %>%
      stringr::str_replace("\\$file_name\\$", file_name)
  }

  #######################
  ## Initiate Download ##
  #######################

  file_extension <- sub(".*\\.", ".", path) %>%
    tolower()

  ## Define Empty Directory and Files For Download

  dir <- tempdir()
  temp <- tempfile(fileext = file_extension, tmpdir = dir)
  on.exit(unlink(temp), add = TRUE)

  if (file_extension != ".dbc") {
    stop("external_download(): unsupported file type '", file_extension, "' for ", path)
  }

  ## Downloading file (with 3-attempt retry for FTP stability)

  perform_download(path = path, temp = temp, source = source)

  ###############
  ## Load Data ##
  ###############

  dat <- read.dbc(temp)

  #################
  ## Return Data ##
  #################

  return(dat)
}

datasets_link <- function(source = NULL, dataset = NULL, url = FALSE) {
  # The URL table used to be a hardcoded tibble::tribble() here. It now lives
  # in inst/extdata/manifest/v1/datasets_link.csv (see R/manifest.R), fetched
  # at runtime from GitHub with a silent fallback to the packaged snapshot.
  # effective_table() returns exactly one row per (survey, dataset).

  survey <- available_time <- available_geo <- NULL

  link <- effective_table() %>%
    dplyr::select(dplyr::all_of(DATASETS_LINK_COLS))

  if (!is.null(source)) {
    link <- link %>%
      dplyr::filter(survey == source)
  }

  if (!is.null(dataset)) {
    link <- link %>%
      dplyr::filter(dataset == !!dataset)
  }

  if (url) {
    link <- link %>%
      purrr::pluck("url")
  }

  return(link)
}
