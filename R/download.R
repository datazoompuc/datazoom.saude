# ---------------------------------------------------------------------------
# Session-scoped download disk cache
#
# Stores downloaded .dbc files in tempdir() subdirectories for the lifetime
# of one R session. Re-running a query for the same file (e.g. changing
# `language`, `raw_data`, or re-running an analysis script) reuses the
# cached file on disk instead of hitting DATASUS FTP again.
#
# Never persists across R sessions. Controlled via
# options(datazoom.saude.cache = FALSE) and clear_download_cache() for tests.
# ---------------------------------------------------------------------------

.dz_download_cache <- new.env(parent = emptyenv())

download_cache_enabled <- function() {
  !isFALSE(getOption("datazoom.saude.cache", TRUE))
}

#' Drop every cached downloaded file in the current session.
#' Internal only -- used by tests to isolate cache state between test_that() blocks.
#' @noRd
clear_download_cache <- function() {
  .dz_download_cache$files <- NULL
  invisible(NULL)
}

download_cache_files_env <- function() {
  if (is.null(.dz_download_cache$files)) {
    .dz_download_cache$files <- new.env(parent = emptyenv())
  }
  .dz_download_cache$files
}

file_cache_key <- function(path, file_extension) {
  paste(file_extension, path, sep = "||")
}

file_cache_get <- function(key) {
  env <- download_cache_files_env()
  if (!exists(key, envir = env, inherits = FALSE)) {
    return(NULL)
  }
  entry <- get(key, envir = env, inherits = FALSE)
  if (!dir.exists(entry$dir) || !file.exists(entry$temp)) {
    return(NULL)
  }
  entry
}

file_cache_set <- function(key, dir, temp) {
  assign(key, list(dir = dir, temp = temp), envir = download_cache_files_env())
}

assert_dbc_file <- function(file_path, source_url) {
  if (!file.exists(file_path)) {
    stop(sprintf("external_download(): downloaded file is missing for %s", source_url))
  }
  size <- file.info(file_path)$size
  if (is.na(size) || size < 32) {
    stop(sprintf("external_download(): received an unrecognized %s-byte file from %s", size, source_url))
  }
  raw_head <- readBin(file_path, what = "raw", n = min(256L, as.integer(size)))
  raw_clean <- raw_head[raw_head != as.raw(0)]
  txt_head <- iconv(rawToChar(raw_clean), from = "UTF-8", to = "UTF-8", sub = "?")
  if (grepl("^[[:space:]]*<(!doctype|html|head|body|\\?xml)", txt_head, ignore.case = TRUE, useBytes = TRUE)) {
    stop(sprintf(
      "external_download(): received an unrecognized %d-byte HTML/XML response instead of a .dbc file from %s",
      as.integer(size), source_url
    ))
  }
  invisible(TRUE)
}

download_file_once <- function(url, destfile, source) {
  if (identical(source, "datasus")) {
    utils::download.file(url = url, destfile = destfile, method = "curl", quiet = TRUE)
  } else {
    utils::download.file(url = url, destfile = destfile, mode = "wb")
  }
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

  if (file_extension != ".dbc") {
    stop("external_download(): unsupported file type '", file_extension, "' for ", path)
  }

  use_cache <- download_cache_enabled()
  cache_key <- file_cache_key(path, file_extension)
  cached <- if (use_cache) file_cache_get(cache_key) else NULL
  file_cache_hit <- !is.null(cached)

  if (file_cache_hit) {
    dir <- cached$dir
    temp <- cached$temp
  } else if (use_cache) {
    dir <- tempfile("datazoom-")
    dir.create(dir)
    temp <- tempfile(fileext = file_extension, tmpdir = dir)
    registered <- FALSE
    on.exit(if (!registered) unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  } else {
    dir <- tempdir()
    temp <- tempfile(fileext = file_extension, tmpdir = dir)
    on.exit(unlink(temp), add = TRUE)
  }

  ## Downloading file (skipped on a session cache hit)

  if (!file_cache_hit) {
    download_file_once(url = path, destfile = temp, source = source)
    assert_dbc_file(temp, path)
  }

  ###############
  ## Load Data ##
  ###############

  dat <- read.dbc(temp)

  ###################################
  ## Register in the session cache ##
  ###################################

  # Registered only after read.dbc() succeeds so a corrupt/truncated file
  # is never cached across retries in the same R session.
  if (!file_cache_hit && use_cache) {
    file_cache_set(cache_key, dir = dir, temp = temp)
    registered <- TRUE
  }

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
