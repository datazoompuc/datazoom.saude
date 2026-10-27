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

  ## Downloading file

  if (source == "datasus") {
    utils::download.file(url = path, destfile = temp, method = "curl", quiet = TRUE)
  } else {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }

  ###############
  ## Load Data ##
  ###############

  if (file_extension != ".dbc") {
    unlink(temp)
    stop("external_download(): unsupported file type '", file_extension, "' for ", path)
  }

  dat <- read.dbc(temp)

  ##############################
  ## Excluding Temporary File ##
  ##############################

  unlink(temp)

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
