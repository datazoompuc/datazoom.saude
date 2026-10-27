# Runtime resolution of the dataset URL manifest.
#
# The manifest is a CSV that used to be a hardcoded tibble::tribble() inside
# datasets_link() (see NEWS.md and git history). It now
# lives at inst/extdata/manifest/v1/datasets_link.csv, which is BOTH the copy
# shipped inside the installed package AND the file kept at the same relative
# path on GitHub. At runtime we try the remote copy first (so users get URL
# fixes without waiting for a CRAN release) and fall back silently to the
# packaged snapshot when the network is unavailable, slow, or the remote file
# is missing/malformed.
#
# SCHEMA (same rules as datazoom.amazonia's manifest)
# ---------------------------------------------------
# A (survey, dataset) group is either:
#   - UNKEYED: exactly one row, geo_level and year both NA -- that one row IS
#     the dataset. Every dataset in this package is unkeyed today.
#   - KEYED: N>=1 rows, every one of them carrying a real geo_level (XOR a
#     real year, never both). There is no blank-key "base row" for a keyed
#     dataset.
# Every row is self-sufficient: it carries every value it needs explicitly,
# even when that repeats a value also present on a sibling row. A blank cell
# means the field genuinely has no value for that row -- never "look
# elsewhere".
#
# Two ways to read a dataset, deliberately kept separate so nothing has to
# guess which row "wins":
#   - dataset_field()/dataset_url(): exact-key lookup. If the dataset is
#     keyed and the caller passes neither geo_level nor year, this stop()s.
#   - dataset_meta(): the ONE legal cross-row read, for fields that are
#     genuinely dataset-wide. Returns the single value every row of the
#     dataset agrees on; ERRORS if they disagree.
#
# Everything here is internal. Loaders call datasets_link() (R/download.R),
# dataset_url() or check_params().

# Namespace-local cache: populated at most once per session (see link_table()).
.dz_manifest_cache <- new.env(parent = emptyenv())

MANIFEST_REL <- file.path("extdata", "manifest", "v1", "datasets_link.csv")

# The 4 key columns plus the 5 value columns each row carries explicitly.
MANIFEST_KEY_COLS <- c("survey", "dataset", "geo_level", "year")

MANIFEST_VALUE_COLS <- c(
  "url", "docs_url",
  "available_time", "available_geo",
  "resolver"
)

MANIFEST_CORE_COLS <- c(MANIFEST_KEY_COLS, MANIFEST_VALUE_COLS)

# The subset of MANIFEST_VALUE_COLS that dataset_meta() is allowed to read:
# fields expected to be genuinely dataset-wide rather than spread across
# per-row values. Deliberately excludes url and docs_url, which can vary by
# row for a keyed dataset.
MANIFEST_META_COLS <- c("available_time", "available_geo", "resolver")

# The columns datasets_link() returns, in order. Before the manifest it also
# returned sidra_code (always NA here) and named the url column "link".
DATASETS_LINK_COLS <- c(
  "survey", "dataset",
  "available_time", "available_geo", "url"
)

#' URL of the remote manifest.
#'
#' Exposed as an option so it can be pointed at a local file:// path or an
#' unreachable URL in tests.
#' @noRd
manifest_url <- function() {
  getOption(
    "datazoom.saude.manifest_url",
    paste0(
      "https://raw.githubusercontent.com/datazoompuc/datazoom.saude/",
      "main/inst/extdata/manifest/v1/datasets_link.csv"
    )
  )
}

#' Parse a manifest CSV (local path or already-downloaded tempfile).
#' @noRd
read_manifest_file <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

#' Minimal structural validation -- just enough to refuse a corrupt or
#' truncated file before it gets cached for the rest of the session.
#' @noRd
validate_manifest <- function(x) {
  stopifnot(
    is.data.frame(x),
    all(MANIFEST_CORE_COLS %in% names(x)),
    nrow(x) >= 30,
    !any(is.na(x$survey)),
    # every row is a real dataset row -- there is no survey-default
    # (dataset = NA) row
    !any(is.na(x$dataset))
  )

  invisible(TRUE)
}

#' Try to fetch the remote manifest. Returns NULL (never errors) on any
#' failure -- link_table() interprets NULL as "fall back to the snapshot".
#' @noRd
fetch_manifest <- function(timeout_s = 8) {
  old_options <- list(
    timeout = getOption("timeout"),
    download.file.method = getOption("download.file.method"),
    download.file.extra = getOption("download.file.extra")
  )
  on.exit(options(old_options), add = TRUE)

  options(timeout = timeout_s, download.file.extra = NULL)
  if (isTRUE(capabilities("libcurl"))) {
    options(download.file.method = "libcurl")
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  status <- tryCatch(
    suppressWarnings(
      utils::download.file(manifest_url(), destfile = tmp, quiet = TRUE, mode = "wb")
    ),
    error = function(e) 1L
  )

  if (!identical(as.integer(status), 0L)) {
    return(NULL)
  }
  if (!file.exists(tmp) || file.size(tmp) < 1000) {
    # Too small to be a real manifest -- most likely a 404 page.
    return(NULL)
  }

  out <- tryCatch(read_manifest_file(tmp), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(out)) {
    return(NULL)
  }

  ok <- tryCatch({
    validate_manifest(out)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    return(NULL)
  }

  out
}

#' The manifest table for this session, fetched (and cached) at most once.
#'
#' Tries the remote manifest first, unless disabled via
#' `options(datazoom.saude.use_remote_manifest = FALSE)` (always the case in
#' tests -- see tests/testthat/setup.R). On any failure it falls back
#' silently to the snapshot shipped inside the package.
#' @noRd
link_table <- function() {
  if (!is.null(.dz_manifest_cache$tbl)) {
    return(.dz_manifest_cache$tbl)
  }

  out <- NULL
  if (!isFALSE(getOption("datazoom.saude.use_remote_manifest", TRUE))) {
    out <- fetch_manifest()
  }

  src <- "remote"
  if (is.null(out)) {
    src <- "snapshot"
    out <- read_manifest_file(
      system.file(MANIFEST_REL, package = "datazoom.saude")
    )
  }

  if (isTRUE(getOption("datazoom.saude.verbose_manifest", FALSE))) {
    message("datazoom.saude: using ", src, " manifest (", nrow(out), " rows).")
  }

  .dz_manifest_cache$tbl <- out
  .dz_manifest_cache$source <- src
  out
}

#' Drop the cached manifest so the next call to link_table() re-fetches it.
#' Internal only -- used by tests and by manifest_info().
#' @noRd
clear_manifest_cache <- function() {
  if (exists("tbl", envir = .dz_manifest_cache, inherits = FALSE)) {
    rm(list = ls(envir = .dz_manifest_cache), envir = .dz_manifest_cache)
  }
  invisible(NULL)
}

#' Diagnostic helper: which manifest is currently cached, and from where.
#' Not exported; enable `options(datazoom.saude.verbose_manifest = TRUE)`
#' for the same information printed as a message at fetch time.
#' @noRd
manifest_info <- function() {
  list(
    source = .dz_manifest_cache$source %||% NA_character_,
    url = manifest_url(),
    n = if (is.null(.dz_manifest_cache$tbl)) NA_integer_ else nrow(.dz_manifest_cache$tbl)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Parse a manifest "available_time" string into an integer vector of years.
#'
#' Grammar: comma-separated tokens, each either a single year ("2023") or a
#' range ("2007-2016").
#' @noRd
parse_years <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(integer(0))
  }

  tokens <- trimws(strsplit(x, ",")[[1]])

  years <- lapply(tokens, function(tok) {
    if (grepl("-", tok, fixed = TRUE)) {
      bounds <- as.integer(trimws(strsplit(tok, "-", fixed = TRUE)[[1]]))
      seq.int(bounds[1], bounds[2])
    } else {
      as.integer(tok)
    }
  })

  unlist(years, use.names = FALSE)
}

#' Resolve a single field for (source, dataset, geo_level, year) with a
#' single exact-key lookup -- no fallthrough between rows.
#'
#' geo_level is matched exactly only for datasets that HAVE geo_level rows;
#' same for year. A KEYED dataset called with neither stop()s. Use
#' dataset_meta() instead if what you want is a value that's the same across
#' the whole dataset.
#' @noRd
dataset_field <- function(source, dataset, field, geo_level = NULL, year = NULL) {
  tbl <- link_table()
  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]

  has_geo <- any(!is.na(ds$geo_level))
  has_year <- any(!is.na(ds$year))

  if (has_geo && is.null(geo_level)) {
    stop(
      "dataset_field(): '", source, "/", dataset, "' is keyed by geo_level -- ",
      "call with a real geo_level, or use dataset_meta() if '", field,
      "' is genuinely the same across every geo_level."
    )
  }
  if (has_year && is.null(year)) {
    stop(
      "dataset_field(): '", source, "/", dataset, "' is keyed by year -- ",
      "call with a real year, or use dataset_meta() if '", field,
      "' is genuinely the same across every year."
    )
  }

  g <- if (is.null(geo_level)) NA_character_ else as.character(geo_level)
  y <- if (is.null(year)) NA_character_ else as.character(year)

  # A caller may pass geo_level/year even for a dataset that doesn't vary by
  # it -- ignore it in that case so the match lands on the row's blank key.
  if (!has_geo) g <- NA_character_
  if (!has_year) y <- NA_character_

  hit <- ds[ds$geo_level %in% g & ds$year %in% y, ]
  if (nrow(hit) == 0) return(NA_character_)

  hit[[field]][1]
}

#' @noRd
dataset_url <- function(source, dataset, geo_level = NULL, year = NULL) {
  dataset_field(source, dataset, "url", geo_level, year)
}

#' Collapse a vector of values (as seen across one dataset's rows, for one
#' column) to a single agreed value. Ignores NA. Returns list(value, agree):
#'   - zero non-NA values -> value = NA, agree = TRUE
#'   - exactly one distinct non-NA value -> that value, agree = TRUE
#'   - more than one distinct non-NA value -> value = NA, agree = FALSE
#' @noRd
agreed_value <- function(x) {
  vals <- unique(x[!is.na(x)])
  if (length(vals) == 0) return(list(value = NA_character_, agree = TRUE))
  if (length(vals) == 1) return(list(value = vals[[1]], agree = TRUE))
  list(value = NA_character_, agree = FALSE)
}

#' The one legal cross-row read: a field from MANIFEST_META_COLS that's
#' expected to be the same across every row of the dataset. stop()s if the
#' dataset's rows actually disagree -- it cannot silently pick a winner.
#' @noRd
dataset_meta <- function(source, dataset, field) {
  if (!field %in% MANIFEST_META_COLS) {
    stop(
      "dataset_meta(): '", field, "' is not in MANIFEST_META_COLS -- it is ",
      "expected to vary per row for some dataset. Use dataset_field() with ",
      "a real key instead."
    )
  }

  tbl <- link_table()
  ds <- tbl[tbl$survey == source & !is.na(tbl$dataset) & tbl$dataset == dataset, ]
  if (nrow(ds) == 0) return(NA_character_)

  res <- agreed_value(ds[[field]])
  if (!res$agree) {
    disagreeing <- paste(unique(ds[[field]][!is.na(ds[[field]])]), collapse = "' vs '")
    stop(
      "dataset_meta(): '", source, "/", dataset, "' rows disagree on '",
      field, "' ('", disagreeing, "') -- this field is not safe to read ",
      "without a key for this dataset. Use dataset_field() with a real ",
      "geo_level/year instead."
    )
  }

  res$value
}

#' One synthesized display row per (survey, dataset) -- every value column
#' collapsed to "the single value every row of the group agrees on, else
#' NA" (see agreed_value()). This is what datasets_link() exposes; a keyed
#' dataset's individual rows stay reachable only through
#' dataset_field()/dataset_meta().
#' @noRd
effective_table <- function() {
  tbl <- link_table()

  groups <- unique(tbl[!is.na(tbl$dataset), c("survey", "dataset")])

  out <- lapply(seq_len(nrow(groups)), function(i) {
    s <- groups$survey[i]
    d <- groups$dataset[i]
    rows <- tbl[tbl$survey == s & !is.na(tbl$dataset) & tbl$dataset == d, ]

    vals <- lapply(MANIFEST_VALUE_COLS, function(col) agreed_value(rows[[col]])$value)
    names(vals) <- MANIFEST_VALUE_COLS

    tibble::as_tibble(c(
      list(survey = s, dataset = d, geo_level = NA_character_, year = NA_character_),
      vals
    ))
  })

  dplyr::bind_rows(out) %>%
    dplyr::select(dplyr::all_of(MANIFEST_CORE_COLS))
}
