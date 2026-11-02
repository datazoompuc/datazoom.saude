# Shared validation helpers for actions/scripts/build_manifest.R.
#
# These functions implement the validation gate: nothing is ever committed
# to the manifest unless it passes every check here. Kept in its own file
# (not the package's R/) because it is CI tooling, not runtime code -- actions/
# is .Rbuildignore'd, so jsonlite/curl used by the HTTP probe never become
# package dependencies.

MANIFEST_ALL_COLS <- c(
  "survey", "dataset", "geo_level", "year",
  "url", "docs_url", "available_time", "available_geo", "resolver"
)

KEY_COLS <- c("survey", "dataset", "geo_level", "year")

is_http_exempt <- function(row) {
  is.na(row$url) || identical(row$url, "")
}

# ---- 1-5: structural validation -------------------------------------------

validate_schema <- function(candidate) {
  errors <- character(0)

  missing_cols <- setdiff(MANIFEST_ALL_COLS, names(candidate))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("missing columns:", paste(missing_cols, collapse = ", ")))
  }
  if (!identical(names(candidate)[seq_along(MANIFEST_ALL_COLS)], MANIFEST_ALL_COLS)) {
    errors <- c(errors, "column order does not match MANIFEST_ALL_COLS")
  }
  if (!all(vapply(candidate, is.character, logical(1)))) {
    errors <- c(errors, "not every column is character")
  }

  errors
}

validate_row_count <- function(old, candidate, tolerance = 0.15) {
  errors <- character(0)
  lo <- floor(nrow(old) * (1 - tolerance))
  hi <- ceiling(nrow(old) * (1 + tolerance))
  if (nrow(candidate) < lo || nrow(candidate) > hi) {
    errors <- c(errors, sprintf(
      "row count %d outside +/-%.0f%% of previous %d (expected [%d, %d])",
      nrow(candidate), tolerance * 100, nrow(old), lo, hi
    ))
  }
  errors
}

validate_no_deletions <- function(old, candidate) {
  errors <- character(0)
  old_pairs <- unique(paste(old$survey, old$dataset, sep = "\r"))
  new_pairs <- unique(paste(candidate$survey, candidate$dataset, sep = "\r"))
  missing <- setdiff(old_pairs, new_pairs)
  if (length(missing) > 0) {
    errors <- c(errors, paste(
      "(survey, dataset) pairs removed (never allowed):",
      paste(gsub("\r", "/", missing), collapse = ", ")
    ))
  }
  errors
}

VALIDATOR_META_COLS <- c("available_time", "available_geo", "resolver")

validate_row_grouping <- function(candidate) {
  errors <- character(0)

  if (any(is.na(candidate$dataset) | candidate$dataset == "")) {
    errors <- c(errors, "one or more rows has dataset = NA or empty (no survey-default row allowed)")
  }

  keys <- candidate[, KEY_COLS]
  dup_idx <- which(duplicated(keys))
  if (length(dup_idx) > 0) {
    dup_labels <- apply(keys[dup_idx, ], 1, function(r) {
      paste(r[!is.na(r) & nzchar(r)], collapse = "/")
    })
    errors <- c(errors, paste("duplicate key row(s):", paste(unique(dup_labels), collapse = ", ")))
  }

  groups <- split(candidate, list(candidate$survey, candidate$dataset), drop = TRUE)
  for (g in groups) {
    has_geo <- !is.na(g$geo_level) & g$geo_level != ""
    has_year <- !is.na(g$year) & g$year != ""
    label <- paste0(g$survey[1], "/", g$dataset[1])

    if (any(has_geo & has_year)) {
      errors <- c(errors, paste(label, "has row(s) mixing geo_level and year overrides"))
    }
    if (nrow(g) == 1 && !has_geo[1] && !has_year[1]) next
    if (any(!has_geo & !has_year)) {
      errors <- c(errors, paste(label, "is keyed but contains a blank-key base row (not allowed)"))
    }
  }

  errors
}

validate_dataset_level_agreement <- function(candidate) {
  errors <- character(0)
  groups <- split(candidate, list(candidate$survey, candidate$dataset), drop = TRUE)
  for (g in groups) {
    if (nrow(g) <= 1) next
    label <- paste0(g$survey[1], "/", g$dataset[1])
    for (col in VALIDATOR_META_COLS) {
      vals <- unique(g[[col]][!is.na(g[[col]]) & nzchar(g[[col]])])
      if (length(vals) > 1) {
        errors <- c(errors, sprintf(
          "%s has disagreement across rows for '%s' ('%s')",
          label, col, paste(vals, collapse = "' vs '")
        ))
      }
    }
  }
  errors
}

validate_placeholders <- function(old, candidate) {
  errors <- character(0)
  extract_placeholders <- function(x) {
    regmatches(x, gregexpr("\\$[a-z_]+\\$", x))
  }
  key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  common <- intersect(key, old_key)
  for (k in common) {
    old_url <- old$url[old_key == k][1]
    new_url <- candidate$url[key == k][1]
    if (is.na(old_url) || is.na(new_url)) next
    if (!setequal(extract_placeholders(old_url)[[1]], extract_placeholders(new_url)[[1]])) {
      errors <- c(errors, paste("placeholder set changed for", gsub("\r", "/", k)))
    }
    allowed <- c("$year$", "$state$", "$file_name$")
    bad <- setdiff(extract_placeholders(new_url)[[1]], allowed)
    if (length(bad) > 0) {
      errors <- c(errors, paste("unknown placeholder(s) in", gsub("\r", "/", k), ":", paste(bad, collapse = ", ")))
    }
  }
  errors
}

parse_years_val <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(integer(0))
  tokens <- trimws(strsplit(x, ",")[[1]])
  years <- lapply(tokens, function(tok) {
    if (grepl("-", tok, fixed = TRUE)) {
      bounds <- as.integer(trimws(strsplit(tok, "-", fixed = TRUE)[[1]]))
      if (length(bounds) == 2 && !anyNA(bounds) && bounds[1] <= bounds[2]) {
        seq.int(bounds[1], bounds[2])
      } else {
        NA_integer_
      }
    } else {
      as.integer(tok)
    }
  })
  unlist(years, use.names = FALSE)
}

validate_key_completeness <- function(candidate) {
  errors <- character(0)
  groups <- split(candidate, list(candidate$survey, candidate$dataset), drop = TRUE)
  for (g in groups) {
    label <- paste0(g$survey[1], "/", g$dataset[1])
    if (nrow(g) == 1 && (is.na(g$geo_level) || g$geo_level == "") && (is.na(g$year) || g$year == "")) {
      # Unkeyed
      next
    }
    has_geo <- any(!is.na(g$geo_level) & g$geo_level != "")
    has_year <- any(!is.na(g$year) & g$year != "")

    if (has_year) {
      declared_time <- unique(g$available_time[!is.na(g$available_time)])
      if (length(declared_time) == 1) {
        dec_yrs <- parse_years_val(declared_time)
        row_yrs <- as.integer(g$year[!is.na(g$year)])
        missing_rows <- setdiff(dec_yrs, row_yrs)
        if (length(missing_rows) > 0) {
          errors <- c(errors, sprintf("%s declares year(s) %s in available_time with no row", label, paste(missing_rows, collapse = ", ")))
        }
        undeclared_rows <- setdiff(row_yrs, dec_yrs)
        if (length(undeclared_rows) > 0) {
          errors <- c(errors, sprintf("%s has row(s) for year(s) %s not declared in available_time", label, paste(undeclared_rows, collapse = ", ")))
        }
      }
    }
  }
  errors
}

# ---- 6: Probe validation ----------------------------------------------------

probe_url <- function(url, timeout_s = 15) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = NA, reason = "curl package not available"))
  }

  probe_target <- sub("\\$file_name\\$", "", url)

  h <- curl::new_handle(timeout = timeout_s)
  if (!grepl("^ftp://", probe_target, ignore.case = TRUE)) {
    curl::handle_setopt(h, nobody = TRUE, followlocation = TRUE)
  }

  resp <- tryCatch(curl::curl_fetch_memory(probe_target, handle = h), error = function(e) e)

  if (inherits(resp, "error")) {
    return(list(ok = FALSE, reason = paste("probe error:", resp$message)))
  }

  # HTTP 200/206 or FTP 226 (directory listing completed)
  status <- resp$status_code
  if (status %in% c(200, 206, 226)) {
    return(list(ok = TRUE, status = status))
  }

  list(ok = FALSE, reason = paste("Status", status))
}

validate_http <- function(candidate, changed_keys) {
  errors <- character(0)
  key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")
  rows <- candidate[key %in% changed_keys, ]
  rows <- rows[!vapply(seq_len(nrow(rows)), function(i) is_http_exempt(rows[i, ]), logical(1)), ]
  if (nrow(rows) == 0) return(errors)

  for (u in unique(rows$url)) {
    offenders <- rows[rows$url == u, ]
    label <- paste(unique(paste(offenders$survey, offenders$dataset, sep = "/")), collapse = ", ")

    probe <- probe_url(u)
    if (isFALSE(probe$ok)) {
      errors <- c(errors, sprintf("%s: Probe check failed (%s) for %s", label, probe$reason, u))
    }
  }

  errors
}

# ---- Change detection -------------------------------------------------------

detect_changes <- function(old, candidate) {
  old_key <- paste(old$survey, old$dataset, old$geo_level, old$year, sep = "\r")
  new_key <- paste(candidate$survey, candidate$dataset, candidate$geo_level, candidate$year, sep = "\r")

  changed_keys <- character(0)
  for (k in intersect(old_key, new_key)) {
    o <- old[old_key == k, MANIFEST_ALL_COLS][1, ]
    n <- candidate[new_key == k, MANIFEST_ALL_COLS][1, ]
    if (!identical(o, n)) {
      changed_keys <- c(changed_keys, k)
    }
  }
  added <- setdiff(new_key, old_key)
  changed_keys <- c(changed_keys, added)

  list(changed_keys = changed_keys, n_changed = length(changed_keys))
}

