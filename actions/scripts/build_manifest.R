# actions/scripts/build_manifest.R
#
# Driver for the manifest refresh pipeline.
#
# Usage:
#   Rscript actions/scripts/build_manifest.R --dry-run
#   Rscript actions/scripts/build_manifest.R --dry-run --only datasus
#   Rscript actions/scripts/build_manifest.R --dry-run --skip datasus
#   Rscript actions/scripts/build_manifest.R --dry-run --check-all
#
# Exit codes:
#   0  candidate validated; no changes
#   10 candidate validated; changes present -- open a PR for human review
#   11 candidate validated; changes present, AND one or more resolvers failed
#   20 structural or probe validation failed -- candidate untrustworthy
#   30 one or more resolvers errored AND there were no changes to report

suppressPackageStartupMessages({
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

this_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
script_path <- if (length(this_file) > 0) this_file[1] else "actions/scripts/build_manifest.R"
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))

repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
source(file.path(repo_root, "actions", "scripts", "manifest_validate.R"))
source(file.path(repo_root, "actions", "scripts", "fragility_notes.R"))

MANIFEST_PATH <- file.path(repo_root, "inst", "extdata", "manifest", "v1", "datasets_link.csv")
SCRAPERS_DIR <- file.path(repo_root, "actions", "scrapers")

OUT_DIR <- file.path(repo_root, "actions", "output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
check_all <- "--check-all" %in% args
only_sources <- character(0)
i <- which(args == "--only")
for (idx in i) {
  if (idx < length(args)) only_sources <- c(only_sources, args[idx + 1])
}
skip_sources <- character(0)
i <- which(args == "--skip")
for (idx in i) {
  if (idx < length(args)) skip_sources <- c(skip_sources, args[idx + 1])
}

resolver_files <- list.files(SCRAPERS_DIR, pattern = "^resolve_.*\\.R$", full.names = TRUE)
for (f in resolver_files) source(f)

resolver_names <- grep("^resolve_", ls(), value = TRUE)
registry <- setNames(
  lapply(resolver_names, get),
  sub("^resolve_", "", resolver_names)
)

all_known_sources <- names(registry)

if (length(only_sources) > 0) {
  unknown <- setdiff(only_sources, all_known_sources)
  if (length(unknown) > 0) {
    stop(
      "--only referenced source(s) with no registered resolver: ",
      paste(unknown, collapse = ", "),
      if (length(all_known_sources) == 0) " (none registered yet)" else ""
    )
  }
  registry <- registry[intersect(names(registry), only_sources)]
}

if (length(skip_sources) > 0) {
  unknown <- setdiff(skip_sources, all_known_sources)
  if (length(unknown) > 0) {
    stop(
      "--skip referenced source(s) with no registered resolver: ",
      paste(unknown, collapse = ", ")
    )
  }
  registry <- registry[setdiff(names(registry), skip_sources)]
}

cat(sprintf(
  "build_manifest.R: %d resolver(s) registered, %d selected to run.\n",
  length(resolver_names), length(registry)
))
if (length(skip_sources) > 0) {
  cat("Skipped by --skip:", paste(skip_sources, collapse = ", "), "\n")
}

read_manifest_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE,
    lazy = FALSE
  )
}

old <- read_manifest_csv(MANIFEST_PATH)
candidate <- old

seed_new_row <- function(candidate, survey, dataset) {
  siblings <- candidate[candidate$survey == survey & candidate$dataset == dataset, ]
  value_cols <- setdiff(MANIFEST_ALL_COLS, KEY_COLS)
  vals <- lapply(value_cols, function(col) {
    v <- unique(siblings[[col]][!is.na(siblings[[col]]) & nzchar(siblings[[col]])])
    if (length(v) == 1) v else NA_character_
  })
  names(vals) <- value_cols
  c(list(survey = survey, dataset = dataset, geo_level = NA_character_, year = NA_character_), vals)
}

resolver_ok <- character(0)
resolver_failed <- list()
resolver_partial_failed <- list()

for (src in names(registry)) {
  cat("Running resolver:", src, "... ")
  owned_rows <- old[!is.na(old$resolver) & old$resolver == src, ]

  result <- tryCatch(
    registry[[src]](owned_rows),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    cat("FAILED:", conditionMessage(result), "\n")
    resolver_failed[[src]] <- conditionMessage(result)
    next
  }
  if (!is.data.frame(result) || nrow(result) == 0) {
    cat("FAILED: resolver returned no rows\n")
    resolver_failed[[src]] <- "resolver returned an empty/invalid result"
    next
  }

  p_fails <- attr(result, "partial_failures")
  if (!is.null(p_fails) && length(p_fails) > 0) {
    resolver_partial_failed[[src]] <- p_fails
  }

  # Verify that non-exempt owned rows received an available_time
  unrefreshed <- available_time_not_refreshed(owned_rows, result)
  if (length(unrefreshed) > 0) {
    unref_msg <- paste("missing available_time for:", paste(unrefreshed, collapse = ", "))
    cat("WARNING (partial failure):", unref_msg, "\n")
    resolver_partial_failed[[src]] <- c(resolver_partial_failed[[src]], unref_msg)
  }

  for (i in seq_len(nrow(result))) {
    r <- result[i, ]
    r_geo <- if ("geo_level" %in% names(r)) r$geo_level[[1]] else NA_character_
    r_year <- if ("year" %in% names(r)) r$year[[1]] else NA_character_

    geo_matches <- if (is.na(r_geo)) is.na(candidate$geo_level) else (!is.na(candidate$geo_level) & candidate$geo_level == r_geo)
    year_matches <- if (is.na(r_year)) is.na(candidate$year) else (!is.na(candidate$year) & candidate$year == r_year)
    dataset_matches <- candidate$dataset == r$dataset[[1]]
    survey_matches <- candidate$survey == r$survey[[1]]

    match_idx <- which(survey_matches & dataset_matches & geo_matches & year_matches)
    cols_present <- intersect(names(r), MANIFEST_ALL_COLS)

    if (length(match_idx) == 0) {
      new_row <- seed_new_row(candidate, r$survey, r$dataset)
      new_row[cols_present] <- r[cols_present]
      new_row$resolver <- src
      candidate <- dplyr::bind_rows(candidate, tibble::as_tibble(new_row))
    } else {
      non_na_cols <- cols_present[!is.na(r[1, cols_present])]
      candidate[match_idx[1], non_na_cols] <- r[1, non_na_cols]
    }
  }

  cat("OK (", nrow(result), "row(s) touched)\n")
  resolver_ok <- c(resolver_ok, src)
}

candidate <- dplyr::arrange(
  candidate, survey,
  dplyr::coalesce(dataset, ""), dplyr::coalesce(geo_level, ""), dplyr::coalesce(year, "")
)

errors <- c(
  validate_schema(candidate),
  validate_row_count(old, candidate),
  validate_no_deletions(old, candidate),
  validate_row_grouping(candidate),
  validate_dataset_level_agreement(candidate),
  validate_placeholders(old, candidate),
  validate_key_completeness(candidate)
)

changes <- detect_changes(old, candidate)

if (length(errors) == 0 && changes$n_changed > 0) {
  errors <- c(errors, validate_http(candidate, changes$changed_keys))
}

manifest_health_report <- function(candidate) {
  broken <- list()
  non_exempt <- candidate[!vapply(seq_len(nrow(candidate)), function(i) is_http_exempt(candidate[i, ]), logical(1)), ]

  for (u in unique(non_exempt$url)) {
    probe <- probe_url(u)
    if (isTRUE(probe$ok)) next
    offenders <- non_exempt[non_exempt$url == u, ]
    for (i in seq_len(nrow(offenders))) {
      row <- offenders[i, ]
      broken[[length(broken) + 1]] <- list(
        survey = row$survey, dataset = row$dataset,
        geo_level = row$geo_level, year = row$year,
        url = row$url,
        reason = if (isFALSE(probe$ok)) probe$reason else "probe skipped"
      )
    }
  }
  broken
}

health <- if (check_all) manifest_health_report(candidate) else NULL

fragility_matching <- list()
if (changes$n_changed > 0) {
  for (src_item in FRAGILITY_SOURCES) {
    if (any(startsWith(changes$changed_keys, src_item$changed_prefix))) {
      notes <- collect_fragility_notes(file.path(repo_root, src_item$r_path), src_item$label)
      if (length(notes) > 0) {
        fragility_matching[[src_item$label]] <- notes
      }
    }
  }
}

report <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  dry_run = dry_run,
  resolvers_run = names(registry),
  resolvers_skipped = skip_sources,
  resolvers_ok = resolver_ok,
  resolvers_failed = resolver_failed,
  resolvers_partial_failed = resolver_partial_failed,
  rows_old = nrow(old),
  rows_candidate = nrow(candidate),
  n_changed = changes$n_changed,
  changed_keys = changes$changed_keys,
  fragility_notes = fragility_matching,
  errors = errors,
  health = health
)

cat("----------------------------------------\n")
cat(sprintf("Rows: %d (old: %d) | Changed keys: %d\n", nrow(candidate), nrow(old), changes$n_changed))
if (changes$n_changed > 0) {
  cat("Changed keys:\n")
  for (k in changes$changed_keys) cat("  *", gsub("\r", " / ", k), "\n")
}
if (length(fragility_matching) > 0) {
  cat("Fragile lines to review in treatment code:\n")
  for (lbl in names(fragility_matching)) {
    for (note in fragility_matching[[lbl]]) cat("  *", note, "\n")
  }
}
if (length(errors) > 0) {
  cat("Validation ERRORS:\n")
  for (e in errors) cat("  [!] ", e, "\n")
}

candidate_csv_path <- file.path(OUT_DIR, "candidate_manifest.csv")
report_json_path <- file.path(OUT_DIR, "manifest_report.json")

readr::write_csv(candidate, candidate_csv_path, na = "")
jsonlite::write_json(report, report_json_path, pretty = TRUE, auto_unbox = TRUE)

cat("Candidate manifest written to:", candidate_csv_path, "\n")
cat("Manifest report written to:", report_json_path, "\n")

total_failed <- length(resolver_failed) + length(resolver_partial_failed)

exit_code <- if (length(errors) > 0) {
  20L
} else if (changes$n_changed > 0 && total_failed == 0) {
  10L
} else if (changes$n_changed > 0 && total_failed > 0) {
  11L
} else if (total_failed > 0) {
  30L
} else {
  0L
}

gh_output_path <- Sys.getenv("GITHUB_OUTPUT", "")
if (nzchar(gh_output_path)) {
  con <- file(gh_output_path, open = "a")
  writeLines(enc2utf8(c(
    sprintf("candidate_path=%s", candidate_csv_path),
    sprintf("report_path=%s", report_json_path),
    sprintf("n_changed=%d", changes$n_changed),
    sprintf("exit_code=%d", exit_code)
  )), con, useBytes = TRUE)
  close(con)
}

cat("Exit code:", exit_code, "\n")
quit(status = exit_code, save = "no")
