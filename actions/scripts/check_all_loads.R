# actions/scripts/check_all_loads.R
#
# Regression harness: executes real loader calls against the current branch and
# compares returned data against the 'main' branch to prove end-to-end data parity
# across the manifest-driven architecture migration.
#
# Usage:
#   Rscript actions/scripts/check_all_loads.R [--no-compare-main]

args <- commandArgs(trailingOnly = TRUE)
compare_main <- !("--no-compare-main" %in% args)

repo_root <- getwd()

test_cases <- list(
  list(
    name = "load_births (SINASC)",
    call = "load_births(time_period = 2020, state = 'AC', raw_data = TRUE)"
  ),
  list(
    name = "load_oncology_case (Painel de Oncologia)",
    call = "load_oncology_case(time_period = 2020, raw_data = TRUE)"
  ),
  list(
    name = "load_mortality (SIM Mortalidade Geral)",
    call = "load_mortality(dataset = 'datasus_sim_do', time_period = 2020, state = 'AC', raw_data = TRUE)"
  ),
  list(
    name = "load_hospital_beds (CNES Leitos)",
    call = "load_hospital_beds(time_period = 2020, state = 'AC', month = 1, raw_data = TRUE)"
  ),
  list(
    name = "load_hospital_admissions (SIHSUS RD)",
    call = "load_hospital_admissions(dataset = 'datasus_sih_rd', time_period = 2020, state = 'AC', month = 1, raw_data = TRUE)"
  ),
  list(
    name = "load_outpatient_procedures (SIASUS PA)",
    call = "load_outpatient_procedures(dataset = 'datasus_siasus_pa', time_period = 2020, state = 'AC', month = 1, raw_data = TRUE)"
  )
)

run_in_dir <- function(target_dir, call_str) {
  out_rds <- tempfile(fileext = ".rds")
  rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  
  expr <- sprintf("
    suppressPackageStartupMessages(devtools::load_all('%s', quiet = TRUE))
    res <- tryCatch(%s, error = function(e) e)
    saveRDS(res, '%s')
  ", gsub("\\\\", "/", target_dir), call_str, gsub("\\\\", "/", out_rds))

  out <- suppressWarnings(system2(rscript_bin, c("-e", shQuote(expr)), stdout = TRUE, stderr = TRUE))
  
  if (file.exists(out_rds)) {
    val <- readRDS(out_rds)
    unlink(out_rds)
    val
  } else {
    structure(list(message = paste(out, collapse = "\n")), class = c("error", "condition"))
  }
}

cat("=======================================================================\n")
cat("Starting regression run comparing format-migration against main branch\n")
cat("=======================================================================\n\n")

main_dir <- NULL
if (compare_main) {
  main_dir <- file.path(tempdir(), "datazoom_saude_main_worktree")
  unlink(main_dir, recursive = TRUE, force = TRUE)
  cat("[*] Setting up temporary git worktree for 'main' branch ...\n")
  wt_res <- system2("git", c("worktree", "add", shQuote(main_dir), "main"), stdout = TRUE, stderr = TRUE)
  if (!dir.exists(main_dir)) {
    cat("Warning: could not create worktree for main. Running branch-only checks.\n")
    compare_main <- FALSE
  }
}

results <- list()
all_match <- TRUE

for (tc in test_cases) {
  cat(sprintf("[*] Testing %s\n    Expression: %s\n", tc$name, tc$call))
  
  t0 <- proc.time()
  res_branch <- run_in_dir(repo_root, tc$call)
  elapsed_branch <- (proc.time() - t0)["elapsed"]

  if (inherits(res_branch, "error")) {
    cat(sprintf("    Branch: FAILED (%.2fs): %s\n", elapsed_branch, conditionMessage(res_branch)))
    results[[tc$name]] <- list(status = "FAILED", error = conditionMessage(res_branch))
    all_match <- FALSE
    next
  }

  cat(sprintf("    Branch: OK (%.2fs, %d rows, %d cols)\n", elapsed_branch, nrow(res_branch), ncol(res_branch)))

  if (compare_main) {
    t0 <- proc.time()
    res_main <- run_in_dir(main_dir, tc$call)
    elapsed_main <- (proc.time() - t0)["elapsed"]

    if (inherits(res_main, "error")) {
      cat(sprintf("    Main:   FAILED (%.2fs): %s\n", elapsed_main, conditionMessage(res_main)))
      results[[tc$name]] <- list(status = "MAIN_FAILED", error = conditionMessage(res_main))
      next
    }

    cat(sprintf("    Main:   OK (%.2fs, %d rows, %d cols)\n", elapsed_main, nrow(res_main), ncol(res_main)))

    eq <- all.equal(res_branch, res_main)
    if (isTRUE(eq)) {
      cat("    Comparison: MATCH (exact data frame equality with main)\n\n")
      results[[tc$name]] <- list(status = "MATCH", rows = nrow(res_branch), cols = ncol(res_branch))
    } else {
      cat("    Comparison: MISMATCH!\n")
      for (diff_line in head(eq, 5)) cat("      - ", diff_line, "\n")
      cat("\n")
      results[[tc$name]] <- list(status = "MISMATCH", diff = eq)
      all_match <- FALSE
    }
  } else {
    results[[tc$name]] <- list(status = "OK", rows = nrow(res_branch), cols = ncol(res_branch))
    cat("\n")
  }
}

if (!is.null(main_dir) && dir.exists(main_dir)) {
  cat("[*] Cleaning up temporary git worktree ...\n")
  suppressWarnings(system2("git", c("worktree", "remove", shQuote(main_dir), "--force"), stdout = FALSE, stderr = FALSE))
}

cat("=======================================================================\n")
cat("Regression run summary:\n")
for (nm in names(results)) {
  cat(sprintf("  %-45s : %s\n", nm, results[[nm]]$status))
}
cat("=======================================================================\n")
