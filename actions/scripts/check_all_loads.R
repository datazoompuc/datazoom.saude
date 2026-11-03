# actions/scripts/check_all_loads.R
#
# Regression harness: executes real loader calls against the manifest-driven
# architecture to verify that actual FTP downloads and data structures work
# end-to-end.
#
# Usage:
#   Rscript actions/scripts/check_all_loads.R

devtools::load_all()

test_cases <- list(
  list(
    name = "load_births (SINASC)",
    fn = function() load_births(dataset = "datasus_sinasc", time_period = 2020, state = "AC", raw_data = TRUE)
  ),
  list(
    name = "load_oncology_case (Painel de Oncologia)",
    fn = function() load_oncology_case(time_period = 2020, raw_data = TRUE)
  ),
  list(
    name = "load_mortality (SIM Mortalidade Geral)",
    fn = function() load_mortality(dataset = "datasus_sim_do", time_period = 2020, state = "AC", raw_data = TRUE)
  ),
  list(
    name = "load_hospital_beds (CNES Leitos)",
    fn = function() load_hospital_beds(time_period = 2020, state = "AC", month = 1, raw_data = TRUE)
  ),
  list(
    name = "load_hospital_admissions (SIHSUS RD)",
    fn = function() load_hospital_admissions(dataset = "datasus_sih_rd", time_period = 2020, state = "AC", month = 1, raw_data = TRUE)
  ),
  list(
    name = "load_outpatient_procedures (SIASUS PA)",
    fn = function() load_outpatient_procedures(dataset = "datasus_siasus_pa", time_period = 2020, state = "AC", month = 1, raw_data = TRUE)
  )
)

cat("=======================================================\n")
cat("Starting live regression run for datazoom.saude loaders\n")
cat("=======================================================\n\n")

results <- list()
all_passed <- TRUE

for (tc in test_cases) {
  cat(sprintf("[*] Testing %s ... ", tc$name))
  t0 <- proc.time()
  res <- tryCatch(tc$fn(), error = function(e) e)
  elapsed <- (proc.time() - t0)["elapsed"]

  if (inherits(res, "error")) {
    cat(sprintf("FAILED (%.2fs): %s\n", elapsed, conditionMessage(res)))
    results[[tc$name]] <- list(status = "FAILED", error = conditionMessage(res), elapsed = elapsed)
    all_passed <- FALSE
  } else if (!is.data.frame(res) || nrow(res) == 0) {
    cat(sprintf("FAILED (%.2fs): returned empty or non-data.frame result\n", elapsed))
    results[[tc$name]] <- list(status = "FAILED", error = "empty result", elapsed = elapsed)
    all_passed <- FALSE
  } else {
    cat(sprintf("OK (%.2fs, %d rows, %d cols)\n", elapsed, nrow(res), ncol(res)))
    results[[tc$name]] <- list(status = "OK", rows = nrow(res), cols = ncol(res), elapsed = elapsed)
  }
}

cat("\n=======================================================\n")
cat(sprintf("Regression run summary: %d/%d passed\n", sum(vapply(results, function(r) r$status == "OK", logical(1))), length(test_cases)))
cat("=======================================================\n")

if (!all_passed) {
  cat("\nNote: Live network errors on gov FTP servers can happen intermittently.\n")
}
