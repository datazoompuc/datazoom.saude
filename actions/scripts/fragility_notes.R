# actions/scripts/fragility_notes.R
#
# Generic mechanism for collecting "# FRAGILE:" tags in treatment code
# and reporting them to PR reviewers when a manifest row changes.

collect_fragility_notes <- function(r_path, label) {
  if (!file.exists(r_path)) return(character(0))
  lines <- readLines(r_path, warn = FALSE)
  is_comment <- grepl("^\\s*#", lines)
  tag_lines <- grep("^\\s*#\\s*FRAGILE:", lines)

  notes <- character(0)
  for (start in tag_lines) {
    end <- start
    while (end + 1 <= length(lines) && is_comment[end + 1]) end <- end + 1
    anchor <- end + 1
    while (anchor <= length(lines) && !nzchar(trimws(lines[anchor]))) anchor <- anchor + 1
    if (anchor > length(lines)) anchor <- end

    text <- sub("^\\s*#\\s*", "", lines[start:end])
    text <- sub("^FRAGILE:\\s*", "", text)
    text <- paste(text, collapse = " ")

    notes <- c(notes, sprintf("%s:%d -- %s", label, anchor, text))
  }
  notes
}

FRAGILITY_SOURCES <- list(
  list(changed_prefix = "datasus\rdatasus_sinasc", r_path = file.path("R", "births.R"), label = "R/births.R", treat_fn = "load_births()"),
  list(changed_prefix = "datasus\rdatasus_po", r_path = file.path("R", "oncology_case.R"), label = "R/oncology_case.R", treat_fn = "load_oncology_case()"),
  list(changed_prefix = "datasus\rdatasus_sim", r_path = file.path("R", "mortality.R"), label = "R/mortality.R", treat_fn = "load_mortality()"),
  list(changed_prefix = "datasus\rdatasus_cnes", r_path = file.path("R", "hospital_beds.R"), label = "R/hospital_beds.R", treat_fn = "load_hospital_beds()"),
  list(changed_prefix = "datasus_sih\rdatasus_sih", r_path = file.path("R", "hospital_admissions.R"), label = "R/hospital_admissions.R", treat_fn = "load_hospital_admissions()"),
  list(changed_prefix = "datasus_siasus\rdatasus_siasus", r_path = file.path("R", "outpatient_procedures.R"), label = "R/outpatient_procedures.R", treat_fn = "load_outpatient_procedures()"),
  list(changed_prefix = "datasus\rdatasus_sipni", r_path = file.path("R", "web_scraping_pni.R"), label = "R/web_scraping_pni.R", treat_fn = "load_pni()")
)
