## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a resubmission

Changes since 0.1.0:
* Fixed non-ASCII characters in R source files
* Added missing imports (jsonlite, tidyr, stats)
* Added @export to dbc2dbf_wrapper and read.dbc
* Updated institutional URL to datazoom.com.br/en/dz_saude/
* Added LICENSE.note for bundled third-party code (Daniela Petruzalek, AGPL-3)
* Updated LICENSE year to 2026
* Added BugReports field to DESCRIPTION
* Added CRAN install instructions to README

## Test environments

* local macOS install (x86_64-apple-darwin20), R 4.5.1 -- 0 errors | 0 warnings | 0 notes
* macOS builder (aarch64-apple-darwin23, R-devel) -- 0 errors | 0 warnings | 0 notes
* win-builder (R-devel) -- 0 errors | 0 warnings | 1 note

## Notes

* NOTE: New submission (expected for a first CRAN submission).
* NOTE: Possibly misspelled words in DESCRIPTION (CNES, DATASUS, SIASUS, SIH, SINASC, SUS) -- these are all legitimate and widely-used acronyms for Brazilian public health information systems. They are not spelling errors.

## References

* This package does not implement statistical methods requiring references. It provides tools to download and standardize public health data from DATASUS.
