## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a resubmission addressing reviewer feedback (Konstanze Lauseker, 2026-07-21)

Changes made in response to CRAN feedback on 0.1.1:

* Added the DATASUS webservice URL to the Description field in the required format: <https://datasus.saude.gov.br/>
* Fixed read.dbc() not being exported: a duplicated @export roxygen tag was preventing the NAMESPACE from being generated correctly. This also resolved the "example for unexported function" issue in read.dbc.Rd.
* Replaced \dontrun{} with \donttest{} for all examples that download data from DATASUS. These examples are executable, just dependent on network access.
* Reduced the data volume in several examples (load_hospital_beds, load_oncology_case, load_outpatient_procedures) to avoid long execution times and network timeouts during automated checks.
* Fixed a missing dplyr:: namespace prefix on a rename() call in load_mortality(), which caused an error when the example was actually executed (previously masked by \dontrun{}).
* Added on.exit() calls to restore user options (timeout, download.file.method, download.file.extra) that are modified within external_download() and load_pni(), as required by CRAN policies on changing user options/working directory.
* Added Daniela Petruzalek and Mark Adler as contributors (ctb) in Authors@R, as they are the original authors of the bundled DBC-decompression code (dbc2dbf.R, read.dbc.R), previously acknowledged only in a LICENSE.note file.

## Test environments

* local macOS install (x86_64-apple-darwin20), R 4.5.1 -- 0 errors | 0 warnings | 0 notes (including --run-donttest)
* win-builder (R-devel) -- pending (awaiting results)

## Notes

* NOTE: Possibly misspelled words in DESCRIPTION (CNES, DATASUS, SIASUS, SIH, SINASC, SUS) -- these are all legitimate and widely-used acronyms for Brazilian public health information systems. They are not spelling errors.

## References

* This package does not implement statistical methods requiring references. It provides tools to download and standardize public health data from DATASUS.
