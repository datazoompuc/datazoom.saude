# datazoom.saude (development version)

* Download URLs now live in a CSV manifest (`inst/extdata/manifest/v1/datasets_link.csv`) instead of being hardcoded. At runtime the package reads the latest copy from GitHub, so a fixed link reaches users without a new CRAN release, and falls back silently to the copy shipped with the package when offline. Set `options(datazoom.saude.use_remote_manifest = FALSE)` to always use the packaged copy.
* Removed unused dependencies: `data.table`, `sf` and `XML` from Imports; `googledrive`, `terra` and `units` from Suggests.

# datazoom.saude 0.1.1

* Fixed non-ASCII characters in R source files
* Added missing imports (jsonlite, tidyr, stats) to DESCRIPTION
* Excluded hex_dzsaude.png from build
* Fixed global variable binding notes
* Generated missing documentation for load_vaccines()
* Fixed typo in dbc2dbf.R (succeded -> succeeded)
* Updated Title and Description in DESCRIPTION
* Updated institutional URL to datazoom.com.br/en/dz_saude/
* Added CRAN install instructions to README
* Added @export to dbc2dbf_wrapper and read.dbc
* Added LICENSE.note for bundled third-party code
* Updated LICENSE year to 2026
* Added BugReports field to DESCRIPTION
