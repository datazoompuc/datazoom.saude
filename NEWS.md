# datazoom.saude (development version)

* Download URLs now live in a CSV manifest (`inst/extdata/manifest/v1/datasets_link.csv`) instead of being hardcoded. At runtime the package reads the latest copy from GitHub, so a fixed link reaches users without a new CRAN release, and falls back silently to the copy shipped with the package when offline. Set `options(datazoom.saude.use_remote_manifest = FALSE)` to always use the packaged copy.
* Corrected and gap-aware available year ranges across DATASUS datasets to reflect actual upstream FTP holdings:
  * `datasus_sinasc`: available from 1996 to 2024 (pre-1996 files removed upstream).
  * `datasus_po`: available 2013-2026.
  * SIM subsystems (`datasus_sim_dofet`, `doext`, `doinf`, `domat`): available 1996-2025.
  * `datasus_sih` and `datasus_siasus_pa`: pre-2008 years removed from active directories (now 2008-2026).
  * `datasus_siasus_sad`: available 2012-2018.
  * `datasus_siasus_ab`: gap-aware range `"2008-2017, 2025"` (reflecting missing 2018-2024 files on FTP).
  * `datasus_cnes_ee`: gap-aware range `"2007-2019, 2021"` (missing year 2020 on FTP).
  `check_params()` now validates against these empirical ranges upfront, rejecting invalid years with clear error messages before attempting FTP downloads.
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
