### Automated Dataset Manifest Update

- **Changed keys:** 1

  - `datasus / datasus_cnes_ee / NA / NA`

### ⚠️ Fragile Treatment Lines to Review:

- R/hospital_beds.R:96 -- Assumes 2-letter state abbreviation is at positions 3:4 of CNES filename.
- R/hospital_beds.R:103 -- Assumes 2-digit year is at positions 5:6 followed by 2-digit month 7:8.
- R/hospital_beds.R:143 -- Hardcodes '20' century prefix for CNES file years.
