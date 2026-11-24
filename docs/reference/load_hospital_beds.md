# Load monthly data on hospital beds from Brazil's National Health Facilities Registry

Retrieves and processes structured microdata on hospital beds from the
'CNES' (Cadastro Nacional de Estabelecimentos de Saude), which is part
of the 'DATASUS' system, maintained by the Brazilian Ministry of Health.
This dataset includes information on the availability and distribution
of hospital beds in Brazil.

## Usage

``` r
load_hospital_beds(
  time_period,
  states = "all",
  raw_data = FALSE,
  language = "eng"
)
```

## Arguments

- time_period:

  A character vector of years (e.g., `c("2019", "2020")`) for which data
  should be loaded.

- states:

  A character vector of state abbreviations (e.g., `c("RJ", "SP")`) to
  filter the data. Use `"all"` to include all states. Defaults to
  `"all"`.

- raw_data:

  Logical. If `TRUE`, returns a list of raw data frames (one per file).
  If `FALSE`, returns a cleaned and labeled data frame. Default is
  `FALSE`.

- language:

  Character string indicating the variable label language. Options are
  `"pt"` for Portuguese or `"eng"` for English. Default is `"eng"`.

## Value

A tibble (if `raw_data = FALSE`) containing harmonized monthly hospital
bed data from Brazil, or a list of raw data frames (if
`raw_data = TRUE`).

## Details

The data is sourced from the 'CNES-LT' subsystem (Leitos - Beds), which
provides monthly information on beds by establishment and type, and can
be filtered by state and time period.

File downloads are performed via the official DATASUS FTP server. Users
can choose to retrieve raw or pre-processed data, and label variables in
English or Portuguese.

This function downloads and processes data from the CNES-LT subsystem,
which is part of the Brazilian National Health Facilities Registry. It
uses the DATASUS FTP server and handles compressed data in DBF format,
using packages such as `foreign` and `RCurl`.

The function labels variables using a built-in dictionary and offers
harmonized column names depending on the selected language. A filter by
state and year is applied to reduce file size and processing time.

## Note

The `foreign` and `RCurl` packages must be installed to use this
function. They are listed in `Suggests` to avoid installing them by
default.

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
# Load beds data for RO in 2020
beds <- load_hospital_beds(time_period = "2020",
                           states = "RO",
                           raw_data = FALSE,
                           language = "eng")
}
```
