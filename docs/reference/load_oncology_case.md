# Loads oncology case data from Brazil provided by DATASUS

This function downloads and organizes data from the Oncology Panel
(Painel de Oncologia), part of DATASUS, used in public health and
epidemiological analyses.

## Usage

``` r
load_oncology_case(time_period, raw_data = FALSE, language = "eng")
```

## Arguments

- time_period:

  A numeric value or vector indicating the year(s) of the data to be
  downloaded. For example, `2020` or `2015:2020`.

- raw_data:

  Logical. If `TRUE`, returns the raw data exactly as provided by
  DATASUS. If `FALSE` (default), returns a cleaned and standardized
  version of the dataset.

- language:

  A string indicating the desired language of variable names and labels.
  Accepts `"eng"` (default) for English or `"pt"` for Portuguese.

## Value

A data table containing oncology case records from the Oncology Panel of
all Brazilian states for the specified period.

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
# Download processed oncology data for the year 2023.
# This will return data from the Oncology Panel for all Brazilian states.
oncology_cases_treated <- load_oncology_case(time_period = 2023,
                                             raw_data = FALSE,
                                             language = "eng"
                                             )
}
```
