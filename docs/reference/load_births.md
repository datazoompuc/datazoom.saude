# Loads birth data of newborns from Brazil provided by DATASUS

This function downloads and organizes data from SINASC (Live Birth
Information System), part of DATASUS, used in public health analyses.

## Usage

``` r
load_births(time_period, states = "all", raw_data = FALSE, language = "eng")
```

## Arguments

- time_period:

  A numeric value or vector indicating the year(s) of the data to be
  downloaded. For example, `2020` or `2015:2020`.

- states:

  A string or vector of strings indicating the Brazilian state(s) for
  which the data should be downloaded. Use `"all"` (by default) to
  download data for the entire country. For specific states, use
  abbreviations like `"SP"`, `"RJ"`, or `c("SP", "RJ")`.

- raw_data:

  Logical. If `TRUE`, returns the raw data exactly as provided by
  DATASUS. If `FALSE` (default), returns a cleaned and standardized
  version of the dataset.

- language:

  A string indicating the desired language of variable names and labels.
  Accepts `"eng"` (default) for English or `"pt"` for Portuguese.

## Value

A data frame containing birth records from SINASC for the specified
period and states.

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
load_births(time_period = 2023,
            states = "RJ",
            raw_data = FALSE)
}
```
