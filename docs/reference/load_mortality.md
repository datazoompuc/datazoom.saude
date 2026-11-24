# Load Brazilian mortality data from the SIM system via DATASUS

Retrieves mortality records from Brazil's official Mortality Information
System (SIM), made available through DATASUS (Department of Informatics
of the Brazilian Unified Health System). This dataset includes detailed
information on deaths by cause, location, sex, age, and other
demographic characteristics. Data is useful for public health research,
epidemiology, and demographic analysis.

## Usage

``` r
load_mortality(
  dataset,
  time_period,
  states = "all",
  raw_data = FALSE,
  keep_all = FALSE,
  language = "eng"
)
```

## Arguments

- dataset:

  A string identifying the specific mortality dataset to download.
  Accepted values are:

  - `"general"`: General death records

  - `"fetal"`: Fetal deaths

  - `"external_causes"`: Deaths by external causes

  - `"infant"`: Infant deaths

  - `"maternal"`: Maternal deaths

- time_period:

  A numeric value or vector indicating the year(s) of the data to be
  downloaded. For example, `2020` or `2015:2020`.

- states:

  A string or vector of strings indicating the Brazilian state(s) for
  which the data should be downloaded. Use `"all"` to download data for
  the entire country. For specific states, use abbreviations like
  `"SP"`, `"RJ"`, or `c("SP", "RJ")`.

- raw_data:

  Logical. If `TRUE`, returns the raw data exactly as provided by
  DATASUS. If `FALSE` (default), returns a cleaned and standardized
  version of the dataset.

- keep_all:

  A `boolean` choosing whether to aggregate the data by municipality,
  losing individual-level variables (`FALSE`) or to keep all original
  variables (`TRUE`). Only applies when `raw_data` is `FALSE`.

- language:

  A string indicating the desired language of variable names and labels.
  Accepts `"eng"` (default) for English or `"pt"` for Portuguese.

## Value

A data frame containing the mortality records.

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
load_mortality(dataset = "general",
               time_period = 2022,
               states = "RJ")

load_mortality(dataset = "maternal",
               time_period = 2020,
               raw_data = FALSE,
               language = "pt")
}
```
