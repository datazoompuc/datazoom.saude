# Load Brazilian vaccination data from the SI-PNI system via DATASUS

Retrieves vaccination records from Brazil's official National
Immunization Program Information System (SI-PNI), made available through
DATASUS. This function supports automated retrieval across all supported
years: for historical data (1994-2023), it queries the legacy SI-PNI Web
portal via HTTP; from 2024 onwards, it queries the new DATASUS
interactive dashboard (`infoms.saude.gov.br`) directly. Alternatively,
the user may provide a locally downloaded file via `data`, in which case
the function performs only validation, cleaning, harmonization, and
standardization.

## Usage

``` r
load_vaccines(
  year,
  state,
  strategy = NULL,
  product = NULL,
  dose = NULL,
  data = NULL,
  language = "eng"
)
```

## Arguments

- year:

  A numeric value indicating the year of the data to be downloaded (1994
  to present).

- state:

  A string indicating the Brazilian state abbreviation (e.g., "SP",
  "RJ", "AC").

- strategy:

  A string identifying the vaccination strategy (e.g., "Rotina",
  "Especial", "Bloqueio"). If NULL and in an interactive session, a menu
  will appear.

- product:

  A string identifying the specific vaccine product (e.g., "BCG - BCG",
  "Hepatite B - HB"). For 2024 onwards, both legacy names (e.g., "BCG -
  BCG") and new dashboard names (e.g., "87 - vacina covid-19-RNAm,
  Pfizer (Comirnaty)") are accepted. If NULL and in an interactive
  session, a menu will appear.

- dose:

  A character vector indicating the dose categories that were selected
  when downloading the data. This argument is only required when using
  manually downloaded files up to 2022.

- data:

  An optional path to a local Excel (.xls/.xlsx) file downloaded
  manually from the official DATASUS vaccination dashboard. If provided,
  automated retrieval is skipped and the function performs only data
  cleaning and harmonization.

- language:

  A string indicating the desired language for variable names and
  labels. Accepts "eng" (default) or "pt".

## Value

A tibble (data frame) containing state, year, municipality codes,
municipality names, strategy, product, month, dose type, and the
quantity of doses applied.

## Details

Regardless of the ingestion mode, the returned dataset is fully
harmonized and consistent across years (1994 to present).

## Examples

``` r
if (FALSE) { # interactive()
# Example: Loading Yellow Fever vaccine data for Acre in 2020
data <- load_vaccines(
  year = 2020,
  state = "AC",
  strategy = "Rotina",
  product = "Febre amarela - FA",
  language = "eng"
)
}
```
