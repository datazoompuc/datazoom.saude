# Loads outpatient procedure data from Brazil provided by 'SIASUS' (Ambulatory Information System)

This function downloads and organizes public health data from 'SIASUS' –
the Ambulatory Information System from Brazil's national health
database, 'DATASUS'. It contains records of outpatient medical
procedures performed throughout the country.

## Usage

``` r
load_outpatient_procedures(
  dataset,
  time_period,
  states = "all",
  raw_data = FALSE,
  language = "eng"
)
```

## Arguments

- dataset:

  A string indicating the type of 'SIASUS' dataset to download. Accepted
  values include: `"bariatric_surgery"`, `"diverse_reports"`,
  `"medicines"`, `"nephrology"`, `"ambulatory_production"`,
  `"psychosocial"`, `"bariatric_surgery_follow_up"`,
  `"fistula_confection"`, `"dialytic_treatment"`, `"home_care"`. See the
  'Details' section for descriptions.

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

- language:

  A string indicating the desired language of variable names and labels.
  Accepts `"eng"` (default) for English or `"pt"` for Portuguese.

## Value

A data frame containing outpatient procedure records from SIASUS for the
specified period and states.

## Details

SIASUS provides multiple datasets that cover different aspects of
outpatient care in Brazil:

- bariatric_surgery_follow_up:

  General preventive and primary care procedures, such as check-ups and
  vaccinations.

- diverse_reports:

  Outpatient dental procedures including extractions, restorations, and
  preventive services.

- medicines:

  Higher complexity procedures including specialized consultations and
  diagnostic exams.

- nephrology:

  Procedures related to early-life screening tests, including metabolic
  and sensory testing.

- chemotherapy:

  Outpatient procedures provided through structured home care programs.

- radiotherapy:

  Procedures focused on physical, cognitive, and functional
  rehabilitation.

- ambulatory_production:

  High-cost procedures that require prior authorization, such as cancer
  treatment or dialysis.

- psychosocial:

  Aggregated or simplified records of outpatient procedures with limited
  detail.

- post_bariatric_surgery_follow_up:

  Records from specialized dental centers providing oral health care.

- fistula_confection:

  Data on medication dispensing and pharmaceutical consultations.

- dialytic_treatment:

  Procedures related to therapies such as chemotherapy and radiotherapy.

- home_care:

  Home-based specialized care such as oxygen therapy and related
  services.

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
load_outpatient_procedures(dataset = "diverse_reports",
                           time_period = 2016,
                           states = "PI",
                           language = "pt")
}
```
