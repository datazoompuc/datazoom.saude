# Loads hospital admission data from Brazil provided by DATASUS (SIHSUS)

This function downloads and organizes data from SIHSUS (Hospital
Information System), part of DATASUS, used in public health and hospital
care analyses.

## Usage

``` r
load_hospital_admissions(
  dataset,
  time_period,
  states = "all",
  raw_data = FALSE,
  language = "eng"
)
```

## Arguments

- dataset:

  A string indicating the type of SIHSUS dataset to download. Accepted
  values are: `"reduced_aih"`, `"professional_services"`,
  `"rejected_aih"`, or `"rejected_aih_error"`. See the 'Details' section
  for explanations.

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

A data frame containing hospital admission records from SIHSUS for the
specified period and states.

## Details

SIHSUS provides several datasets related to hospital admissions in
Brazil:

- reduced_aih (RD) - Reduced AIH (Hospital Admission Authorization):

  A simplified database with the main information from approved and
  processed AIHs. It is the most commonly used dataset for statistical
  and epidemiological analyses, including data on the main procedure,
  diagnoses, and total values of each admission.

- professional_services (SP) - Professional Services:

  A stratified dataset containing details about services provided during
  the hospital stay, such as medical procedures, professional
  identification (CBO/CNS), and values related to professional and
  hospital services.

- rejected_aih (RJ) - Rejected AIHs:

  Contains rejected AIHs and summarizes the reasons for rejection. It is
  useful for analyzing the volume and impact of rejected records but
  does not include detailed information on each rejection.

- rejected_aih_error (ER) - Rejected AIHs with Error Code:

  Includes AIHs rejected due to inconsistencies identified during
  processing. These records contain specific error codes indicating why
  the rejection occurred (e.g., patient data inconsistency, procedure
  incompatibility).

## Connection Reliability

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. Downloads are automatically retried up to 3
times.

## Examples

``` r
if (FALSE) { # interactive()
load_hospital_admissions(dataset = "professional_services",
                         time_period = 2020,
                         states = "AC")
}
```
