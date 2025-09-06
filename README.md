
<a href="https://github.com/datazoompuc/datazoom.saude"><img src="man/figures/logo.png" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.saude

<!-- badges: start -->

[![R build
status](https://github.com/datazoompuc/datazoom.saude/workflows/R-CMD-check/badge.svg)](https://github.com/datazoompuc/datazoom.saude/actions)
[![Total de
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/datazoom.saude)](https://cran.r-project.org/package=datazoom.saude)
[![Downloads
mensais](https://cranlogs.r-pkg.org/badges/datazoom.saude)](https://cran.r-project.org/package=datazoom.saude)
[![Linguagens](https://img.shields.io/github/languages/count/datazoompuc/datazoom.saude?style=flat)](https://github.com/datazoompuc/datazoom.saude)
[![Atividade de
Commits](https://img.shields.io/github/commit-activity/y/datazoompuc/datazoom.saude?style=flat)](https://github.com/datazoompuc/datazoom.saude/commits/main)
[![Issues
Abertas](https://img.shields.io/github/issues-raw/datazoompuc/datazoom.saude?style=flat)](https://github.com/datazoompuc/datazoom.saude/issues)
[![Issues
Fechadas](https://img.shields.io/github/issues-closed-raw/datazoompuc/datazoom.saude?style=flat)](https://github.com/datazoompuc/datazoom.saude/issues)
<!-- badges: end -->

The `datazoom.saude` package provides simple, direct, and reliable
functions to import, organize, and explore public health databases in
Brazil. It is part of the **`datazoom`** ecosystem, designed to simplify
access to and analysis of national data.

## Installation

You can install the released version of `datazoom.saude` from the
development version from GitHub.

``` r
# Install the 'devtools' package if you don't have it yet
install.packages("devtools")

# Install datazoom.saude directly from GitHub
devtools::install_github("datazoompuc/datazoom.saude")

devtools::load_all()
```

## Supported Databases

For detailed usage examples and guides on each database, please refer to
the vignettes below.

**[1 - Mortality (SIM)](#mortality)**

**[2 - Live Births (SINASC)](#live-births)**

**[3 - Hospital Admissions (SIH)](#hospital-admissions)**

**[4 - Hospital Beds (CNES-LT)](#hospital-beds)**

**[5 - Outpatient Procedures (SIASUS)](#outpatient-procedures)**

**[6 - Oncology Cases (PO)](#oncology-cases)**

<br>

# Mortality

The `load_mortality` function provides access to the **System of
Mortality Information (SIM)** datasets, which contain detailed
information about deaths in Brazil. Each original SIM data file includes
rows corresponding to a declaration of death (DO) and columns with
several characteristics of the person, the place of death, and the cause
of death.

------------------------------------------------------------------------

The `load_mortality` function offers the following parameters:

1.  **dataset**: Specifies the SIM dataset to download:

    - SIM Datasets:
      - `"general"` – Main Declarations of Death. Contains records of
        all non-fetal Death Certificates (DO) in Brazil, including
        socio-demographic data, location, and causes of death (ICD-10).
        It’s the base for general mortality analysis.
      - `"fetal"` – Fetal mortality data. Contains records of fetal
        deaths, with information on the mother, pregnancy, and causes of
        fetal death. It’s essential for maternal and child health.
      - `"external_causes"` – Mortality data from external causes.
        Contains a subset of `"general"` focusing on deaths due to
        accidents, violence, and other unnatural causes. Used for safety
        and prevention studies.
      - `"infant"` – Infant mortality data (children). Contains a subset
        of `"general"` recording deaths of children under 1 year old,
        detailing causes and birth-related factors. Crucial for
        assessing child health.
      - `"maternal"` – Maternal mortality data. Contains a subset of
        `"general"` for deaths of women during or shortly after
        pregnancy/childbirth, detailing obstetric causes. Important for
        women’s health.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data. Only
      effective for SIM-DO and subsets, SIH, and CNES-LT.

3.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.

4.  **states**: a string or vector of strings indicating the Brazilian
    state(s) for which the data should be downloaded. Use `"all"` to
    download data for the entire country. For specific states (valid
    only for the `general` dataset), use abbreviations like `"SP"` (São
    Paulo), `"RJ"` (Rio de Janeiro), or `c("SP", "RJ")`.

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`.

6.  **keep_all**: A boolean choosing whether to aggregate the data by
    municipality, losing individual-level variables (`FALSE`) or to keep
    all original variables (`TRUE`). Only applies when `raw_data` is
    `FALSE`.

**Examples:**

``` r
library(datazoom.saude)

# Download raw data for the year 2022 in the state of RJ for general mortality.
raw_data_general_rj <- load_mortality(
  dataset = "general",
  time_period = 2022,
  states = "RJ",
  raw_data = TRUE
)

# Download treated data with the number of deaths by cause in RJ, aggregated by municipality and year.
trated_data_general_rj <- load_mortality(
  dataset = "general",
  time_period = 2022,
  states = "RJ",
  raw_data = FALSE,
  keep_all = FALSE # Explicitly stating default behavior
)

# Download treated data for Maternal Deaths (`maternal`) for 2020,
# for the entire country, with descriptions in Portuguese.
# Note: `maternal` does not provide separate files by state.
data_maternal_pt <- load_mortality(
  dataset = "maternal",
  time_period = 2020,
  raw_data = FALSE,
  language = "pt"
)

# Download treated data for Infant Deaths (`infant`) for 2017,
# for all states, keeping all individual variables (not aggregated).
data_infant_full <- load_mortality(
  dataset = "infant",
  time_period = 2017,
  raw_data = FALSE,
  keep_all = TRUE
)
```

# Live Births

The `load_births` function provides access to the **Live Birth
Information System (SINASC)** dataset, which collects and records
detailed information about births in Brazil. This data is extracted from
Live Birth Certificates (DNVs) and includes information about the
newborn, such as sex, weight, and gestational age, as well as data about
the mother, such as age, number of children, and health conditions.
SINASC is essential for monitoring maternal and child health and
generating relevant indicators for public health policy formulation.

------------------------------------------------------------------------

The `load_births` function offers the following parameters:

1.  **time_period**: A numeric value or vector indicating the year(s) of
    the data to be downloaded. For  
    example, 2020 or 2015:2020.

2.  **states**: A string or array of strings indicating the Brazilian
    state(s) for which data should be  
    downloaded. Use “all” (by default) to download data for the entire
    country. For specific states, use abbreviations such as “SP”, “RJ”,
    or c(“SP”, “RJ”).

3.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

4.  **language**: A string indicating the desired language of variable
    names and labels. Accepts “eng” (default) for English or “pt” for
    Portuguese.

**Examples:**

``` r
library(datazoom.saude)

# Download raw birth data for 2023 in the state of Rio de Janeiro (RJ).
data_raw_births <- load_births(
  time_period = 2023,
  states = "RJ"
)

# Download raw birth data for 2020 in the states of Rio de Janeiro (RJ) and São Paulo (SP),
# keeping the original raw format.
data_raw_births2 <- load_births(
  time_period = 2020,
  states = c("RJ","SP"),
  raw_data = TRUE
)

# Download raw birth data for 2014 in the state of Amazonas (AM),
# with variable labels in Portuguese.
data_raw_births3 <- load_births(
  time_period = 2014,
  states = "AM",
  language = "pt"
)

# Download processed birth data for 2015 in the state of Amazonas (AM),
# with variable labels in Portuguese for easier analysis.
data_processed_births <- load_births(
  time_period = 2015,
  states = "AM",
  raw_data = FALSE,
  language = "pt"
)
```

# Hospital Admissions

The `load_hospital_admissions` function provides access to multiple
datasets from the **Hospital Information System (SIH)**, which record
detailed information about hospital admissions funded by Brazil’s public
health system (SUS). Each row corresponds to a Hospital Admission
Authorization (AIH), and the files are organized by the type of
information they contain.

------------------------------------------------------------------------

The `load_hospital_admissions` function offers the following parameters:

1.  **dataset**: Specifies the SIH dataset to download:

    - SIH hospitalization data is split across four datasets:
      - `"reduced_aih"` – Reduced AIHs (summary of hospitalizations).
        Contains consolidated information about approved and processed
        AIHs, including the main procedure performed, related diagnoses,
        and total costs. This is the most commonly used dataset for
        statistical and epidemiological analyses.
      - `"professional_services"` – Professional Services performed
        during hospitalization. Provides detailed records of the
        professional services carried out during hospital stays,
        including procedures performed, professionals involved
        (CBO/CNS), and amounts paid for medical and hospital services.
      - `"rejected_aih"` – Rejected AIHs (general reason). Includes
        consolidated records of AIHs that were rejected, specifying the
        general reason for the rejection but without detailed error
        codes. Useful for analyzing the volume and impact of rejections.
      - `"rejected_aih_error"` – Rejected AIHs with specific error
        codes. Contains AIHs that were rejected due to inconsistencies
        found during processing. Each rejection includes a specific
        error code indicating the reason (e.g., invalid patient data,
        procedure incompatibilities).

2.  **raw_data**: there are two options:

    - `TRUE`: If you want the original DATASUS files in their segmented
      form.
    - `FALSE`: if you want the treated version of the data.

3.  **time_period**: Specifies the years for which the data will be
    downloaded.

4.  **states**: A vector of states by which to filter the data.

5.  **language**: You can choose between Portuguese `("pt")` and English
    `("eng")` for variable descriptions when `raw_data = FALSE`.

**Examples:**

``` r
library(datazoom.saude)

# Download raw data for Reduced AIHs (AIHs Reduzida) – State of Amazonas, 2010.
data_rd_raw <- load_hospital_admissions(
  dataset = "reduced_aih",
  time_period = 2010,
  states = "AM",
  raw_data = TRUE
)

# Download processed data for Rejected AIHs with Error Codes – State of Amazonas, 2010.
data_er_processed <- load_hospital_admissions(
  dataset = "rejected_aih_error",
  time_period = 2010,
  states = "AM",
  raw_data = FALSE
)

# Download raw data for Professional Services – State of Acre, 2010.
data_sp_raw <- load_hospital_admissions(
  dataset = "professional_services",
  time_period = 2010,
  states = "AC",
  raw_data = TRUE
)

# Download processed data for Professional Services – Federal District, 2010.
data_sp_processed <- load_hospital_admissions(
  dataset = "professional_services",
  time_period = 2010,
  states = "DF",
  raw_data = FALSE
)
```

# Hospital Beds

The `load_hospital_beds` function specifically focuses on the **CNES -
LT (Beds)** dataset, part of the National Register of Health
Establishments (CNES). This dataset provides information on the number
of available hospital beds in health establishments across Brazil.

------------------------------------------------------------------------

The `load_hospital_beds` function offers the following parameters:

1.  **raw_data**: there are two options:

    - `TRUE`: If you want the original DATASUS files in their segmented
      form.
    - `FALSE`: if you want the treated version of the data.

2.  **time_period**: Specifies the years for which the data will be
    downloaded.

3.  **states**: A vector of states by which to filter the data.

4.  **language**: You can choose between Portuguese `("pt")` and English
    `("eng")` for variable descriptions when `raw_data = FALSE`.

**Examples:**

``` r
library(datazoom.saude)

# Download treated data with the number of available beds in Amazonas (AM) and Pará (PA).
data_beds_full <- load_hospital_beds(
  time_period = 2010,
  states = c("AM", "PA"),
  raw_data = FALSE
)

# Download treated data with the number of available beds in whole country.
data_beds_full <- load_hospital_beds(
  time_period = 2010,
  states = "all",
  raw_data = FALSE
)

# Download raw data for the number of hospital beds in 2015 for Rio de Janeiro.
data_beds_raw <- load_hospital_beds(
  time_period = 2015,
  states = "RJ",
  raw_data = TRUE
)
```

# Outpatient Procedures

The `load_outpatient_procedures` function provides access to various
**SIASUS (Ambulatory Information System)** datasets, covering a broad
spectrum of outpatient services funded by the public health system
(SUS). Each row in these datasets corresponds to a procedure performed
at an outpatient level, including clinical, administrative, and
financial details. The data is organized by type of service or procedure
group.

------------------------------------------------------------------------

The `load_outpacient_procedures` function offers the following
parameters:

1.  **dataset**: Specifies the SIASUS dataset to download: \* SIASUS
    Ambulatory Care Datasets:

    - `"siasus_pa"` – Consolidated Outpatient Procedures (Procedimentos
      Ambulatoriais). Contains records of approved outpatient procedures
      across all specialties. This is the most comprehensive SIASUS
      dataset and is often used for general outpatient service analysis.
    - `"siasus_ab"` – Bariatric Surgery (Cirurgia Bariátrica). Records
      related to bariatric surgery procedures performed in outpatient
      settings.
    - `"siasus_abo"` – Post-Bariatric Surgery Follow-Up (Acompanhamento
      Bariátrico). Includes follow-up care for patients who have
      undergone bariatric surgery, focusing on long-term monitoring and
      outcomes.
    - `"siasus_acf"` – Vascular Access for Dialysis (Fístula
      Arteriovenosa). Documents procedures involving the creation or
      maintenance of arteriovenous fistulas, essential for hemodialysis
      treatment.
    - `"siasus_ad"` – Miscellaneous Specialized Procedures (Laudos
      Diversos) Covers less frequent or highly specialized outpatient
      procedures not classified in other datasets.
    - `"siasus_am"` – High-Cost Medications (Medicamentos) Tracks the
      distribution and usage of outpatient medications that are
      high-cost and part of specific therapeutic programs.
    - `"siasus_an"` – Nephrology / Dialysis (Nefrologia) Contains
      outpatient nephrology procedures, particularly related to the care
      and monitoring of patients with chronic kidney disease.
    - `"siasus_atd"` – Dialysis Treatment (Tratamento Dialítico)
      Includes outpatient dialysis treatment sessions for patients with
      kidney failure.
    - `"siasus_ps"` – RAAS Psychosocial Care (RAAS Psicossocial) Part of
      the Specialized Outpatient Mental Health Services. Records care
      provided through Psychosocial Care Centers (CAPS), including
      treatments for severe mental disorders and substance use.
    - `"siasus_sad"` – RAAS Home Care (RAAS Atenção Domiciliar) Focuses
      on outpatient care provided at patients’ homes, often involving
      chronic condition management, palliative care, and
      multi-professional follow-ups.

2.  **raw_data**: there are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data. Only
      effective for SIM-DO and subsets, SIH, and CNES-LT.

3.  **time_period**: picks the years for which the data will be
    downloaded

4.  **states**: a vector of states by which to filter the data. Only
    works for datasets whose data is provided in separate files by
    state.

5.  **language**: you can choose between Portuguese `("pt")` and English
    `("eng")`

**Examples:**

``` r
library(datazoom.saude)

# Download processed data for Post-Bariatric Surgery Follow-Up (ABO) – State of Acre, 2012.
teste_abo <- load_outpatient_procedures(
  dataset = "siasus_abo",
  time_period = 2012,
  raw_data = FALSE,
  language = "eng",
  states = "AC"
)

# Download processed data for Consolidated Outpatient Procedures (PA) – State of Acre, 2022.
teste_pa <- load_outpatient_procedures(
  dataset = "siasus_pa",
  time_period = 2022,
  raw_data = FALSE,
  language = "eng",
  states = "AC"
)

# Download raw data for High-Cost Medications (AM) for the year 2021 in Pernambuco (PE).
teste_am_raw <- load_outpatient_procedures(
  dataset = "siasus_am",
  time_period = 2021,
  raw_data = TRUE,
  states = "PE"
)

# Download processed data for Psychosocial Care (PS) for the year 2022 in Acre (AC).
teste_ps <- load_outpatient_procedures(
  dataset = "siasus_ps",
  time_period = 2022,
  raw_data = FALSE,
  language = "eng",
  states = "AC"
)
```

# Oncology Cases

The `load_oncology_case` function downloads and organizes data from the
**Oncology Panel (Painel de Oncologia)**, part of DATASUS. This dataset
is widely used in public health and epidemiological analyses related to
cancer cases in Brazil.

------------------------------------------------------------------------

The `load_oncology_case` function offers the following parameters:

1.  **raw_data**: there are two options:

    - `TRUE`: If you want the original DATASUS files in their segmented
      form.
    - `FALSE`: if you want the treated version of the data.

2.  **time_period**: Specifies the years for which the data will be
    downloaded.

3.  **language**: You can choose between Portuguese `("pt")` and English
    `("eng")` for variable descriptions when `raw_data = FALSE`.

**Examples:**

``` r
library(datazoom.saude)

# Download processed oncology data for the year 2023.
# This will return data from the Oncology Panel for all Brazilian states.
oncology_data_2023 <- load_oncology_case(
  time_period = 2023,
  raw_data = FALSE,
  language = "eng"
)

# Download raw oncology data for the years 2020 to 2022 with labels in portuguese.
oncology_data_raw <- load_oncology_case(
  time_period = 2020:2022,
  raw_data = TRUE,
  language = "pt"
)
```

<br>

## Contributing

Thank you for your interest in contributing! If you have found a bug or
have a suggestion for improvement, please open a [GitHub
issue](https://github.com/datazoompuc/datazoom.saude/issues).

## Credits

DataZoom is developed by a team at the Pontifícia Universidade Católica
do Rio de Janeiro (PUC-Rio), Department of Economics. Our official
website is: <https://www.econ.puc-rio.br/datazoom/>.

To cite the `datazoom.saude` package in publications, use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata. <https://www.econ.puc-rio.br/datazoom/english/index.html>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2023,
      author = {Data Zoom},
      title = {Data Zoom: Simplifying Access To Brazilian Microdata},
      url = {[https://www.econ.puc-rio.br/datazoom/](https://www.econ.puc-rio.br/datazoom/)},
      year = {2023},
    }
