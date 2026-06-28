
<a href="https://github.com/datazoompuc/datazoom.saude"><img src="https://raw.githubusercontent.com/datazoompuc/datazoom.saude/master/hex_dzsaude.png" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.saude

<!-- badges: start -->

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

## Overview

The `datazoom.saude` package provides simple, direct, and reliable
functions to import, organize, and explore public health databases in
Brazil. It is part of the **`datazoom`** ecosystem, designed to simplify
access to and analysis of national data.

## About DATASUS

**DATASUS** is the information technology department of **SUS — the
Brazilian Unified Health System**. It maintains a wide range of open
databases covering topics such as **health establishments, mortality,
access to healthcare services, hospital admissions, births, and
epidemiological indicators** across the country.

The `datazoom.saude` package streamlines access to these resources by:

- Downloading multiple raw DATASUS datasets automatically;
- Cleaning and standardizing selected datasets for easier analysis;
- Providing consistent structures across data sources for seamless
  integration;

Each supported dataset is detailed in the sections below.

------------------------------------------------------------------------

## Installation

You can install the released version of `datazoom.saude` from the
development version from GitHub.

``` r
# Install the 'devtools' package if you don't have it yet
install.packages("devtools")

# Install datazoom.saude directly from GitHub
devtools::install_github("datazoompuc/datazoom.saude")
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

**[7 - Vaccination (SI-PNI)](#vaccines)**

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
      - `"general"` – Main Declarations of Death. (National dataset
        available — `states = "all"`) Contains records of all non-fetal
        Death Certificates (DO) in Brazil, including socio-demographic
        data, location, and causes of death (ICD-10). It’s the base for
        general mortality analysis. (since 1979 to present)
      - `"fetal"` – Fetal mortality data. (National dataset not
        available) Contains records of fetal deaths, with information on
        the mother, pregnancy, and causes of fetal death. It’s essential
        for maternal and child health. (since 1979 to present)
      - `"external_causes"` – Mortality data from external causes.
        (National dataset not available) Contains a subset of
        `"general"` focusing on deaths due to accidents, violence, and
        other unnatural causes. Used for safety and prevention studies.
        (since 1979 to present)
      - `"infant"` – Infant mortality data (children). (National dataset
        not available) Contains a subset of `"general"` recording deaths
        of children under 1 year old, detailing causes and birth-related
        factors. Crucial for assessing child health. (since 1979 to
        present)
      - `"maternal"` – Maternal mortality data. (National dataset not
        available) Contains a subset of `"general"` for deaths of women
        during or shortly after pregnancy/childbirth, detailing
        obstetric causes. Important for women’s health. (since 1996 to
        present)

2.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.

3.  **states**: (valid only for the `general` dataset) — a string or a
    vector of strings indicating the Brazilian state(s) for which the
    data should be downloaded. The default is `"all"`, which downloads
    data for the entire country. For specific states, use the official
    abbreviations such as `"SP"` (São Paulo), `"RJ"` (Rio de Janeiro),
    or `c("SP", "RJ")`.

4.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

5.  **keep_all**: A boolean choosing whether to aggregate the data by
    municipality, losing individual-level variables (`FALSE`) or to keep
    all original variables (`TRUE`). Only applies when `raw_data` is
    `FALSE`.

6.  **language**: A string indicating the desired language of variable
    names and labels. Accepts `"eng"` (default) for English or `"pt"`
    for Portuguese (only when `raw_data = FALSE`).

**Examples:**

``` r
library(datazoom.saude)

# Download raw data for general mortality - State of Rio de Janeiro, 2022.
raw_data_general_rj <- load_mortality(
  dataset = "general",
  time_period = 2022,
  states = "RJ",
  raw_data = TRUE
)

# Download treated data for general mortality - States of Rio and São Paulo, 2022.
trated_data_general_rj <- load_mortality(
  dataset = "general",
  time_period = 2022,
  states = c("RJ", "SP"),
  raw_data = FALSE,
  keep_all = FALSE # Explicitly stating default behavior
)

# Download treated data for Maternal Deaths - Brazil, 2020 to 2022.
# Descriptions in Portuguese.
# Note: `maternal` does not provide separate files by state.
data_maternal_pt <- load_mortality(
  dataset = "maternal",
  time_period = 2020:2022,
  states = "all",
  raw_data = FALSE,
  language = "pt"
)

# Download treated data for Infant Deaths - Brazil, 2017.
# Keeping all individual variables (not aggregated).
data_infant_full <- load_mortality(
  dataset = "infant",
  time_period = 2017,
  states = "all",
  raw_data = FALSE,
  keep_all = TRUE,
  language = "eng"
)
  
# Download treated data for Fetal Deaths - State of Amazonas, 2000.
data_infant_full <- load_mortality(
  dataset = "fetal",
  time_period = 2000,
  states = "AM", 
  raw_data = FALSE,
  language = "eng"
)

# Download treated data for External Causes Deaths - State of Acre, 2022.
data_infant_full <- load_mortality(
  dataset = "fetal",
  time_period = 2022,
  states = "AC", 
  raw_data = FALSE,
  language = "eng"
)
```

# Live Births

The `load_births` function provides access to the **Live Birth
Information System (SINASC)** dataset, which collects and records
detailed information about births in Brazil. This data is extracted from
Live Birth Certificates (DNVs) and includes information about the
newborn, such as sex, weight, and gestational age, as well as data about
the mother, such as age, number of children and health conditions (since
1994 to present). SINASC is essential for monitoring maternal and child
health and generating relevant indicators for public health policy
formulation.

------------------------------------------------------------------------

The `load_births` function offers the following parameters:

1.  **time_period**: A numeric value or vector indicating the year(s) of
    the data to be downloaded. For  
    example, `2020` or `2015:2020`. (since 1994 to present)

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

    - SIH hospitalization data is split across four datasets (since
      Jan/2008 to present):
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

2.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.

3.  **states**: a string or vector of strings indicating the Brazilian
    state(s) for which the data should be downloaded. Use `"all"` to
    download data for the entire country. For specific states (valid
    only for the `general` dataset), use abbreviations like `"SP"` (São
    Paulo), `"RJ"` (Rio de Janeiro), or `c("SP", "RJ")`.

4.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

5.  **language**: A string indicating the desired language of variable
    names and labels. Accepts `"eng"` (default) for English or `"pt"`
    for Portuguese (only when `raw_data = FALSE`).

**Examples:**

``` r
library(datazoom.saude)

# Download raw data for Reduced AIHs (AIHs Reduzida) – All country, 2010.
data_rd_raw <- load_hospital_admissions(
  dataset = "reduced_aih",
  time_period = 2010,
  states = "all",
  raw_data = TRUE,
  language = "eng"
)

# Download processed data for Rejected AIHs with Error Codes – State of Amazonas, 2010 to 2020.
# Descriptions in Portuguese.
data_er_processed <- load_hospital_admissions(
  dataset = "rejected_aih_error",
  time_period = 2010:2020,
  states = "AM",
  raw_data = FALSE,
  language = "pt"
)

# Download raw data for Professional Services – States of Rio and São Paulo, 2022.
data_sp_raw <- load_hospital_admissions(
  dataset = "professional_services",
  time_period = 2022,
  states = C("RJ","SP"),
  raw_data = TRUE,
  language = "eng"
)

# Download processed data for Professional Services – Federal District, 2020 to 2022.
# Descriptions in Portuguese.
data_sp_processed <- load_hospital_admissions(
  dataset = "professional_services",
  time_period = 2020:2022,
  states = "DF",
  raw_data = FALSE,
  language = "pt"
)
```

# Hospital Beds

The `load_hospital_beds` function specifically focuses on the **CNES -
LT (Beds)** dataset, part of the National Register of Health
Establishments (CNES). This dataset provides information on the number
of available hospital beds in health establishments across Brazil (since
Out/2005 to present).

------------------------------------------------------------------------

The `load_hospital_beds` function offers the following parameters:

1.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.
    (since Out/2005 to present)

2.  **states**: a string or vector of strings indicating the Brazilian
    state(s) for which the data should be downloaded. Use `"all"` to
    download data for the entire country. For specific states (valid
    only for the `general` dataset), use abbreviations like `"SP"` (São
    Paulo), `"RJ"` (Rio de Janeiro), or `c("SP", "RJ")`.

3.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

4.  **keep_all**: A boolean choosing whether to aggregate the data by
    municipality, losing individual-level variables (`FALSE`) or to keep
    all original variables (`TRUE`). Only applies when `raw_data` is
    `FALSE`.

5.  **language**: A string indicating the desired language of variable
    names and labels. Accepts `"eng"` (default) for English or `"pt"`
    for Portuguese (only when `raw_data = FALSE`).

**Examples:**

``` r
library(datazoom.saude)

# Download treated data - States of Amazonas and Pará, 2010.
data_beds_full <- load_hospital_beds(
  time_period = 2010,
  states = c("AM", "PA"),
  raw_data = FALSE,
  language = "eng"
)

# Download treated data - Brrazil, 2010 to 2022.
# Descriptions in Portuguese.
data_beds_full <- load_hospital_beds(
  time_period = 2010:2022,
  states = "all",
  raw_data = FALSE,
  language = "pt"
)

# Download raw data - States of Rio de Janeiro, 2015.
data_beds_raw <- load_hospital_beds(
  time_period = 2015,
  states = "RJ",
  raw_data = TRUE,
  language = "eng"
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

> **Note:** In all SIASUS datasets, variables related to the *Cadastro
> Nacional de Saúde* (CNS – National Health Card number) are
> **encrypted** by DATASUS.  
> This ensures patient confidentiality and means that individual-level
> CNS identifiers cannot be directly used for linkage across datasets.
> Because of this, this variable is removed when `raw_data = FALSE`.

------------------------------------------------------------------------

The `load_outpacient_procedures` function offers the following
parameters:

1.  **dataset**: Specifies the SIASUS dataset to download:

    - `"ambulatory_production"` – Consolidated Outpatient Procedures
      (Procedimentos Ambulatoriais). Contains records of approved
      outpatient procedures across all specialties. This is the most
      comprehensive SIASUS dataset and is often used for general
      outpatient service analysis. (since Jul/1994 to present)
    - `"bariatric_surgery"` – Pre-Bariatric Surgery (Pré Cirurgia
      Bariátrica). Records related to bariatric surgery procedures
      performed in outpatient settings. (Jan/2008 to Mar/2013)
    - `"bariatric_surgery_follow_up"` – Bariatric Surgery Follow-Up
      (Acompanhamento Bariátrico). Includes follow-up care for patients
      who have undergone bariatric surgery, focusing on long-term
      monitoring and outcomes. (since Apr/2013 to present)
    - `"fistula_confection"` – Vascular Access for Dialysis (Fístula
      Arteriovenosa). Documents procedures involving the creation or
      maintenance of arteriovenous fistulas, essential for hemodialysis
      treatment. (since Jun/2014 to present)
    - `"diverse_reports"` – Miscellaneous Specialized Procedures (Laudos
      Diversos) Covers less frequent or highly specialized outpatient
      procedures not classified in other datasets. (since Jan/2008 to
      present)
    - `"medicines"` – High-Cost Medications (Medicamentos) Tracks the
      distribution and usage of outpatient medications that are
      high-cost and part of specific therapeutic programs. (since
      Jan/2008 to present)
    - `"nephrology"` – Nephrology / Dialysis (Nefrologia) Contains
      outpatient nephrology procedures, particularly related to the care
      and monitoring of patients with chronic kidney disease. (Jan/2008
      to Out/2024)
    - `"dialytic_treatment"` – Dialysis Treatment (Tratamento Dialítico)
      Includes outpatient dialysis treatment sessions for patients with
      kidney failure. (since Jun/2014 to present)
    - `"psychosocial"` – RAAS Psychosocial Care (RAAS Psicossocial) Part
      of the Specialized Outpatient Mental Health Services. Records care
      provided through Psychosocial Care Centers (CAPS), including
      treatments for severe mental disorders and substance use. (since
      Jan/2013 to present)
    - `"home_care"` – RAAS Home Care (RAAS Atenção Domiciliar) Focuses
      on outpatient care provided at patients’ homes, often involving
      chronic condition management, palliative care, and
      multi-professional follow-ups. (since Nov/2012 to present)

2.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.

3.  **states**: a string or vector of strings indicating the Brazilian
    state(s) for which the data should be downloaded. Use `"all"` to
    download data for the entire country. For specific states (valid
    only for the `general` dataset), use abbreviations like `"SP"` (São
    Paulo), `"RJ"` (Rio de Janeiro), or `c("SP", "RJ")`.

4.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

5.  **language**: A string indicating the desired language of variable
    names and labels. Accepts `"eng"` (default) for English or `"pt"`
    for Portuguese (only when `raw_data = FALSE`).

**Examples:**

``` r
library(datazoom.saude)

# Download processed data for Post-Bariatric Surgery Follow-Up (ABO) – State of Acre, 2012.
bariatric_surgery_follow_up <- load_outpatient_procedures(
  dataset = "bariatric_surgery_follow_up",
  time_period = 2012,
  states = "AC",
  raw_data = FALSE,
  language = "eng"
)

# Download processed data for Consolidated Outpatient Procedures (PA) – State of Acre, 2022.
# Descriptions in Portuguese.
ambulatory_production <- load_outpatient_procedures(
  dataset = "ambulatory_production",
  time_period = 2022,
  states = "AC",
  raw_data = FALSE,
  language = "pt"
)

# Download raw data for High-Cost Medications (AM) - State of Pernambuco, 2021.
medicines_raw <- load_outpatient_procedures(
  dataset = "medicines",
  time_period = 2021,
  states = "PE",
  raw_data = TRUE,
  language = "eng"
)

# Download processed data for Psychosocial Care (PS) - State of Acre, 2022 to 2023.
psychosocial <- load_outpatient_procedures(
  dataset = "psychosocial",
  time_period = 2022:2023,
  states = "AC",
  raw_data = FALSE,
  language = "eng"
)
```

# Oncology Cases

The `load_oncology_case` function downloads and organizes data from the
**Oncology Panel (Painel de Oncologia)**, part of DATASUS. This dataset
is widely used in public health and epidemiological analyses related to
cancer cases in Brazil (since 2013 to present).

------------------------------------------------------------------------

The `load_oncology_case` function offers the following parameters:

1.  **time_period**: a numeric value or vector indicating the year(s) of
    the data to be downloaded. For example, `2020` or `2015:2020`.
    (since 2013 to present)

2.  **raw_data**: Logical, default is `FALSE`.

    - `TRUE`: If TRUE, returns the raw data exactly as provided by
      DATASUS.
    - `FALSE`: If FALSE (default), returns a cleaned and standardized
      version of the dataset.

3.  **language**: A string indicating the desired language of variable
    names and labels. Accepts `"eng"` (default) for English or `"pt"`
    for Portuguese (only when `raw_data = FALSE`).

**Examples:**

``` r
library(datazoom.saude)

# Download processed oncology data for the year 2023.
# This will return data from the Oncology Panel for all Brazilian states.
oncology_cases_treated <- load_oncology_case(
  time_period = 2023,
  raw_data = FALSE,
  language = "eng"
)

# Download raw oncology data for the years 2021 to 2022 with labels in portuguese.
oncology_cases_raw <- load_oncology_case(
  time_period = 2021:2022,
  raw_data = TRUE,
  language = "pt"
)
```

# Vaccines

The `load_vaccines()` function provides access to the **National
Immunization Program Information System (SI-PNI)**. This dataset
contains records of vaccine doses applied across Brazil, allowing for
the analysis of immunization coverage and public health strategies.

------------------------------------------------------------------------

The `load_vaccines` function offers the following parameters:

1.  **year**: A numeric value indicating the year of the data to be
    downloaded.
    - Currently supported range: `1994` to present.
    - Note: You must input only one year at a time.
2.  **state**: A string indicating the Brazilian state abbreviation for
    which the data should be downloaded (e.g., `"SP"`, `"RJ"`, `"AC"`).
    - Note: You must input only one state at a time.
3.  **strategy**: Specifies the vaccination strategy.
    - Common strategies include:
      - `"Rotina"` – Routine vaccination schedule.
      - `"Especial"` – Special immunobiologicals.
      - `"Bloqueio"` – Blocking vaccination in outbreak areas.
      - `"Intensificação"` – Intensification campaigns.
      - `"Serviço Privado"` – Data from private clinics.
    - Note: If set to `NULL`, a selection menu will appear.
4.  **product**: Specifies the specific vaccine or immunobiological
    product (e.g., `"BCG - BCG"`, `"Febre amarela - FA"`,
    `"Hepatite B - HB"`).
    - The valid options for product depend strictly on the chosen
      **strategy**.
    - Note: If set to `NULL`, a selection menu will appear.
5.  **dose**: A character vector indicating the dose categories that
    were selected when downloading the data.
    - This argument is **only required when using manually downloaded
      files** up to 2022.
      - The values must correspond to the **“Dose”** filter used (e.g.,
        `"D1"`, `c("D1", "2")`, `"Única"`, etc.).
6.  **data**: Optional path to a local `.xls` or `.xlsx` file downloaded
    manually.
    - If provided, `load_vaccines()` will **skip web scraping** and only
      perform data cleaning and harmonization.
    - This argument is optional for data up to 2022, but **required for
      data from 2023 onwards**.
7.  **language**: A string indicating the desired language of variable
    names and labels.
    - Accepts `"eng"` (default) for English or `"pt"` for Portuguese.

------------------------------------------------------------------------

## Data ingestion modes

The function supports **two distinct data ingestion modes**, depending
on the availability and stability of the official SI-PNI portals:

1.  **Automated web scraping** of the legacy SI-PNI Web system (for data
    up to 2022).
2.  **Manual data ingestion**, where the user downloads the data
    directly from the official DATASUS dashboard and the function
    performs only cleaning, harmonization, and standardization (optional
    for data up to 2022, but required for 2023 onwards).

Both modes produce a **fully harmonized output**, consistent with the
historical SI-PNI data structure.

### 1. Automated web scraping (1994 - 2022)

For historical data (`1994`–`2022`), `load_vaccines()` can automatically
retrieve consolidated vaccination data directly from the legacy SI-PNI
Web portal using web scraping techniques.

This mode:

1.  Requires a stable internet connection.
2.  Depends on the availability and responsiveness of the SI-PNI Web
    system.
3.  Uses a Chrome-based browser via the `chromote` package.

No manual intervention is required from the user.

### 2.1. Manual download (1994 - 2022)

From **1994 to 2022**, vaccination data are available through the
**legacy SI-PNI Web system**. Although `load_vaccines()` can retrieve
these data automatically via web scraping, users may also choose to
**manually download the data and provide the file to the function for
harmonization**.

In this case, `load_vaccines()` will perform only the cleaning,
harmonization, and standardization steps, ensuring that the resulting
dataset follows the same structure as the automatically collected data.

This approach can be useful when:

1.  The SI-PNI website is unstable or slow
2.  The user already possesses previously downloaded data
3.  The user prefers manual control over the data retrieval process

#### Step-by-step instructions for manual download

1.  Access the DATASUS vaccination dashboard:  
    *<https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf>*

2.  In the filter panel, **fill only the following fields**:

- **UF**
- **Ano**
- **Estratégia**
- **Produto**
- **Dose**

(Do not apply any additional filters)

3.  Select the option **“Totalizar por Município”**.

4.  Click **“Pesquisar”** and wait for the table to be generated.

5.  Below the table, locate the section **“Exportar Para o Formato”**
    and click on the first icon (`.xls`) to download the data.

6.  Provide the downloaded file to `load_vaccines()` using the `data`
    argument.

### 2.2. Manual download (2023 - present)

From **2023 onwards**, vaccination data are published exclusively
through the new DATASUS interactive dashboard. Due to technical and
legal constraints, automated scraping is not supported for this
platform.

In this case, `load_vaccines()` will perform only the cleaning,
harmonization, and standardization steps.

#### Step-by-step instructions for manual download

1.  Access the DATASUS vaccination dashboard:  
    *<https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA.html>*

2.  In the filter panel, **fill only the following fields**:

- **UF Ocorrência**
- **Ano Vacina**
- **Estratégia de Vacinação**
- **Imunobiológico**
- **Doses**

(Do not apply any additional filters)

3.  Switch to the **“Tabelas”** tab.

4.  In the table configuration:

- Add the variable **“Tipo Dose”** (at the left side) to the table.
- Expand all rows to display the full table.

5.  Click **“Baixar Dados”** and save the file in `.xlsx` format.

6.  Provide the downloaded file to `load_vaccines()` using the `data`
    argument.

------------------------------------------------------------------------

**Interactive Mode:**

If you are unsure of the exact strings for `strategy` or `product`, you
can run the function providing only the `year` and `state`. The function
will provide an interactive menu in the R console for you to choose from
valid combinations. (The interactive mode is only valid for data between
`1994` and `2022`)

------------------------------------------------------------------------

**Examples:**

``` r
library(datazoom.saude)

# Download data for Yellow Fever via web scraping (Routine strategy) - State of Acre, 2020
data_fa_acre <- load_vaccines(
  year = 2020,
  state = "AC",
  strategy = "Rotina",
  product = "Febre amarela - FA",
  language = "eng"
)

# Download data for BCG via web scraping (Private Service strategy) - State of São Paulo, 2018
data_bcg_sp <- load_vaccines(
  year = 2018,
  state = "SP",
  strategy = "Serviço Privado",
  product = "BCG - BCG",
  language = "pt"
)

# Download data for Trivalent Influenza using a manually downloaded file (Blockade strategy) - State of Acre, 2018
data_fa_acre <- load_vaccines(
  year = 2018,
  state = "AC",
  strategy = "Bloqueio",
  product = "Influenza Trivalente - FLU3V",
  doses = c("D1", "DU", "REV", "DI"), # required for data up to 2022
  data = "C:/path/to/downloaded_file.xls", #.xls
  language = "eng"
)

# Download data for Trivalent Influenza using a manually downloaded file (Blockade strategy) - State of Minas Gerais, 2024
data_fa_acre <- load_vaccines(
  year = 2024,
  state = "MG",
  strategy = "Bloqueio",
  product = "Influenza Trivalente - FLU3V",
  doses = NULL, # not required for data 2023 onwards
  data = "C:/path/to/downloaded_file.xls", #.xls
  language = "pt"
)

# Example of calling the function to trigger interactive selection - State of Minas Gerais, 2010
data_interactive <- load_vaccines(
  year = 2010,
  state = "MG",
  language = "pt")
```

------------------------------------------------------------------------

**Technical Note:**

1.  Automated web scraping relies on the legacy SI-PNI Web system and is
    subject to instability, timeouts, and unexpected failures.
2.  For recent years (2023 onwards), manual download via the DATASUS
    dashboard is the only supported data source.
3.  Regardless of the ingestion mode, `load_vaccines()` always returns a
    harmonized dataset with consistent variable names, dose categories,
    and structure.

**Important:** For the web scraping mode, please be aware that the
**SI-PNI** website
(*<https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf>*)
often experiences significant instability. This may result in connection
timeouts, slow response times, or unexpected errors during the scraping
process. If the function fails, it is recommended to wait a few minutes
and try again. If the error persists, please check the **SI-PNI** portal
status or report the issue on our GitHub repository.

<br>

## Contributing

Thank you for your interest in contributing! If you have found a bug or
have a suggestion for improvement, please open a [GitHub
issue](https://github.com/datazoompuc/datazoom.saude/issues).

## Credits

DataZoom is developed by a team at the Pontifícia Universidade Católica
do Rio de Janeiro (PUC-Rio), Department of Economics. Our official
website is: <https://datazoom.com.br/en/dz_saude>.

To cite the `datazoom.saude` package in publications, use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata. <https://datazoom.com.br/en/>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2023,
      author = {Data Zoom},
      title = {Data Zoom: Simplifying Access To Brazilian Microdata},
      url = {[https://datazoom.com.br/en/dz_saude](https://datazoom.com.br/en/dz_saude)},
      year = {2023},
    }
