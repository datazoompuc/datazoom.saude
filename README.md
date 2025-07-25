# datazoom.saude

**`datazoom.saude`** is an R package that provides simple, direct, and reliable functions to import, organize, and explore public health databases in Brazil.  
The package is part of the **`datazoom`** ecosystem, designed to simplify access to and analysis of national data.

## Installation
```r
# Install the 'devtools' package if you don't have it yet
install.packages("devtools")

# Install datazoom.saude
devtools::install_github("datazoompuc/datazoom.saude")
```
## Main features

- **Raw data import** with a single line of code.
- **Automatic variable name standardization.**
- Access to datasets organized by year, state (UF), and data source.
- Initial support for the SUS Oncology Panel (PO).

## Supported databases

- **Oncology Panel (PO):** Cancer cases registered in the SUS by diagnosis location, treatment location, and cancer type.
- **Births (SINASC):** Live birth records in Brazil.

Coming soon: SIM, SIH/SUS, and other Brazilian health datasets.

