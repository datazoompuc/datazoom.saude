# datazoom.saude: Download Brazilian Health Data from DATASUS

Functions to download, clean, and standardize Brazilian public health
microdata from DATASUS.

## Details

Downloaded `.dbc` files are cached on disk in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) for the duration of
the current R session to avoid redundant transfers on repeated queries.
To disable session caching, set `options(datazoom.saude.cache = FALSE)`.

The DATASUS FTP server (`ftp.datasus.gov.br`) intermittently drops
connections mid-transfer. All FTP loaders automatically retry each file
download up to 3 times before failing.

## See also

Useful links:

- <https://datazoom.com.br/en/dz_saude/>

- Report bugs at <https://github.com/datazoompuc/datazoom.saude/issues>

## Author

**Maintainer**: Laura Tavares Regadas <lauratregadas@gmail.com>

Authors:

- Laura Tavares Regadas <lauratregadas@gmail.com>

- Luana Laport Santos <luanalaportsantos@icloud.com>

- Vanndher Amós B. A. de Souza <vanndheramos@gmail.com>

Other contributors:

- João Paulo dos Santos P. Barbosa <joao.3158129@gmail.com>
  \[contributor\]

- DataZoom (PUC-Rio) <datazoom@econ.puc-rio.br> \[funder\]

- Daniela Petruzalek <daniela.petruzalek@gmail.com> \[contributor\]

- Mark Adler \[contributor\]

- Antonio Pedro Rodrigues Bergallo
  ([ORCID](https://orcid.org/0009-0004-6132-3070)) \[contributor\]
