# Suppress R CMD check notes for NSE column names and other global bindings
utils::globalVariables(c(
  "dose", "dose_raw", "mes", "month",
  "munic_code", "munic_name", "municipio", "quantity"
))
