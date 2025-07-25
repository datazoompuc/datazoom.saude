# Declare global variables to avoid check notes

utils::globalVariables(c(
  "file_name", "dataset", "link",
  "name_eng", "label_eng", "name_pt", "label_pt", "var_code",

  # load_oncology_case
  "dt_diag", "dt_trat", "dt_nasc", "mun_diag",

  # load_births
  "dtnascmae", "dtultmenst", "codmunnasc", "dtcadastro", "dtnasc", "dtrecebim",
  "dtdeclarac"

))
