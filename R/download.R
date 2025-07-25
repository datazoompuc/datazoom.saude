external_download <- function(dataset = NULL, source = NULL, year = NULL,
                              geo_level = NULL, coords = NULL, dataset_code = NULL,
                              sheet = NULL, skip_rows = NULL, file_name = NULL,
                              state = NULL) {
  ## Bind Global Variables

  link <- NULL

  ## Define Basic Parameters

  param <- list()
  param$dataset <- dataset
  param$source <- source

  # Optional parameters for functions that need them:

  param$year <- year # if download is perform separately by year
  param$geo_level <- geo_level # if some geo_levels have a different download link
  param$coords <- coords
  param$dataset_code <- dataset_code
  param$skip_rows <- skip_rows # number of rows to skip atop a spreadsheet
  param$file_name <- file_name
  param$sheet <- sheet # which sheet of a .xlsx to read
  param$state <- state

  if (is.null(param$skip_rows)) param$skip_rows <- 0 # makes it more error-proof

  #####################
  ## Construct Links ##
  #####################

  ## Pull URL from datasets_link

  param$url <- datasets_link(
    source = param$source,
    dataset = param$dataset,
    url = TRUE
  )

  # For most sources, the URL in datasets_link is already the URL needed for the download

  path <- param$url

  ## Filling in URLs

  # Some URLs are in the form www.data/$year$_$dataset$.csv, where expression
  # surrounded by $ are placeholders. The code below subs them in for actual parameters

  if (stringr::str_detect(path, "\\$year\\$|\\$state\\$|\\$file_name\\$")) {
    if (!is.null(param$year)) {
      path <- path %>%
        stringr::str_replace("\\$year\\$", as.character(param$year))
    }
    if (!is.null(param$state)) {
      path <- path %>%
        stringr::str_replace("\\$state\\$", param$state)
    }
    if (!is.null(param$file_name)) {
      path <- path %>%
        stringr::str_replace("\\$file_name\\$", param$file_name)
    }
  }

  # Below are the exceptions, for which manipulation is needed

  ##### Exceptions only #####

  # If the datasets_link URL is the download path you need,
  # do not change this section for a new function

  ## MapBiomas

  # Download path depends on dataset and geo_level

  if (source == "mapbiomas") {
    if (dataset == "mapbiomas_cover") {
      if(param$geo_level == "indigenous_land") {
        path <- "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-INDIGENOUS_LANDS-1.xlsx"
      }
    }
    if (dataset == "mapbiomas_transition") {
      if (param$geo_level == "biome") {
        path <- "https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2024/08/MAPBIOMAS_BRAZIL-COL.9-BIOMES.xlsx"
      }
      if (param$geo_level == "municipality") {
        path <- "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/downloads/mapbiomas_brasil_col9_state_municipality.xlsx"
      }
    }
  }

  ## SEEG

  # Download path depends on geo_level

  if (source == "seeg") {
    if (geo_level == "municipality") {
      path <- "https://drive.google.com/u/0/uc?confirm=bhfS&id=1rUc6H8BVKT9TH-ri6obzHVt7WI1eGUzd"
    }
  }

  ## TerraClimate

  # Download path depends on geographical parameters

  if (source == "terraclimate") {
    filename <- paste0(
      "agg_terraclimate_",
      param$dataset_code,
      "_1958_CurrentYear_GLOBE.nc"
    )

    path <- paste0(
      param$url,
      "/",
      filename,
      "?",
      "&var=",
      param$dataset_code,
      "&south=",
      param$coords$lat_min,
      "&north=",
      param$coords$lat_max,
      "&west=",
      param$coords$lon_min,
      "&east=",
      param$coords$lon_max,
      "&horizStride=1",
      "&time_start=",
      param$year$initial_time,
      "&time_end=",
      param$year$final_time,
      "&timeStride=1",
      "&disableProjSubset=on&addLatLon=true&accept=netcdf"
    )
  }

  #######################
  ## Initiate Download ##
  #######################

  ## Specify file extension to be passed to tempfile()

  # For most functions, the file extension is automatically detected

  file_extension <- sub(".*\\.", ".", path) %>%
    tolower()

  ##### Exceptions only #####

  # Only manually input the file_extension if the download_path does
  # not end in ".ext", where .ext is any file extension

  # googledrive links do not contain the file extension, for example

  if (source %in% c("seeg", "iema", "ips")) {
    file_extension <- ".xlsx"
  }
  if (source == "prodes") {
    file_extension <- ".txt"
  }
  if (source == "terraclimate") {
    file_extension <- ".nc"
  }
  if (source == "deter") {
    file_extension <- ".zip"
  }
  if (source == "ibama") {
    if (dataset == "embargoed_areas") {
      file_extension <- ".zip"
    } else {
      file_extension <- ".csv"
    }
  }
  if (source == "imazon") {
    file_extension <- ".rds"
  }
  if (source == "epe") {
    if (param$dataset == "national_energy_balance") {
      file_extension <- ".csv"
    }
  }
  if (source == "aneel") {
    if (dataset == "energy_development_budget") {
      file_extension <- ".rds"
    }
    if (dataset == "energy_generation") {
      file_extension <- ".xlsx"
    }
    if (dataset == "energy_enterprises_distributed") {
      file_extension <- ".csv"
    }
  }

  ## Define Empty Directory and Files For Download

  dir <- tempdir()
  temp <- tempfile(fileext = file_extension, tmpdir = dir)

  ## Picking the way to download the file

  download_method <- "standard" # works for most functions

  if (source %in% c("iema", "imazon")) {
    download_method <- "googledrive"
  }
  if (source == "aneel") {
    if (dataset == "energy_development_budget") {
      download_method <- "googledrive"
    }
    if (dataset == "energy_enterprises_distributed") {
      message("This may take a while.\n")
      options(timeout = 1000) # increase timeout limit
    }
  }
  if (source == "epe") {
    if (dataset == "national_energy_balance") {
      download_method <- "googledrive"
    }
  }
  if (source %in% c("deter", "terraclimate", "baci", "sigmine", "mapbiomas")) {
    download_method <- "curl"
    quiet <- FALSE
  }
  if (source == "datasus") {
    download_method <- "curl"
    quiet <- TRUE
  }
  if (source == "ibama") {
    download_method <- "curl"
    options(download.file.method = "curl", download.file.extra = "-k -L") # https://stackoverflow.com/questions/69716835/turning-ssl-verification-off-inside-download-file
    quiet <- TRUE
  }
  if (source == "seeg") {
    if (geo_level == "municipality") {
      download_method <- "googledrive"
    }
  }

  ## Downloading file by the selected method

  if (download_method == "standard") {
    utils::download.file(url = path, destfile = temp, mode = "wb")
  }
  if (download_method == "curl") {
    utils::download.file(url = path, destfile = temp, method = "curl", quiet = quiet)
  }
  if (download_method == "googledrive") {
    message("Please follow the steps from `googledrive` package to download the data. This may take a while.\nIn case of authentication errors, run vignette(\"GOOGLEDRIVE\").")

    googledrive::drive_download(path, path = temp, overwrite = TRUE)
  }

  ## Unzipping if the file is zipped

  if (file_extension == ".zip") {
    utils::unzip(temp, exdir = dir)
  }

  ###############
  ## Load Data ##
  ###############


  ##### Exceptions only #####

  if (file_extension == ".zip") {
    if (param$dataset == "degrad") {
      dat <- sf::read_sf(file.path(dir, param$file_name))
      dat$year <- param$year
    }
    if (param$source == "deter") {
      if (param$dataset == "deter_amz") {
        dat <- sf::read_sf(file.path(dir, "deter-amz-deter-public.shp"))
      }
      if (param$dataset == "deter_cerrado") {
        dat <- sf::read_sf(file.path(dir, "deter_public.shp"))
      }
    }
    if (param$source == "sigmine") {
      dat <- sf::read_sf(file.path(dir, "BRASIL.shp"))
    }

    if (param$source == "ibama") {
      # get latest downloaded file (the name changes daily)
      file <- file.info(list.files(dir, pattern = "rel_areas_embargadas_.*.xls"))
      file <- file[with(file, order(as.POSIXct(mtime))), ]
      file <- rownames(file)

      doc <- XML::htmlParse(file.path(dir, file), encoding = "UTF-8")

      tableNode <- XML::getNodeSet(doc, "//table")

      dataset <- XML::readHTMLTable(tableNode[[1]])


      colnames(dataset) <- dataset[5, ]

      dat <- dataset[-c(1:5), ]
    }

    if (param$source == "baci") {
      # as year can be a vector, sets up expressions of the form "*YYYY_V202401b.csv" for each year to match file names
      file_expression <- paste0("*", param$year, "_V202401b.csv")

      # now turning into *XXXX_V202401b.csv|YYYY_V202401b.csv|ZZZZ_V202401b.csv" to match as regex
      file_expression <- paste0(file_expression, collapse = "|")

      file <- list.files(dir, pattern = file_expression, full.names = TRUE) %>%
        as.list()

      # now reads each file
      dat <- lapply(file, data.table::fread, header = TRUE, sep = ",")

      # each data frame in the list is named after the corresponding year
      names(dat) <- param$year
    }
  }

  if (param$source == "epe" & param$dataset == "energy_consumption_per_class") {
    # param$sheet contains the selected sheets

    # Making a list with all the sheets
    dat <- purrr::imap(
      param$sheet,
      function(sheets, number) {
        base::message(
          paste0("Reading sheet ", number, " out of ", length(param$sheet), " (", sheets, ")")
        )
        base::suppressMessages(
          readxl::read_xls(temp, sheet = sheets)
        )
      }
    )

    names(dat) <- param$sheet
  }
  if (param$source == "aneel") {
    if (param$dataset == "energy_enterprises_distributed") {
      dat <- data.table::fread(temp, encoding = "Latin-1")
    } else if (dataset == "energy_generation") {
      dat <- readxl::read_xlsx(temp, sheet = param$sheet, skip = param$skip_rows, na = c("-", ""))
    }
  }

  if (param$source == "ips") {
    dat <- param$sheet %>%
      purrr::map(
        ~ readxl::read_xlsx(temp, sheet = .)
      )
  }

  ## Now the rest of the functions

  # This Depends on Data Type (.csv, .shp, ...) and on the data source

  else {
    if (file_extension == ".csv") {
      dat <- data.table::fread(temp)
    }
    if (file_extension == ".txt") {
      dat <- readr::read_csv(temp)
    }
    if (file_extension == ".nc") {
      dat <- terra::rast(temp)
    }
    if (file_extension == ".rds") {
      dat <- readr::read_rds(temp)
    }
    if (file_extension == ".xlsx") {
      dat <- readxl::read_xlsx(temp, sheet = param$sheet, skip = param$skip_rows)
    }
    if (file_extension == ".dbc") {
      dat <- read.dbc(temp)
    }
  }

  ##############################
  ## Excluding Temporary File ##
  ##############################

  # Folder is kept

  if (file_extension != ".nc") {
    unlink(temp)
  }

  #################
  ## Return Data ##
  #################

  return(dat)
}

datasets_link <- function(source = NULL, dataset = NULL, url = FALSE) {
  survey <- NULL

  link <- tibble::tribble(
    ~survey, ~dataset, ~sidra_code, ~available_time, ~available_geo, ~link,

    ########################
    ## Environmental data ##
    ########################

    ## DATASUS

    "datasus", "datasus_sim_do", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/$file_name$",
    "datasus", "datasus_sim_dofet", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_doext", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_doinf", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_sim_domat", NA, "1996-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/$file_name$",
    "datasus", "datasus_cnes_lt", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/$file_name$",
    "datasus", "datasus_cnes_st", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/$file_name$",
    "datasus", "datasus_cnes_dc", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/$file_name$",
    "datasus", "datasus_cnes_eq", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/$file_name$",
    "datasus", "datasus_cnes_sr", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/$file_name$",
    "datasus", "datasus_cnes_hb", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/$file_name$",
    "datasus", "datasus_cnes_pf", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/$file_name$",
    "datasus", "datasus_cnes_ep", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/$file_name$",
    "datasus", "datasus_cnes_rc", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/$file_name$",
    "datasus", "datasus_cnes_in", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/$file_name$",
    "datasus", "datasus_cnes_ee", NA, "2007-2021", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/$file_name$",
    "datasus", "datasus_cnes_ef", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/$file_name$",
    "datasus", "datasus_cnes_gm", NA, "2005-2023", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/$file_name$",
    "datasus", "datasus_sinasc", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/$file_name$",
    "datasus", "datasus_po", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/$file_name$",
    "datasus", "datasus_sih_rd", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/$file_name$",
    "datasus", "datasus_sih_rj", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/$file_name$",
    "datasus", "datasus_sih_sp", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/$file_name$",
    "datasus", "datasus_sih_er", NA, "1979-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_ab", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_ad", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_am", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_an", NA, "2008-2014", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_aq", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_ar", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_pa", NA, "1994-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_ps", NA, "2013-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_abo", NA, "2008-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_acf", NA, "1994-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_atd", NA, "2014-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
    "datasus", "datasus_siasus_sad", NA, "2012-2025", "State", "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/$file_name$",
  )

  # returns only the desired rows

  if (!is.null(source)) {
    link <- link %>%
      dplyr::filter(survey == source)
  }

  if (!is.null(dataset)) {
    link <- link %>%
      dplyr::filter(dataset == !!dataset)
  }

  if (url) {
    link <- link %>%
      purrr::pluck("link")
  }

  return(link)
}
