load_pni <- function(year, state, strategy, product, dose, data) {

  # Check if the mandatory data file path is provided
  if (is.null(data)) {

    # Notify the user that the scraping process is beginning
    message("Please wait while web scraping is being performed.")

    # Increase timeout to handle slow government server responses
    options(timeout = 2000)

    # Initialize a new Chrome session via Chromote
    b <- ChromoteSession$new()
    # Ensure the session closes automatically when the function exits (even on error)
    on.exit(try(b$close(), silent = TRUE), add = TRUE)

    # Remove the '#' from the line below if you need to debug visually (opens browser window)
    #b$view()

    # Create a clean, exclusive temporary folder for downloads
    download_dir <- file.path(tempdir(), "pni_downloads")

    # Delete the directory if it already exists and recreate it
    unlink(download_dir, recursive = TRUE, force = TRUE)
    dir.create(download_dir, recursive = TRUE)

    # Configure the browser to allow downloads in the specified directory
    try(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = download_dir), silent = TRUE)

    # Target URL for the SI-PNI consolidated monthly applied doses report
    target_url <- "https://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf"

    # Navigate to the site and wait for initial scripts to load
    b$Page$navigate(target_url)
    Sys.sleep(5)

    ## State (UF) Selection
    # Find and click the list item matching the specified State label
    b$Runtime$evaluate(sprintf("
    (function(){
      var el=[...document.querySelectorAll('li[data-label]')].find(x=>x.dataset.label==='%s');
      if(el) el.click();
    })();
  ", state))
    Sys.sleep(1)

    ## Aggregate by Municipality (Totalizar por município)
    # Check the IBGE aggregation checkbox if it's not already active
    b$Runtime$evaluate("
    (function(){
      var box = document.querySelector(\"[id='dosesAplicadasMensalForm:chkTotalizarIBGE'] .ui-chkbox-box\");
      if (box && !box.classList.contains('ui-state-active')) box.click();
    })();
  ")
    Sys.sleep(15)

    ## Strategy Selection
    # Select the vaccination strategy from the dynamic panel
    b$Runtime$evaluate(sprintf("
    (function(){
      var panel = document.querySelector(\"[id='dosesAplicadasMensalForm:estrategiaPesquisa_panel']\");
      if(!panel) return;
      var opt = [...panel.querySelectorAll('li[data-label]')].find(el => el.dataset.label === '%s');
      if (opt) opt.click();
    })();
  ", strategy))
    Sys.sleep(15)

    ## Product Selection
    # Select the vaccine product from the dynamic panel
    b$Runtime$evaluate(sprintf("
    (function(){
      var panel = document.querySelector(\"[id='dosesAplicadasMensalForm:produtoPesquisa_panel']\");
      if(!panel) return;
      var opt = [...panel.querySelectorAll('li[data-label]')].find(x => (x.dataset.label||x.textContent.trim()) === '%s');
      if (opt) opt.click();
    })();
  ", product))
    Sys.sleep(15)

    ## Dose Options Extraction
    # Capture the HTML of the dose selection container
    html_doses <- b$Runtime$evaluate("
  (function(){
    const el = document.querySelector('#dosesAplicadasMensalForm\\\\:dosePesquisa');
    return el ? el.outerHTML : null;
  })();
")$result$value

    if (is.null(html_doses) || html_doses == "null") {
      # Custom error message for missing dose container
      message("⚠️ Dose selection failed, please try again. (If the error persists, take a screenshot and send it to the Data Zoom team on GitHub)")
      return(invisible(NULL))
    }

    # Extract unique dose labels using regex, filtering out "REF" (Reference)
    dose_labels <- stringr::str_match_all(html_doses, "<label[^>]*>(.*?)</label>")[[1]][,2]
    dose_labels <- stringr::str_trim(dose_labels)
    dose_labels <- unique(dose_labels[dose_labels != "REF"])

    # Store dose labels as an attribute of the session object
    attr(b, "dose_labels") <- dose_labels
    dose_labels <- sort(dose_labels)

    ## Manage Doses (Selection Logic)
    # JavaScript to select all relevant doses while unchecking 'REF/REFORÇO' (booster) values
    res_doses <- b$Runtime$evaluate("
  (function(){
    // Normalizer: removes accents/diacritics, converts to uppercase, and trims whitespace
    const norm = s => (s||'')
      .normalize('NFD')
      .replace(/\\p{Diacritic}/gu,'')
      .toUpperCase()
      .trim();

    const root = document.querySelector(\"[id='dosesAplicadasMensalForm:dosePesquisa']\");
    if (!root) return JSON.stringify({ok:false, reason:'dosePesquisa_not_found'});

    const rows = Array.from(root.querySelectorAll('tr'));
    const actions = [];
    let marked = 0, unmarked = 0, skipped = 0, total = 0;

    // Iterate through table rows to find labels and corresponding checkboxes
    rows.forEach(tr => {
      const lb = tr.querySelector('label[for]');
      if (!lb) return;
      total++;

      const labelText = (lb.textContent || '').trim();
      const textN = norm(labelText);
      const forId = lb.getAttribute('for');

      const input = root.querySelector(\"input[id='\" + forId + \"']\");
      const box = input && input.closest('.ui-chkbox') ? input.closest('.ui-chkbox').querySelector('.ui-chkbox-box') : null;
      if (!box) {
        actions.push({label: labelText, forId, action: 'no_box'});
        return;
      }

      // Robust identification of REF/REFORÇO:
      // Checks for exact matches or strings starting with/containing 'REF'
      const isRef = textN === 'REF' || textN.startsWith('REFORCO') || /\\bREF\\b/.test(textN);
      const wasActive = box.classList.contains('ui-state-active');

      if (isRef) {
        // Ensure UNCHECKED for reference/booster doses
        if (wasActive) {
          box.click();
          unmarked++;
          actions.push({label: labelText, forId, action:'uncheck'});
        } else {
          skipped++;
          actions.push({label: labelText, forId, action:'keep_unchecked'});
        }
      } else {
        // Ensure CHECKED for standard doses (D1, D2, etc.)
        if (!wasActive) {
          box.click();
          marked++;
          actions.push({label: labelText, forId, action:'check'});
        } else {
          skipped++;
          actions.push({label: labelText, forId, action:'keep_checked'});
        }
      }
    });

    return JSON.stringify({ ok:true, total, marked, unmarked, skipped, actions });
  })();
")$result$value

    if (!is.null(res_doses) && res_doses != "null") {
      dd <- try(jsonlite::fromJSON(res_doses), silent = TRUE)
      if (!inherits(dd, "try-error") && isTRUE(dd$ok)) {
        # Success logic (no action required here)
      } else {
        # Error feedback if the dose selection logic fails
        message("⚠️ Dose selection failed, please try again. (If the error persists, take a screenshot and send it to the Data Zoom team on GitHub)")
        return(invisible(NULL))
      }
    }
    Sys.sleep(2)

    ## Year Selection
    # Set the year input field value directly via JavaScript
    b$Runtime$evaluate(sprintf("
    (function(){
      function setAno(){
        let el = document.querySelector(\"[id='dosesAplicadasMensalForm:ano']\");
        if (el){ el.focus(); el.value = '%s'; el.blur(); }
      }
      setAno(); setTimeout(setAno, 2000);
    })();
  ", year))
    Sys.sleep(2)

    ## Search Execution
    # Click the submit button to generate the report
    b$Runtime$evaluate("
    (function(){
      var btn = document.querySelector('input[type=submit][value=\"Pesquisar\"]');
      if (btn) btn.click();
    })();
  ")
    Sys.sleep(20)

    t0 <- Sys.time()
    data_status <- "loading"

    # Polling loop to check if the result table has loaded or if no records were found
    repeat {
      status_js <- "
      (function(){
        var tbody = document.querySelector(\"[id='dosesAplicadasMensalForm:listaDoseAplicadasTable_data']\");
        var txt = tbody ? (tbody.innerText || '') : '';
        if (/\\b[0-9]{6}\\b/.test(txt)) return 'success';

        var msgs = document.querySelector(\"div[id$=':messages'] .ui-messages-summary\");
        var msgGlobal = msgs ? msgs.innerText : '';
        if (/Nenhum Registro Encontrado!/i.test(msgGlobal) || /Nenhum Registro Encontrado!/i.test(txt)) {
           return 'empty';
        }
        return 'loading';
      })();
    "
      data_status <- try(b$Runtime$evaluate(status_js)$result$value, silent = TRUE)
      if (inherits(data_status, "try-error")) data_status <- "loading"

      if (data_status == "success") {
        break
      }

      if (data_status == "empty") {
        Sys.sleep(3)
        if (try(b$Runtime$evaluate(status_js)$result$value, silent=TRUE) == "empty") {
          return(data.frame())
        }
      }

      if (difftime(Sys.time(), t0, units = 'secs') > 90) {
        message("⚠️ The table generation timed out. Please wait a few minutes for the website to stabilize and try again.")
        return(invisible(NULL))
      }

      Sys.sleep(2)
    }

    ## CSV Download
    # Trigger the CSV export by clicking the icon/link via JS
    b$Runtime$evaluate("
(function(){
  const anchors = document.querySelectorAll('a[onclick*=\"mojarra.jsfcljs\"]');
  for (const a of anchors) {
    if (a.outerHTML.includes('csv.png') || a.textContent.includes('CSV')) {
      a.click();
      return true;
    }
  }
  return false;
})();
")

    # Wait for the file to appear in the temporary download directory
    csv_file <- NULL
    t0 <- Sys.time()

    repeat {
      files <- list.files(download_dir, pattern = "\\.csv$", full.names = TRUE)
      if (length(files) > 0) {
        csv_file <- files[which.max(file.info(files)$mtime)]
        break
      }
      if (difftime(Sys.time(), t0, units = "secs") > 60) {
        message("⚠️ The CSV download timed out. Please wait a few minutes for the website to stabilize and try again.")
        return(invisible(NULL))
      }
    }

    # Load the downloaded CSV file using specific encoding for Brazilian characters
    dat <- suppressMessages(
      readr::read_csv(
        csv_file,
        show_col_types = FALSE,
        locale = readr::locale(encoding = "ISO-8859-1")
      )
    )


  } else {

    dat <- suppressMessages(
      readxl::read_excel(data)
      )

    # Extract dose labels directly from the spreadsheet structure
    dose_labels <- toupper(dose)
    dose_labels <- sort(dose_labels)

  }


  # Data Wrangling and Column Renaming
  df <- dat %>%
    dplyr::rename(municipio = 1) %>% # The first column contains municipality info
    dplyr::mutate(
      munic_code  = stringr::str_extract(municipio, "^[0-9]+"), # Extract IBGE code
      munic_name  = stringr::str_remove(municipio, "^[0-9]+ - "), # Extract city name
      .before = municipio
    ) %>%
    dplyr::select(-municipio)

  # Calculate column mappings based on months and extracted dose labels
  ndoses  <- length(dose_labels)
  nmeses  <- 12
  meses   <- rep(month.name[1:nmeses], each = ndoses)
  doses   <- rep(dose_labels, nmeses)

  # Dynamically assign meaningful names to data columns
  if ((ncol(df) - 2) == length(meses)) {
    names(df)[-(1:2)] <- sprintf("%s_%s", meses, doses)
  }

  # Final transformation: pivot to long format and clean up data types
  dat_final <- df %>%
    tidyr::pivot_longer(
      cols = -c(munic_code, munic_name),
      names_to = c("mes", "dose"),
      names_sep = "_",
      values_to = "quantity"
    ) %>%
    dplyr::mutate(
      munic_code = as.numeric(munic_code),
      munic_name = stringr::str_to_lower(munic_name),
      month      = stringr::str_to_lower(mes),
      quantity   = suppressWarnings(as.numeric(quantity)),
      state      = state,
      year       = as.integer(year),
      strategy   = strategy,
      product    = product
    ) %>%
    dplyr::select(state, year, munic_code, munic_name, strategy, product, month, dose, quantity)

  return(dat_final)

}
