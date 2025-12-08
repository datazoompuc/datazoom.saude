load_applied_vaccines <- function(uf,
                                  ano,
                                  estrategia = NULL,
                                  produto = NULL) {

  # --- Verificação e Carregamento de Pacotes ---
  pacotes_necessarios <- c("chromote", "httr", "readr", "dplyr", "tidyr", "stringr", "janitor")

  # Encontra pacotes que NÃO estão instalados
  pacotes_faltando <- pacotes_necessarios[!sapply(pacotes_necessarios, requireNamespace, quietly = TRUE)]

  # Se houver pacotes faltando, para a função e avisa o usuário
  if (length(pacotes_faltando) > 0) {
    msg_stop <- paste(
      "Erro: Pacote(s) necessário(s) não encontrado(s):",
      paste(shQuote(pacotes_faltando), collapse = ", "),
      "\n\nPor favor, instale-os antes de continuar:",
      sprintf("\ninstall.packages(c(%s))", paste(shQuote(pacotes_faltando), collapse = ", "))
    )
    stop(msg_stop, call. = FALSE)
  }

  # Se todos os pacotes existem, carrega-os
  lapply(pacotes_necessarios, library, character.only = TRUE)

  ## 1. DICIONÁRIO DE DADOS (Estratégia x Produto)
  pni_valid_combos <- list(
    "Bloqueio" = c(
      "DTP/Hib - Tetra", "Dupla adulto - dT", "Dupla viral - SR", "Febre amarela - FA",
      "Influenza Trivalente - FLU3V", "Meningocócica AC - Meningo AC",
      "Meningocócica conjugada C - Men Conj C", "Pneumocócica 10V - Pncc10V",
      "Poliomielite inativada - VIP", "Tetra Viral - Tetra Viral",
      "Tríplice bacteriana - DTP", "Tríplice viral - SCR", "Varicela(atenuada) - Varc"
    ),
    "Campanha indiscriminada" = c(
      "Influenza Trivalente - FLU3V", "Poliomielite oral (Bivalente) - VOP",
      "Tríplice viral - SCR"
    ),
    "Especial" = c(
      "Cólera oral - Cólera", "DTPa/Hib/Polio Inativa - PENTAinativada",
      "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla infantil - DT",
      "Febre tifóide (atenuada) - Fta", "Febre tifóide (polissacarídica) - FTp",
      "Haemophilus tipo b - Hib", "Hepatite A - HA", "Hepatite A Pediátrica - HAped",
      "Hepatite B - HB", "Hexavalente - HEXA", "HPV Quadrivalente - HPV Quadri",
      "Imunoglobulina anti hepatite B - IGHB", "Imunoglobulina anti rábica - IGRH",
      "Imunoglobulina anti tetânica - IGTH", "Imunoglobulina anti varicela zoster - IGVZ",
      "Influenza H1N1 - H1N1", "Influenza Trivalente - FLU3V",
      "Meningocócica A C W Y135 - Meningo ACWY135", "Meningocócica conjugada C - Men Conj C",
      "Pneumocócica 10V - Pncc10V", "Pneumocócica 13V - Pncc13V",
      "Pneumocócica 23V - Pncc23V", "Pneumocócica 7V - Pncc7V",
      "Poliomielite inativada - VIP", "Raiva em cultivo celular (Embrião) - Embrião",
      "Tríplice acelular infantil - DTPa", "Tríplice bacteriana acelular (adulto)- dTpa - dTpa adulto",
      "Vacina ads hepatite A (inativada, virossomal) - HAadulto", "Varicela(atenuada) - Varc"
    ),
    "Intensificação" = c(
      "BCG - BCG", "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla adulto - dT",
      "Dupla viral - SR", "Febre amarela - FA", "Febre Amarela-Dose fracionada (0,1 ml) - FA(0,1 ml)",
      "Hepatite A Pediátrica - HAped", "Hepatite B - HB", "HPV Quadrivalente - HPV Quadri",
      "Meningocócica conjugada C - Men Conj C", "Pneumocócica 10V - Pncc10V",
      "Poliomielite inativada - VIP", "Poliomielite oral (Bivalente) - VOP",
      "Tetra Viral - Tetra Viral", "Tríplice bacteriana - DTP",
      "Tríplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tríplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina rotavírus humano - VRH", "Varicela(atenuada) - Varc"
    ),
    "Monitoramento Rápido de Cobertura Vacinal" = c(
      "DTP/HB/Hib - Penta", "Febre amarela - FA", "Meningocócica conjugada C - Men Conj C",
      "Pneumocócica 10V - Pncc10V", "Poliomielite inativada - VIP",
      "Poliomielite oral (Bivalente) - VOP", "Tetra Viral - Tetra Viral",
      "Tríplice bacteriana - DTP", "Tríplice viral - SCR", "Vacina rotavírus humano - VRH"
    ),
    "Rotina" = c(
      "BCG - BCG", "DTP/HB/Hib - Penta", "DTP/Hib - Tetra", "Dupla adulto - dT",
      "Dupla viral - SR", "Febre amarela - FA", "Hepatite A Pediátrica - HAped",
      "Hepatite B - HB", "HPV Quadrivalente - HPV Quadri",
      "Meningocócica A C W Y135 - Meningo ACWY135", "Meningocócica conjugada C - Men Conj C",
      "Pneumocócica 10V - Pncc10V", "Pneumocócica 23V - Pncc23V",
      "Pneumocócica 7V - Pncc7V", "Poliomielite inativada - VIP",
      "Poliomielite oral (Bivalente) - VOP", "Raiva em cultivo celular Vero - Vero",
      "Rubéola - Rubéola", "Sarampo - Sarampo", "Tetra Viral - Tetra Viral",
      "Toxóide Tetânico - TT", "Tríplice bacteriana - DTP",
      "Tríplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tríplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina rotavírus humano - VRH", "Varicela(atenuada) - Varc"
    ),
    "Serviço Privado" = c(
      "BCG - BCG", "DTPa/Hib/Polio Inativa - PENTAinativada", "Dupla adulto - dT",
      "Febre amarela - FA", "Febre tifóide (atenuada) - Fta",
      "Febre tifóide (polissacarídica) - FTp", "Haemophilus tipo b - Hib",
      "Hepatite A - HA", "Hepatite A Pediátrica - HAped", "Hepatite AeB(pediátrica) - HAeHBped",
      "Hepatite AeB(uso adulto) - HAeHB", "Hepatite B - HB", "Herpez Zoster - VHZ",
      "Hexavalente - HEXA", "HPV Bivalente - HPV", "HPV Quadrivalente - HPV Quadri",
      "Influenza ID - FLU ID", "Influenza Tetravalente (Quadrivalente) - FLU4V",
      "Influenza Trivalente - FLU3V", "Meningocócica B - MEN B",
      "Meningocócica A C W Y135 - Meningo ACWY135", "Meningocócica B/C - MEN B/C",
      "Meningocócica conjugada C - Men Conj C", "Pneumocócica 13V - Pncc13V",
      "Pneumocócica 23V - Pncc23V", "Poliomielite inativada - VIP",
      "Rotavírus pentavalente - ROTA penta", "Tetra Viral - Tetra Viral",
      "Toxóide Tetânico - TT", "Tríplice acelular infantil - DTPa",
      "Tríplice acelular/poliomelite inativada - DTPaVIP", "Tríplice bacteriana - DTP",
      "Tríplice bacteriana acelular (adulto)- dTpa - dTpa adulto", "Tríplice viral - SCR",
      "Vacina Dengue 1, 2, 3 e 4 (recomb e atenuada) - Dengue",
      "Vacina Herpes-Zoster (recombinante) - VZR", "Varicela(atenuada) - Varc"
    ),
    "Soroterapia" = c(
      "soro antiaracnídico - SARC", "Soro botrópico - SBOTR",
      "Soro botrópico/crotálico - SBOCR", "Soro botrópico/laquético - SBOLAQ",
      "Soro botulínico bivalente - SBOTULBI", "Soro botulínico trivalente - SBOTULTRI",
      "Soro crotálico - SCROT", "Soro diftérico - SAD", "Soro elapídico - SELAP",
      "Soro escorpiônico - SESCOR", "Soro lonômico - SLONO", "Soro loxoscélico - SLOXO",
      "Soro rábico humano - SARH", "Soro tetânico - SAT"
    )
  )

  ## 2. SELEÇÃO INTERATIVA (se os argumentos não forem fornecidos)
  if (is.null(estrategia) && interactive()) {
    estrategias_disponiveis <- names(pni_valid_combos)
    cat("Carregando dados de validação...\n")
    choice_index <- utils::menu(estrategias_disponiveis, title = "Selecione a Estratégia:")

    if (choice_index == 0) stop("Seleção cancelada pelo usuário.", call. = FALSE)
    estrategia <- estrategias_disponiveis[choice_index]
  }

  if (is.null(produto) && interactive()) {
    # Garante que a estratégia (mesmo que fornecida via argumento) é válida antes de listar produtos
    if (!estrategia %in% names(pni_valid_combos)) {
      stop(paste("Estratégia fornecida ('", estrategia, "') não é válida.", sep=""), call. = FALSE)
    }

    produtos_disponiveis <- pni_valid_combos[[estrategia]]
    choice_index <- utils::menu(produtos_disponiveis, title = paste("Selecione o Produto para '", estrategia, "':", sep=""))

    if (choice_index == 0) stop("Seleção cancelada pelo usuário.", call. = FALSE)
    produto <- produtos_disponiveis[choice_index]
  }

  ## 3. VALIDAÇÃO (garante que a combinação é válida)
  if (is.null(estrategia) || is.null(produto)) {
    stop("Estratégia e Produto devem ser fornecidos (ou selecionados interativamente).", call. = FALSE)
  }

  valid_strategies <- names(pni_valid_combos)
  if (!(estrategia %in% valid_strategies)) {
    msg <- paste("Estratégia inválida: '", estrategia, "'.\n",
                 "Opções válidas são: ", paste(shQuote(valid_strategies), collapse = ", "), sep="")
    stop(msg, call. = FALSE)
  }

  valid_products_for_strategy <- pni_valid_combos[[estrategia]]
  if (!(produto %in% valid_products_for_strategy)) {
    msg <- paste("Produto inválido: '", produto, "' para a estratégia '", estrategia, "'.\n\n",
                 "Opções válidas para '", estrategia, "' são: \n",
                 paste(shQuote(valid_products_for_strategy), collapse = "\n"), sep="")
    stop(msg, call. = FALSE)
  }

  # --- Início do código original da função ---
  if (ano < 1994) {

    message("Por favor, selecione um ano a partir de 1994.")

  } else if (ano >= 1994 & ano < 2023) {

    message("Aguarde enquanto os dados são baixados.")

    b <- ChromoteSession$new()
    on.exit(try(b$close(), silent = TRUE), add = TRUE)

    # b$view() # remova o '#' da frente se precisar depurar visualmente
    try(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = tempdir()), silent = TRUE)

    b$Page$navigate("http://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf")
    Sys.sleep(5)

    ## UF
    b$Runtime$evaluate("document.querySelector(\"[id='dosesAplicadasMensalForm:uf_label']\")?.click();")
    Sys.sleep(1)
    b$Runtime$evaluate(sprintf("
    (function(){
      var el=[...document.querySelectorAll('li[data-label]')].find(x=>x.dataset.label==='%s');
      if(el) el.click();
    })();
  ", uf))
    Sys.sleep(5)

    ## Totalizar por município
    b$Runtime$evaluate("
    (function(){
      var box = document.querySelector(\"[id='dosesAplicadasMensalForm:chkTotalizarIBGE'] .ui-chkbox-box\");
      if (box && !box.classList.contains('ui-state-active')) box.click();
    })();
  ")
    Sys.sleep(5)

    ## Ano
    b$Runtime$evaluate(sprintf("
    (function(){
      function setAno(){
        let el = document.querySelector(\"[id='dosesAplicadasMensalForm:ano']\");
        if (el){ el.focus(); el.value = '%s'; el.blur(); }
      }
      setAno(); setTimeout(setAno, 2000);
    })();
  ", ano))
    Sys.sleep(5)

    ## Estratégia
    b$Runtime$evaluate("document.querySelector(\"[id='dosesAplicadasMensalForm:estrategiaPesquisa_label']\")?.click();")
    Sys.sleep(1.2)
    b$Runtime$evaluate(sprintf("
    (function(){
      var panel = document.querySelector(\"[id='dosesAplicadasMensalForm:estrategiaPesquisa_panel']\");
      if(!panel) return;
      var opt = [...panel.querySelectorAll('li[data-label]')].find(el => el.dataset.label === '%s');
      if (opt) opt.click();
    })();
  ", estrategia)) # Agora usa a variável validada
    Sys.sleep(5)

    ## Produto
    b$Runtime$evaluate("document.querySelector(\"[id='dosesAplicadasMensalForm:produtoPesquisa_label']\")?.click();")
    Sys.sleep(1)
    b$Runtime$evaluate(sprintf("
    (function(){
      var panel = document.querySelector(\"[id='dosesAplicadasMensalForm:produtoPesquisa_panel']\");
      if(!panel) return;
      var opt = [...panel.querySelectorAll('li[data-label]')].find(x => (x.dataset.label||x.textContent.trim()) === '%s');
      if (opt) opt.click();
    })();
  ", produto)) # Agora usa a variável validada
    Sys.sleep(5)

    ## Doses
    html_doses <- b$Runtime$evaluate("
  (function(){
    const el = document.querySelector('#dosesAplicadasMensalForm\\\\:dosePesquisa');
    return el ? el.outerHTML : null;
  })();
")$result$value

    if (is.null(html_doses) || html_doses == "null") {
      # Mensagem de erro personalizada
      stop("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
    }

    # Extrai os labels das doses
    dose_labels <- stringr::str_match_all(html_doses, "<label[^>]*>(.*?)</label>")[[1]][,2]
    dose_labels <- stringr::str_trim(dose_labels)
    dose_labels <- unique(dose_labels[dose_labels != "REF"])
    # Mensagem removida

    # Armazena dentro do objeto retornado
    attr(b, "dose_labels") <- dose_labels
    dose_labels <- sort(dose_labels)

    ## Doses (todas)
    # Mensagem removida
    res_doses <- b$Runtime$evaluate("
  (function(){
    // Normalizador: remove acentos, upper, trim
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

    // Para cada linha com label[for], decida o estado explicitamente
    rows.forEach(tr => {
      const lb = tr.querySelector('label[for]');
      if (!lb) return;
      total++;

      const labelText = (lb.textContent || '').trim();
      const textN = norm(labelText); // ex.: D1, D2, R1, REF, REFORCO...
      const forId = lb.getAttribute('for'); // acha o 'box' correspondente

      // (sem CSS.escape por compatibilidade)
      const input = root.querySelector(\"input[id='\" + forId + \"']\");
      const box = input && input.closest('.ui-chkbox') ? input.closest('.ui-chkbox').querySelector('.ui-chkbox-box') : null;
      if (!box) {
        actions.push({label: labelText, forId, action: 'no_box'});
        return;
      }

      // Identificação robusta de REF/REFORÇO:
      // - texto 'REF' exato
      // - 'REFORCO' (com/sem acento, eventualmente com sufixos)
      // - contém palavra REF separada
      const isRef = textN === 'REF' || textN.startsWith('REFORCO') || /\\bREF\\b/.test(textN);
      const wasActive = box.classList.contains('ui-state-active');

      if (isRef) {
        // Garante DESMARCADO
        if (wasActive) {
          box.click();
          unmarked++;
          actions.push({label: labelText, forId, action:'uncheck'});
        } else {
          skipped++;
          actions.push({label: labelText, forId, action:'keep_unchecked'});
        }
      } else {
        // Garante MARCADO
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
        # Mensagem removida
      } else {
        # Mensagem de erro personalizada
        message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
      }
    }

    Sys.sleep(1.5)

    ## Pesquisar
    # Mensagem removida
    b$Runtime$evaluate("
    (function(){
      var btn = document.querySelector('input[type=submit][value=\"Pesquisar\"]');
      if (btn) btn.click();
    })();
  ")
    Sys.sleep(8)

    ## Espera pela tabela (versão corrigida com o ID CORRETO)
    # Mensagem removida
    t0 <- Sys.time()
    data_status <- "loading" # Initial state

    repeat {
      # Este JS verifica 3 estados:
      # 'success' (encontrou um código de município de 6 dígitos)
      # 'empty' (encontrou a msg 'Nenhum Registro')
      # 'loading' (nenhum dos anteriores)
      status_js <- "
      (function(){
        // Check 1: 'Nenhum Registro' (global message box)
        var msgs = document.querySelector(\"div[id$=':messages'] .ui-messages-summary\");
        if (msgs && /Nenhum Registro Encontrado!/i.test(msgs.innerText || '')) {
           return 'empty';
        }

        // Check 2: Table body (o <tbody>) - ID CORRIGIDO GRAÇAS À SUA IMAGEM
        var tbody = document.querySelector(\"[id='dosesAplicadasMensalForm:listaDoseAplicadasTable_data']\");
        if (!tbody) return 'loading'; // Tabela ainda não existe no DOM

        var txt = tbody.innerText || '';

        // Check 2a: 'Nenhum Registro' (dentro da tabela)
        // A sua imagem confirma exatamente este texto
        if (/Nenhum Registro Encontrado!/i.test(txt)) {
          return 'empty';
        }

        // Check 3: VERIFICAÇÃO DE DADOS REAIS
        // Procura por um código de município (um número de 6 dígitos)
        if (/\\b[0-9]{6}\\b/.test(txt)) {
          return 'success'; // Sucesso! Encontrou dados.
        }

        // Se não é 'empty' e não é 'success', continua 'loading'
        return 'loading';
      })();
    "

      data_status <- try(
        b$Runtime$evaluate(status_js)$result$value,
        silent = TRUE
      )

      if (inherits(data_status, "try-error")) data_status <- "loading"

      # CASO 1: SUCESSO (O seu caso!)
      if (data_status == "success") {
        # Mensagem removida
        break # Sai do loop e continua para o download do CSV
      }

      # CASO 2: VAZIO (Para consultas futuras)
      if (data_status == "empty") {
        # Esta mensagem foi mantida, pois é um resultado válido
        message("ℹ️ Pesquisa concluída: Nenhum Registro Encontrado!")
        try(b$close(), silent = TRUE)

        # Retorna um data.frame vazio com a estrutura correta
        df_long_empty <- data.frame(
          UF = character(), ano = integer(), cod_municipio = numeric(), # Corrigido para numeric
          nome_municipio = character(), mes = character(), dose = character(),
          estrategia = character(), produto = character(), quantidade = numeric(),
          stringsAsFactors = FALSE
        )
        df_long_empty <- df_long_empty[0, , drop = FALSE] # Garante 0 linhas

        # Adiciona os parâmetros da consulta para consistência
        df_long_empty$UF <- character(0)
        df_long_empty$ano <- integer(0)
        df_long_empty$estrategia <- character(0)
        df_long_empty$produto <- character(0)

        return(df_long_empty) # Encerra a função
      }

      # CASO 3: TIMEOUT
      if (difftime(Sys.time(), t0, units = 'secs') > 60) {
        # Mensagem de erro personalizada
        message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
        break
      }

      Sys.sleep(2)
    }

    ## ViewState (não é estritamente necessário se vamos serializar o form completo, mas mantemos por conferência)
    viewstate <- b$Runtime$evaluate("
    (function(){
      var vs = document.querySelector('input[name=\"javax.faces.ViewState\"]');
      return vs ? vs.value : null;
    })();
  ")$result$value
    if (is.null(viewstate) || identical(viewstate, "null")) {
      # Mensagem de erro personalizada
      message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
      return(invisible(b))
    }

    ## Cookies da sessão (para o httr)
    b$Network$enable()
    cks <- b$Network$getAllCookies()
    domain_cookies <- Filter(function(x) grepl('sipni\\.datasus\\.gov\\.br$', x$domain), cks$cookies)
    ck_list <- setNames(lapply(domain_cookies, `[[`, "value"),
                        sapply(domain_cookies, `[[`, "name"))
    ck <- do.call(httr::set_cookies, as.list(ck_list))

    ## Descobrir ID do botão CSV
    csv_btn_id <- b$Runtime$evaluate("
    (function(){
      const anchors = document.querySelectorAll('a[onclick*=\"mojarra.jsfcljs\"]');
      for (const a of anchors) {
        const html = a.outerHTML;
        if (/csv\\.png/i.test(html) || /CSV/i.test(html)) {
          const m = html.match(/'(dosesAplicadasMensalForm:[^']+)'/);
          if (m) return m[1];
        }
      }
      return 'dosesAplicadasMensalForm:j_idt1447';
    })();
  ")$result$value

    # 1º POST AJAX: gera estado parcial (opcional; mantemos)
    csv_url <- "http://sipni.datasus.gov.br/si-pni-web/faces/relatorio/consolidado/dosesAplicadasMensal.jsf"
    body_export <- list(
      "javax.faces.partial.ajax"    = "true",
      "javax.faces.source"          = csv_btn_id,
      "javax.faces.partial.execute" = "@all",
      "javax.faces.partial.render"  = csv_btn_id
    )
    body_export[[csv_btn_id]] <- csv_btn_id
    body_export[["javax.faces.ViewState"]] <- viewstate

    res <- httr::POST(
      url    = csv_url,
      body   = body_export,
      encode = "form",
      ck,
      httr::add_headers(
        "Faces-Request" = "partial/ajax",
        Referer         = csv_url,
        `User-Agent`    = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) R/httr"
      ),
      httr::timeout(180)
    )

    txt <- httr::content(res, "text", encoding = "ISO-8859-1")
    if (!grepl("<partial-response", txt)) {
      # Mensagem de erro personalizada
      message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
      return(invisible(b))
    }

    # (opcional) ViewState novo do parcial — mas vamos serializar o form no DOM, que já estará atualizado
    # new_viewstate <- sub(".*<update id=\"javax.faces.ViewState\"><!\\[CDATA\\[(.*?)\\]\\]>.*", "\\1", txt)

    # >>>>>>>>>>>>>>>>>>>>>>>>> CORREÇÃO PRINCIPAL <<<<<<<<<<<<<<<<<<<<<<<<<<
    # Serializa o formulário COMPLETO no DOM e adiciona o botão real como par nome=valor,
    # reproduzindo exatamente o que mojarra.jsfcljs(form, {...}) faz.
    ser <- b$Runtime$evaluate(sprintf("
    (function(){
      var f = document.getElementById('dosesAplicadasMensalForm');
      if(!f) return null;
      var fd = new FormData(f);
      fd.append('%s','%s'); // acrescenta o botão
      var usp = new URLSearchParams(fd);
      return usp.toString(); // corpo application/x-www-form-urlencoded
    })();
  ", csv_btn_id, csv_btn_id))$result$value

    if (is.null(ser) || identical(ser, "null") || !nzchar(ser)) {
      # Mensagem de erro personalizada
      message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
      return(invisible(b))
    }

    # 2º POST: enviar o formulário serializado por inteiro (não-AJAX), como o clique real faria
    res_final <- httr::POST(
      url    = csv_url,
      body   = charToRaw(ser),     # corpo já url-encoded
      encode = "raw",
      ck,
      httr::add_headers(
        "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
        Referer        = csv_url,
        `User-Agent`   = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) R/httr"
      ),
      httr::timeout(300)
    )

    ct_final <- httr::headers(res_final)[["content-type"]]
    if (!is.null(ct_final) && grepl("(text/csv|application/octet-stream)", ct_final, ignore.case = TRUE)) {
      tmp <- tempfile(fileext = ".csv")
      writeBin(httr::content(res_final, "raw"), tmp)
      dados <- readr::read_csv(tmp, show_col_types = FALSE)
      # Mensagem removida

      # 1. Limpeza e estrutura básica
      df <- dados %>%
        rename(municipio = 1) %>%
        mutate(
          cod_municipio  = str_extract(municipio, "^[0-9]+"),
          nome_municipio = str_remove(municipio, "^[0-9]+ - "),
          .before = municipio
        ) %>%
        select(-municipio)

      # 2. Contagem de colunas de dados
      ncols   <- ncol(df) - 2
      ndoses  <- length(dose_labels)
      nmeses  <- 12 # segurança
      meses   <- month.abb[1:nmeses]
      meses   <- rep(meses, each = ndoses)
      doses   <- rep(dose_labels, nmeses)

      # 3. Aplicar nomes estruturados
      names(df)[-(1:2)] <- sprintf("%s_%s", meses, doses)

      # 4. Pivotar para formato longo (tidy)
      df_long <- df %>%
        pivot_longer(
          cols = -c(cod_municipio, nome_municipio),
          names_to = c("mes", "dose"),
          names_sep = "_",
          values_to = "quantidade"
        ) %>%
        mutate(
          cod_municipio = as.numeric(cod_municipio), # Corrigido para numeric
          mes           = str_to_lower(mes),
          quantidade    = suppressWarnings(as.numeric(quantidade)),
          UF            = uf,
          ano           = as.integer(ano), # Corrigido para integer
          estrategia    = estrategia,
          produto       = produto
        ) %>%
        select(UF, ano, cod_municipio, nome_municipio, estrategia, produto, mes, dose, quantidade)

      message("✅ Dados baixados com sucesso!")
      return(df_long)

    } else {

      # Mensagem de erro personalizada
      message("⚠️ Erro, tente novente. (Caso o erro persista, print a tela e mande para a equipe do Data Zoom no github)")
      return(invisible(b))
    }

  } else {

    message("Iniciando scraper do InfoMS (Qlik) para >= 2023...")

    # --- Configuração ---
    url_painel <- "https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_OCORRENCIA.html"

    # Mapeamento de meses (o CSV virá com "jan", "fev", etc.)
    meses_map <- setNames(
      month.abb[1:12],
      c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
    )

    # --- Funções Auxiliares (Helpers) ---
    # Estas funções rodam dentro do navegador (via b$Runtime$evaluate)

    # Helper 1: Clica em um item de filtro (ex: clica em '2023' no filtro 'Ano')
    js_click_filter_item <- "
    (function(filterTitle, itemValue) {
      try {
        let filter = Array.from(document.querySelectorAll('div.qv-filterpane-data'))
                          .find(el => el.querySelector('div.qv-filter-title-text')?.innerText.trim() === filterTitle);
        if (!filter) throw new Error('Filtro ' + filterTitle + ' não encontrado');

        let item = Array.from(filter.querySelectorAll('li.qv-listbox-item'))
                        .find(el => el.querySelector('.qv-listbox-text')?.innerText.trim() === itemValue);
        if (!item) throw new Error('Item ' + itemValue + ' não encontrado no filtro ' + filterTitle);

        item.click();
        return { ok: true, filter: filterTitle, item: itemValue };
      } catch (e) {
        return { ok: false, error: e.message };
      }
    })"

    # Helper 2: Clica em um botão/aba pelo texto
    js_click_by_text <- "
    (function(elementType, text) {
      try {
        let el = Array.from(document.querySelectorAll(elementType))
                      .find(e => e.innerText.trim() === text);
        if (!el) throw new Error('Elemento ' + elementType + ' com texto ' + text + ' não encontrado');

        el.click();
        return { ok: true, text: text };
      } catch (e) {
        return { ok: false, error: e.message };
      }
    })"

    # Helper 3: Clica em um botão/aba pelo TÍTULO (title=)
    js_click_by_title <- "
    (function(elementType, title) {
      try {
        let el = document.querySelector(elementType + '[title=\"' + title + '\"]');
        if (!el) throw new Error('Elemento ' + elementType + ' com título ' + title + ' não encontrado');

        el.click();
        return { ok: true, title: title };
      } catch (e) {
        return { ok: false, error: e.message };
      }
    })"


    # 1. Cria o objeto NAVEGADOR
    b_browser <- chromote::Chromote$new()
    # 2. Cria a SESSÃO para a página principal
    b_main <- b_browser$new_session()
    # 3. Fecha o NAVEGADOR (e todas as sessões) ao sair
    on.exit(try(b_browser$close(), silent = TRUE), add = TRUE)

    # b_main$view() # remova o '#' da frente se precisar depurar visualmente

    # Define um diretório de download temporário
    temp_dir <- file.path(tempdir(), "pni_downloads")
    if (!dir.exists(temp_dir)) dir.create(temp_dir)

    # Define o comportamento de download no NAVEGADOR
    try(b_browser$Browser$setDownloadBehavior(
      behavior = "allow",
      downloadPath = temp_dir,
      eventsEnabled = TRUE
    ), silent = TRUE)


    message("Navegando para o painel... (Isso pode demorar até 30s)")
    b_main$Page$navigate(url_painel)

    # Espera longa para o Qlik carregar completely
    Sys.sleep(20)

    message("Buscando alvos (targets) do navegador...")

    # O NAVEGADOR (b_browser) é quem sabe dos alvos
    all_targets_df <- b_browser$get_targets()

    # Pega o ID do ALVO da sessão principal para podermos ignorá-lo
    main_target_id <- b_main$target_id
    message(paste("Alvo (target) principal:", main_target_id))

    # Converte o data.frame de alvos para uma lista de linhas
    all_targets_list <- split(all_targets_df, seq(nrow(all_targets_df)))

    iframe_target_info <- NULL

    for (t in all_targets_list) {
      # Verifica se 'type' e 'url' são válidos antes de usá-los
      type_is_valid <- !is.null(t$type) && !is.na(t$type)
      url_is_valid <- !is.null(t$url) && !is.na(t$url)

      if (type_is_valid && url_is_valid) {

        # LÓGICA:
        # 1. Seja do tipo 'page' (iframes Qlik são tratados como 'page')
        # 2. NÃO seja o nosso alvo principal
        # 3. Tenha 'qlik' ou 'sense' na URL

        is_page <- (t$type == 'page')
        is_not_main <- (t$targetId != main_target_id)
        is_qlik_url <- grepl("qlik|sense", t$url, ignore.case = TRUE)

        if (is_page && is_not_main && is_qlik_url) {
          message(paste("Debug: Alvo do Iframe encontrado! Tipo:", t$type, "URL:", t$url)) # Era tL$url
          iframe_target_info <- t
          break
        }
      }
    }


    if (is.null(iframe_target_info)) {
      message("⚠️ Erro: Nenhum alvo (target) de iframe ativo foi encontrado. O painel pode ter mudado.")
      # Imprime os alvos para depuração
      print(all_targets_df)
      return(NULL)
    }

    # Pega o ID da SESSÃO do alvo que encontramos
    iframe_session_id <- iframe_target_info$sessionId

    if (is.null(iframe_session_id) || is.na(iframe_session_id)) {
      message("⚠️ Erro: Alvo do Iframe encontrado, mas não tem sessionId. Não é possível conectar.")
      return(NULL)
    }

    message(paste("Sessão do Iframe encontrada:", iframe_session_id))

    # Agora, crie um NOVO OBJETO DE SESSÃO apontado para o iframe
    b_iframe <- b_browser$session(iframe_session_id)
    message("Nova sessão para o iframe criada com sucesso.")


    Sys.sleep(5) # Espera o contexto do iframe


    # --- 1. Aplicando Filtros ---
    message("Aplicando filtros...")

    # Ano
    # Note que agora usamos b_iframe
    res_ano <- b_iframe$Runtime$evaluate(paste0(js_click_filter_item, "('Ano Vacina', '", ano, "');"))
    if (!isTRUE(res_ano$result$value$ok)) message("Aviso: Filtro 'Ano' falhou. Continuando...")
    Sys.sleep(2)

    # UF Ocorrência
    res_uf <- b_iframe$Runtime$evaluate(paste0(js_click_filter_item, "('UF Ocorrência', '", uf, "');"))
    if (!isTRUE(res_uf$result$value$ok)) message("Aviso: Filtro 'UF' falhou. Continuando...")
    Sys.sleep(2)

    # Estratégia de Vacinação
    res_estrategia <- b_iframe$Runtime$evaluate(paste0(js_click_filter_item, "('Estratégia de Vacinação', '", estrategia, "');"))
    if (!isTRUE(res_estrategia$result$value$ok)) message("Aviso: Filtro 'Estratégia' falhou. Continuando...")
    Sys.sleep(2)

    # Imunobiológicos
    res_produto <- b_iframe$Runtime$evaluate(paste0(js_click_filter_item, "('Imunobiológicos', '", produto, "');"))
    if (!isTRUE(res_produto$result$value$ok)) message("Aviso: Filtro 'Imunobiológicos' falhou. Continuando...")
    Sys.sleep(3)

    # Doses (Vamos selecionar "Todas")
    # (Assumindo que "Doses" é o título do filtro)
    res_dose <- b_iframe$Runtime$evaluate(paste0(js_click_filter_item, "('Doses', '(Todas)');"))
    if (!isTRUE(res_dose$result$value$ok)) message("Aviso: Filtro 'Doses' falhou. (Isso pode ser normal)")
    Sys.sleep(3)


    # --- 2. Navegando para a Tabela ---
    message("Navegando para a aba 'Tabelas'...")
    res_tab <- b_iframe$Runtime$evaluate(paste0(js_click_by_text, "('div.qv-tab-name', 'Tabelas');"))
    if (!isTRUE(res_tab$result$value$ok)) {
      message("⚠️ Erro: Não foi possível clicar na aba 'Tabelas'.")
      return(NULL)
    }
    Sys.sleep(5) # Espera a tabela carregar


    # --- 3. Adicionando Dimensões (CRUCIAL) ---
    message("Adicionando dimensões à tabela...")

    # Adiciona "Código Município"
    res_dim_cod <- b_iframe$Runtime$evaluate(paste0(js_click_by_text, "('li.qv-dimension-list-item', 'Código Município');"))
    if (!isTRUE(res_dim_cod$result$value$ok)) message("Aviso: Não foi possível adicionar 'Código Município'.")
    Sys.sleep(2)

    # Adiciona "Tipo de Dose"
    res_dim_dose <- b_iframe$Runtime$evaluate(paste0(js_click_by_text, "('li.qv-dimension-list-item', 'Tipo de Dose');"))
    if (!isTRUE(res_dim_dose$result$value$ok)) message("Aviso: Não foi possível adicionar 'Tipo de Dose'.")
    Sys.sleep(2)


    # --- 4. Expandindo a Tabela ---
    message("Expandindo todas as linhas da tabela (clicando em '+')...")
    # Esta é uma suposição de seletor. Pode precisar de ajuste.
    js_expand_all <- "
    (function() {
      let plusButtons = document.querySelectorAll('button.lui-icon--plus');
      let count = 0;
      plusButtons.forEach(btn => {
        btn.click();
        count++;
      });
      return { clicked: count };
    })"

    res_expand <- b_iframe$Runtime$evaluate(js_expand_all)
    message(paste("Foram clicados", res_expand$result$value$clicked, "botões de expandir."))
    Sys.sleep(5) # Espera a expansão


    # --- 5. Baixando os Dados ---
    message("Clicando em 'Baixar Dados'...")

    # O botão de download está FORA do iframe.
    # Voltamos a usar a sessão principal 'b_main'
    res_download <- b_main$Runtime$evaluate(paste0(js_click_by_text, "('button.bp-button', 'Baixar Dados');"))
    if (!isTRUE(res_download$result$value$ok)) {
      message("⚠️ Erro: Não foi possível clicar em 'Baixar Dados'.")
      return(NULL)
    }

    # Espera o download ser concluído (pode precisar de ajuste)
    message("Aguardando o download do CSV...")
    Sys.sleep(10)


    # Lista os arquivos no diretório de download
    downloaded_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(downloaded_files) == 0) {
      message("⚠️ Erro: O arquivo CSV não foi encontrado no diretório temporário.")
      return(NULL)
    }

    # Pega o arquivo mais recente
    latest_csv <- downloaded_files[which.max(file.info(downloaded_files)$mtime)]
    message(paste("Arquivo baixado:", latest_csv))


    # --- 6. Processando e Formatando os Dados ---
    message("Processando e formatando o CSV baixado...")

    # O CSV do Qlik pode ter lixo no cabeçalho e rodapé.
    # Esta é uma suposição; pode precisar de ajuste.
    tryCatch({

      # Tentativa de ler o CSV.
      raw_data <- readr::read_csv(latest_csv, show_col_types = FALSE)

      # 1. Renomear colunas
      data_renamed <- raw_data %>%
        janitor::clean_names() # Transforma "Código Município" em "codigo_municipio"

      # 2. Pivotar (derreter) os meses
      data_long <- data_renamed %>%
        pivot_longer(
          cols = c(jan, fev, mar, abr, mai, jun, jul, ago, set, out, nov, dez),
          names_to = "mes_abrev",
          values_to = "quantidade"
        )

      # 3. Mapear e Adicionar colunas
      data_final <- data_long %>%
        mutate(
          # Adiciona colunas dos filtros
          UF = uf,
          ano = as.integer(ano),
          estrategia = estrategia,

          # Mapeia colunas existentes para o formato antigo
          cod_municipio = as.numeric(codigo_municipio),
          nome_municipio = municipio_ocorrencia,
          produto = imunobiologico, # ou "abreviacao_vacina" se estiver disponível
          dose = tipo_de_dose,
          mes = tolower(meses_map[mes_abrev]),

          # Limpa quantidade
          quantidade = as.numeric(quantidade)
        ) %>%
        filter(!is.na(quantidade) & quantidade > 0) # Remove linhas sem dados

      # 4. Selecionar e reordenar
      data_final_formatada <- data_final %>%
        select(
          UF, ano, cod_municipio, nome_municipio,
          estrategia, produto, mes, dose, quantidade
        )

      # Limpa o arquivo temporário
      unlink(latest_csv)

      message("✅ Dados do InfoMS (2023+) baixados e formatados com sucesso!")
      return(data_final_formatada)

    }, error = function(e) {
      message("⚠️ Erro CRÍTICO ao processar o arquivo CSV baixado.")
      message("O formato do CSV pode ter mudado. O scraper precisará de ajuste.")
      message("Erro R: ", e$message)
      return(NULL)
    })
  }
}

teste <- load_applied_vaccines(uf = "AC", ano = 2022, estrategia = "Rotina", produto = "Febre amarela - FA")
teste <- load_applied_vaccines(uf = "AC", ano = 2022)
