# =============================================================================
# DFA'da Orneklem Yeterliligi: Ampirik Parametre Geri-Kazanimi (Parameter Recovery)
#
# Amac: Guc analizi ile belirlenen orneklem buyuklugunun, DFA model
# parametrelerini (yukler, kesisimler, artik varyanslar, faktor kovaryanslari)
# yeterli dogrulukta kestirip kestirmedigini, buyuk/tam orneklem referansina
# karsi yanlilik (bias), ampirik standart hata, RMSE ve kapsama (coverage)
# olcutleriyle degerlendirmek.
#
# Yontem (ampirik yeniden ornekleme / bootstrap):
#   1. Tam veriye model uydurulur. Bu kestirimler "referans (gercek) deger"dir.
#   2. Her orneklem buyuklugu icin K kez iadeli orneklem cekilir, model kurulur,
#      parametreler kaydedilir.
#   3. Referansa gore yanlilik, ampirik SH, RMSE, kapsama ve yakinsama /
#      uygun-cozum oranlari hesaplanir.
#
# Kutuphaneler: shiny, lavaan, ggplot2, dplyr, sortable
# =============================================================================

library(shiny)
library(lavaan)
library(ggplot2)
library(dplyr)
library(sortable)

# Auto-install flextable and officer if not present
for (.pkg in c("flextable", "officer")) {
  if (!requireNamespace(.pkg, quietly = TRUE))
    install.packages(.pkg, repos = "https://cloud.r-project.org")
}
library(flextable)
library(officer)

# =============================================================================
# OZEL CSS
# =============================================================================
custom_css <- "
  body { background-color:#f0f2f7; font-family:'Segoe UI',Roboto,Arial,sans-serif; color:#2d3748; }
  .app-header {
    background: linear-gradient(135deg,#1a237e 0%,#283593 50%,#3949ab 100%);
    color:white; padding:20px 30px; margin:-15px -15px 20px -15px;
    border-radius:0 0 12px 12px; box-shadow:0 4px 15px rgba(26,35,126,0.35);
  }
  .app-header h2 { margin:0 0 5px 0; font-size:22px; font-weight:700; }
  .app-header p  { margin:0; font-size:13px; opacity:0.82; }
  .well { background:#fff; border:none; border-radius:12px;
          box-shadow:0 2px 12px rgba(0,0,0,0.09); padding:20px; }
  .section-label {
    font-size:11px; font-weight:700; text-transform:uppercase;
    letter-spacing:1px; color:#6c757d;
    margin:14px 0 8px 0; padding-bottom:4px; border-bottom:2px solid #e9ecef;
  }
  /* Bucket boxes */
  .rank-list-container {
    border-radius:10px !important; border:2px dashed #c5cae9 !important;
    background:#f8f9ff !important; min-height:60px !important; padding:8px !important;
    transition:border-color 0.2s,background 0.2s;
  }
  .rank-list-container:hover { border-color:#7986cb !important; background:#f0f2ff !important; }
  .rank-list-item {
    background:#fff !important; border:1px solid #dee2e6 !important;
    border-radius:7px !important; padding:5px 10px !important; margin:3px 0 !important;
    font-size:13px !important; font-weight:500 !important; cursor:grab !important;
    box-shadow:0 1px 4px rgba(0,0,0,0.08) !important; color:#3949ab !important;
    transition:transform 0.15s,box-shadow 0.15s !important;
  }
  .rank-list-item:hover { transform:translateY(-1px) !important;
    box-shadow:0 3px 10px rgba(57,73,171,0.18) !important; }
  .bucket-list-header { font-size:12px; font-weight:700; color:#5c6bc0;
    text-transform:uppercase; letter-spacing:0.5px; margin-bottom:4px; }
  /* Syntax box */
  .syntax-box {
    background:#1e1e2e; color:#cdd6f4; border-radius:8px;
    padding:10px 14px; font-family:'Courier New',monospace; font-size:12.5px;
    line-height:1.6; border:1px solid #313244; white-space:pre; overflow-x:auto;
  }
  .syntax-label { font-size:11px; font-weight:700; color:#6c757d;
    text-transform:uppercase; letter-spacing:0.8px; margin-bottom:5px; }
  /* Run button */
  #run_sim {
    background:linear-gradient(135deg,#1a237e,#3949ab) !important;
    border:none !important; border-radius:8px !important; color:white !important;
    font-weight:700 !important; font-size:14px !important; padding:10px !important;
    box-shadow:0 4px 12px rgba(26,35,126,0.35) !important;
    transition:all 0.2s !important;
  }
  #run_sim:hover { transform:translateY(-2px) !important;
    box-shadow:0 6px 18px rgba(26,35,126,0.45) !important; }
  /* Main cards */
  .main-card { background:#fff; border-radius:12px;
    box-shadow:0 2px 12px rgba(0,0,0,0.09); padding:20px 24px; margin-bottom:18px; }
  .card-title { font-size:15px; font-weight:700; color:#1a237e;
    border-left:4px solid #3949ab; padding-left:10px; margin-bottom:14px; }
  /* Info box */
  .info-box { background:linear-gradient(135deg,#e8eaf6,#f3f4ff);
    border-left:4px solid #3949ab; border-radius:0 8px 8px 0;
    padding:14px 18px; color:#333; }
  .info-box ol { padding-left:18px; margin:8px 0 0 0; }
  .info-box li { margin-bottom:4px; font-size:13.5px; }
  /* Benchmark badge */
  .bench-badge { background:#e8f5e9; border-left:4px solid #2e7d32;
    border-radius:0 8px 8px 0; padding:12px 16px; font-size:13px; color:#1b3a1d;
    margin-bottom:14px; }
  .bench-badge b { color:#1b5e20; }
  /* Warn / criteria badge */
  .warn-badge { background:#fff3e0; border:1px solid #ffcc80;
    border-radius:6px; padding:8px 12px; font-size:12px; color:#e65100; margin-top:6px;
    line-height:1.5; }
  /* Param row */
  .param-row { display:flex; gap:10px; }
  .param-row .form-group { flex:1; }
  /* Radio buttons */
  .inv-radio .radio { margin:4px 0; }
  .inv-radio label { font-size:13px; }
  /* Plot index selector */
  .plot-index-row { display:flex; align-items:center; gap:12px; margin-bottom:10px; }
  .plot-index-row label { font-weight:600; font-size:13px; color:#1a237e;
    margin:0; white-space:nowrap; }
  .plot-index-row .form-group { margin:0; flex:0 0 260px; }
  /* Scrollable benchmark table */
  .scroll-box { max-height:320px; overflow-y:auto; border:1px solid #e9ecef;
    border-radius:8px; }
  /* Download buttons */
  .dl-row { display:flex; gap:10px; flex-wrap:wrap; }
  .dl-row .btn { background:#3949ab; color:#fff; border:none; border-radius:8px;
    font-size:13px; font-weight:600; padding:8px 14px; }
  .dl-row .btn:hover { background:#283593; color:#fff; }
  /* Table header */
  .table thead tr th { background:#1a237e !important; color:white !important;
    font-weight:600; font-size:13px; }
  .table-striped tbody tr:nth-of-type(odd) { background-color:#f0f2ff; }
"

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  
  tags$head(tags$style(HTML(custom_css))),
  
  div(class = "app-header",
      tags$h2(HTML("&#x1F4CA; Sample Size Adequacy in CFA &#x2014; Empirical Parameter Recovery Simulation")),
      tags$p("Parameter Recovery | Empirical (Bootstrap) Resampling | Power-Analysis Validation")
  ),
  
  sidebarLayout(
    
    sidebarPanel(width = 4,
                 
                 # -- CSV --
                 div(class = "section-label", HTML("&#x1F4C2; Data Source")),
                 fileInput("data_file", NULL, accept = ".csv",
                           buttonLabel = "Browse", placeholder = "No CSV file selected..."),
                 
                 # -- Drag-and-drop factor setup --
                 div(class = "section-label", HTML("&#x1F9E9; Factor Model Specification")),
                 uiOutput("bucket_ui"),
                 uiOutput("syntax_preview"),
                 
                 hr(),
                 
                 # -- Simulation parameters --
                 div(class = "section-label", HTML("&#x2699;&#xFE0F; Simulation Parameters")),
                 textInput("sample_sizes", "Sample Sizes (comma-separated)",
                           value = "100, 200, 300, 500"),
                 div(class = "param-row",
                     numericInput("n_iter", "Replications / condition", 200, min = 20, max = 2000, step = 10),
                     numericInput("seed",   "Random seed",             2025, min = 1,  max = 999999, step = 1)
                 ),
                 checkboxInput("ordered_items",
                               HTML("Treat items as <b>ordered categorical</b> (true WLSMV)"),
                               value = TRUE),
                 checkboxInput("strict_categories",
                               HTML("Discard resamples with an <b>empty response category</b>"),
                               value = FALSE),
                 div(style = "font-size:11.5px; color:#6c757d; margin-top:-6px; margin-bottom:8px;",
                     HTML("When ticked, items are passed to lavaan via <code>ordered =</code>, so the model is\r
                           fitted to polychoric correlations with estimated thresholds. When unticked, the\r
                           items are treated as continuous and <code>estimator = \"WLSMV\"</code> reduces to DWLS\r
                           on the Pearson covariance matrix.")),
                 
                 br(),
                 actionButton("run_sim", HTML("&#x25B6;&nbsp; Run Simulation"), width = "100%"),
                 br(), br(),
                 div(class = "warn-badge",
                     HTML("&#x26A0; <b>Interpretation guidelines:</b> |Relative Bias| &lt; 5% negligible, 5&ndash;10% acceptable
              (Hoogland &amp; Boomsma, 1998) &nbsp;|&nbsp; Coverage should fall within 91&ndash;98%
              (Muth&eacute;n &amp; Muth&eacute;n, 2002) &nbsp;|&nbsp; Smaller RMSE / Emp. SE indicates more precise estimation.")
                 )
    ),
    
    mainPanel(width = 8,
              
              # -- Reference model card --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F3AF; Reference Model (Full Sample = Population Truth)")),
                  uiOutput("benchmark_info"),
                  uiOutput("benchmark_table_ui"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_bench_csv",  "&#x1F4C5; Download CSV"),
                      downloadButton("dl_bench_docx", "&#x1F4C4; Download DOCX (APA 7)")
                  )
              ),
              
              # -- Recovery plot card --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F4C8; Parameter Recovery by Sample Size")),
                  uiOutput("plot_metric_ui"),
                  plotOutput("recovery_plot", height = "380px"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_recovery_png",  "&#x1F4F7; PNG (300 dpi)"),
                      downloadButton("dl_recovery_tiff", "&#x1F5BC; TIFF (300 dpi)"),
                      downloadButton("dl_recovery_pdf",  "&#x1F4C4; PDF (Vector)")
                  )
              ),
              
              # -- Absolute fit indices: RMSEA + SRMR --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F4C9; Model Fit Indices &#x2014; Absolute Fit (RMSEA &amp; SRMR)")),
                  plotOutput("fit_abs_plot", height = "340px"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_fit_abs_png",  "&#x1F4F7; PNG (300 dpi)"),
                      downloadButton("dl_fit_abs_tiff", "&#x1F5BC; TIFF (300 dpi)"),
                      downloadButton("dl_fit_abs_pdf",  "&#x1F4C4; PDF (Vector)")
                  )
              ),
              
              # -- Incremental fit indices: CFI + TLI --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F4C8; Model Fit Indices &#x2014; Incremental Fit (CFI &amp; TLI)")),
                  plotOutput("fit_inc_plot", height = "340px"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_fit_inc_png",  "&#x1F4F7; PNG (300 dpi)"),
                      downloadButton("dl_fit_inc_tiff", "&#x1F5BC; TIFF (300 dpi)"),
                      downloadButton("dl_fit_inc_pdf",  "&#x1F4C4; PDF (Vector)")
                  )
              ),
              
              # -- Condition health card --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1FA7A; Simulation Condition Health (Convergence &amp; Proper Solutions)")),
                  tableOutput("health_table"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_health_csv",  "&#x1F4C5; Download CSV"),
                      downloadButton("dl_health_docx", "&#x1F4C4; Download DOCX (APA 7)")
                  )
              ),
              
              # -- Recovery summary table card --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F4CB; Parameter Recovery Summary")),
                  tableOutput("recovery_table"),
                  div(class = "dl-row", style = "margin-top:10px;",
                      downloadButton("dl_recovery_csv",  "&#x1F4C5; Download CSV"),
                      downloadButton("dl_recovery_docx", "&#x1F4C4; Download DOCX (APA 7)")
                  )
              ),
              
              # -- Raw data export card --
              div(class = "main-card",
                  div(class = "card-title", HTML("&#x1F4BE; Export Raw Data (CSV)")),
                  div(class = "dl-row",
                      downloadButton("dl_raw",      "Raw Iterations"),
                      downloadButton("dl_param",    "Parameter-Level Summary"),
                      downloadButton("dl_fam",      "Family-Level Summary"),
                      downloadButton("dl_fit_summ", "Fit Index Summary")
                  )
              ),
              
              uiOutput("info_message")
    )
  )
)

# =============================================================================
# YARDIMCI FONKSIYONLAR
# =============================================================================

# Bir lavaan fit nesnesinden ilgilenilen parametreleri (aile etiketiyle) cek
extract_params <- function(fit, items, factors) {
  pe <- lavaan::parameterEstimates(fit, se = TRUE, ci = TRUE, level = 0.95)
  # ci.lower / ci.upper sütunlarının varlığını güvence altına al
  if (!"ci.lower" %in% names(pe)) pe$ci.lower <- NA_real_
  if (!"ci.upper" %in% names(pe)) pe$ci.upper <- NA_real_
  fam <- rep(NA_character_, nrow(pe))
  fam[pe$op == "=~"] <- "Factor Loadings"
  fam[pe$op == "|"  & pe$lhs %in% items] <- "Thresholds"
  fam[pe$op == "~1" & pe$lhs %in% items] <- "Item Intercepts"
  fam[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% items] <- "Residual Variances"
  if (length(factors) >= 2) {
    fam[pe$op == "~~" & pe$lhs != pe$rhs &
          pe$lhs %in% factors & pe$rhs %in% factors] <- "Factor Covariances"
  }
  pe$family <- fam
  pe$key    <- paste(pe$lhs, pe$op, pe$rhs)
  # Yalnizca serbestce kestirilen parametreleri tut. Kategorik WLSMV'de
  # (delta parametrelemesi) artik varyanslar serbest degil, turetilmistir;
  # bunlarin geri kazanimini raporlamak yaniltici olur.
  free_keys <- tryCatch({
    pt <- lavaan::parTable(fit)
    unique(paste(pt$lhs, pt$op, pt$rhs)[pt$free > 0])
  }, error = function(e) NULL)
  keep <- !is.na(pe$family)
  if (!is.null(free_keys)) keep <- keep & (pe$key %in% free_keys)
  pe[keep, c("key", "family", "lhs", "op", "rhs", "est", "se", "ci.lower", "ci.upper")]
}

# Uygun cozum denetimi: post.check — farkli lavaan surumlerinde guvenli cagri
is_proper_solution <- function(fit) {
  tryCatch({
    # lavaan >= 0.6-13'te lavInspect tercih edilir
    result <- lavaan::lavInspect(fit, "post.check")
    isTRUE(result)
  }, warning = function(w) {
    # Uyarı = uygunsuz çözüm (Heywood vakası vb.)
    FALSE
  }, error = function(e) {
    # Hata durumunda da FALSE
    FALSE
  })
}

# Sonlu olmayan degerleri tire ile, sonlulari sabit ondalikla bicimle
fmt <- function(x, d) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "\u2014")

# Aile siralamasi ve duzeye gore gosterilecek aileler
FAM_ORDER <- c("Factor Loadings", "Thresholds", "Item Intercepts",
               "Residual Variances", "Factor Covariances")

# Yeniden ornekte her maddenin tum kategorileri temsil ediliyor mu?
# Bir kategori dusserse o madde icin daha az esik kestirilir ve model
# referans modelle karsilastirilamaz hale gelir (bkz. Myers ve ark., 2011).
has_all_categories <- function(d, items, cat_list) {
  for (it in items) {
    if (!all(cat_list[[it]] %in% unique(d[[it]]))) return(FALSE)
  }
  TRUE
}

# Esik etiketlerini tam veri kategorilerine hizala.
# lavaan esikleri yeniden ornekte GOZLENEN kategorilere gore t1, t2, ...
# diye yeniden numaralar. Alt ya da orta bir kategori bos kalirsa etiketler
# kayar (ornegin 0 kategorisi yoksa yeniden ornegin t1'i referansin t2'sine
# karsilik gelir). Yeniden ornekteki j. esik, j. gozlenen kategorinin hemen
# ustundeki sinirdir; referanstaki sirasi bu kategorinin tam veri
# kategorileri icindeki konumudur. Bos kategorinin ustundeki referans esigi
# o replikasyonda kestirilemez ve geri kazanim hesabina girmez.
align_threshold_keys <- function(pe, d, items, cat_list) {
  is_th <- pe$op == "|"
  if (!any(is_th)) return(pe)
  for (it in intersect(unique(pe$lhs[is_th]), items)) {
    rows    <- which(is_th & pe$lhs == it)
    rows    <- rows[order(as.integer(sub("^t", "", pe$rhs[rows])))]
    present <- sort(unique(d[[it]]))
    ref_idx <- match(present[-length(present)], cat_list[[it]])
    if (length(ref_idx) != length(rows) || anyNA(ref_idx)) {
      pe$key[rows] <- NA_character_
      next
    }
    pe$rhs[rows] <- paste0("t", ref_idx)
    pe$key[rows] <- paste(pe$lhs[rows], pe$op[rows], pe$rhs[rows])
  }
  pe
}

# Uyum indekslerini duzeltilmemis ve olceklenmis olarak birlikte al
grab_fit <- function(fit) {
  want <- c("cfi", "tli", "rmsea", "srmr",
            "cfi.scaled", "tli.scaled", "rmsea.scaled")
  out <- setNames(rep(NA_real_, length(want)), want)
  fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
  if (is.null(fm)) return(out)
  have <- intersect(want, names(fm))
  out[have] <- as.numeric(fm[have])
  out
}


# -----------------------------------------------------------------------------
# APA 7 uyumlu figur temasi
# Figurun uzerinde baslik/altbaslik YOK. APA 7'de figur numarasi ve basligi
# figurun ustune, aciklama (Note) altina makale metninde yazilir. Bu tema
# duz siyah metin, sans-serif yazi tipi ve minimum kilavuz cizgisi kullanir.
# -----------------------------------------------------------------------------
theme_apa <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text               = ggplot2::element_text(colour = "black"),
      axis.text          = ggplot2::element_text(colour = "black", size = base_size),
      axis.title         = ggplot2::element_text(colour = "black", size = base_size),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks         = ggplot2::element_line(colour = "black", linewidth = 0.4),
      panel.grid.major.y = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(colour = "black", size = base_size),
      legend.text        = ggplot2::element_text(colour = "black", size = base_size),
      legend.key         = ggplot2::element_blank(),
      legend.background  = ggplot2::element_blank(),
      plot.title         = ggplot2::element_blank(),
      plot.subtitle      = ggplot2::element_blank(),
      plot.caption       = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(4, 8, 4, 4),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA)
    )
}




# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # CSV oku
  # ---------------------------------------------------------------------------
  main_data <- reactive({
    req(input$data_file)
    tryCatch(
      read.csv(input$data_file$datapath, sep = ",", stringsAsFactors = FALSE),
      error = function(e) {
        showNotification(paste("CSV okuma hatas\u0131:", e$message), type = "error", duration = 8)
        NULL
      }
    )
  })
  
  all_vars <- reactive({
    req(main_data())
    setdiff(names(main_data()), "group")
  })
  
  # ---------------------------------------------------------------------------
  # Factor Builder
  # ---------------------------------------------------------------------------
  output$bucket_ui <- renderUI({
    req(all_vars())
    tagList(
      p(style = "font-size:12px; color:#6c757d; margin-bottom:8px;",
        HTML("&#x1F449; Drag variables from the pool into the factor boxes below.")),
      bucket_list(
        header      = NULL,
        group_name  = "factor_buckets",
        orientation = "vertical",
        add_rank_list(
          text     = HTML("<span class='bucket-list-header'>&#x1F4E6; Variable Pool</span>"),
          labels   = as.list(all_vars()),
          input_id = "available_vars"
        ),
        add_rank_list(
          text     = HTML("<span class='bucket-list-header' style='color:#2e7d32;'>&#x1F7E2; Factor 1 (F1)</span>"),
          labels   = NULL,
          input_id = "f1_vars"
        ),
        add_rank_list(
          text     = HTML("<span class='bucket-list-header' style='color:#c62828;'>&#x1F534; Factor 2 (F2)</span>"),
          labels   = NULL,
          input_id = "f2_vars"
        )
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # Syntax Preview
  # ---------------------------------------------------------------------------
  model_syntax <- reactive({
    f1 <- input$f1_vars
    f2 <- input$f2_vars
    if (length(f1) == 0 && length(f2) == 0) return(NULL)
    lines <- c()
    if (length(f1) > 0) lines <- c(lines, paste0("F1 =~ ", paste(f1, collapse = " + ")))
    if (length(f2) > 0) lines <- c(lines, paste0("F2 =~ ", paste(f2, collapse = " + ")))
    paste(lines, collapse = "\n")
  })
  
  output$syntax_preview <- renderUI({
    syn <- model_syntax()
    if (is.null(syn)) return(NULL)
    tagList(
      br(),
      div(class = "syntax-label", HTML("&#x1F527; Generated lavaan Syntax")),
      div(class = "syntax-box", syn)
    )
  })
  
  # ---------------------------------------------------------------------------
  # Simulasyon durumu
  # ---------------------------------------------------------------------------
  sim_state <- reactiveValues(
    long       = NULL,   # ham iterasyon verisi (uzun format)
    param_summ = NULL,   # parametre duzeyi ozet
    fam_summ   = NULL,   # aile duzeyi ozet
    fit_summ   = NULL,   # uyum indeksleri ozeti
    health     = NULL,   # kosul sagligi
    bench_tbl  = NULL,   # referans parametre tablosu
    bench_fit  = NULL,   # referans uyum indeksleri
    bench_n    = NULL,   # tam orneklem N
    seed       = NULL
  )
  
  # ---------------------------------------------------------------------------
  # SIMULASYON
  # ---------------------------------------------------------------------------
  observeEvent(input$run_sim, {
    
    req(input$data_file)
    veri <- main_data()
    if (is.null(veri)) return()
    
    N_full <- nrow(veri)
    if (N_full < 30) {
      showNotification("Dataset too small. At least 30 observations are required.",
                       type = "error", duration = 6); return()
    }
    
    syn <- model_syntax()
    if (is.null(syn)) {
      showNotification("Please assign at least one variable to a factor.",
                       type = "warning", duration = 5); return()
    }
    
    # Orneklem buyukluklerini ayristir
    ns <- suppressWarnings(as.integer(trimws(strsplit(input$sample_sizes, ",")[[1]])))
    ns <- sort(unique(ns[is.finite(ns) & ns >= 30]))
    if (length(ns) == 0) {
      showNotification("No valid sample sizes found (minimum 30, comma-separated).",
                       type = "error", duration = 7); return()
    }
    
    K          <- input$n_iter
    seed       <- input$seed
    
    items   <- c(input$f1_vars, input$f2_vars)
    factors <- character(0)
    if (length(input$f1_vars) > 0) factors <- c(factors, "F1")
    if (length(input$f2_vars) > 0) factors <- c(factors, "F2")
    
    # -- ADIM 1: Referans model (tam orneklem) --
    use_ordered <- isTRUE(input$ordered_items)
    ord_arg     <- if (use_ordered) items else character(0)
    cat_list    <- lapply(setNames(items, items), function(it) sort(unique(veri[[it]])))

    fit_full <- tryCatch(
      suppressWarnings(lavaan::cfa(syn, veri, std.lv = TRUE, estimator = "WLSMV",
                                   ordered = ord_arg)),
      error = function(e) NULL
    )
    if (is.null(fit_full)) {
      showNotification(
        "Reference model could not be estimated. Please check the model syntax and data.",
        type = "error", duration = 9)
      sim_state$long <- NULL; return()
    }
    conv_full <- tryCatch(isTRUE(lavaan::lavInspect(fit_full, "converged")),
                          error = function(e) FALSE)
    if (!conv_full) {
      showNotification(
        "Reference model did not converge on the full sample. Please check the model specification.",
        type = "error", duration = 9)
      sim_state$long <- NULL; return()
    }
    prop_full <- is_proper_solution(fit_full)
    if (!prop_full) {
      showNotification(
        "Reference model yielded an improper solution (negative residual variance / Heywood case). Please revise the model.",
        type = "error", duration = 9)
      sim_state$long <- NULL; return()
    }
    
    pe_bench   <- extract_params(fit_full, items, factors)
    bench_est  <- setNames(pe_bench$est, pe_bench$key)
    bench_fit  <- tryCatch(
      c(grab_fit(fit_full),
        npar = as.numeric(lavaan::fitMeasures(fit_full, "npar"))),
      error = function(e) NULL
    )
    
    # Onceki sonuclari sifirla
    sim_state$long <- NULL
    
    # -- ADIM 2: Yeniden ornekleme dongusu --
    set.seed(seed)
    total_fits <- length(ns) * K
    long_list  <- list()
    fit_list   <- list()
    health_list <- list()
    
    withProgress(message = "Running simulation...", value = 0, {
      for (nn in ns) {
        
        n_conv <- 0; n_prop <- 0; n_incomp <- 0
        for (i in seq_len(K)) {
          
          incProgress(1 / total_fits,
                      detail = sprintf("n=%d | replication %d/%d | proper: %d", nn, i, K, n_prop))
          
          idx <- sample(N_full, nn, replace = TRUE)
          d   <- veri[idx, , drop = FALSE]
          
          # Bos kategori iceren yeniden ornek: her zaman sayilir, ancak
          # yalnizca kati politika secildiyse atilir. Atilmazsa lavaan o madde
          # icin daha az esik kestirir; esikler align_threshold_keys() ile
          # referans siniflarina hizalanir, bos kategorinin esigi hesaptan duser.
          incomplete_cat <- use_ordered && !has_all_categories(d, items, cat_list)
          if (incomplete_cat) {
            n_incomp <- n_incomp + 1
            if (isTRUE(input$strict_categories)) next
          }
          
          fit <- tryCatch(
            suppressWarnings(lavaan::cfa(syn, d, std.lv = TRUE, estimator = "WLSMV",
                                         ordered = ord_arg)),
            error = function(e) NULL
          )
          if (is.null(fit)) next
          conv_ok <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")),
                              error = function(e) FALSE)
          if (!conv_ok) next
          n_conv <- n_conv + 1
          
          if (!is_proper_solution(fit)) next
          n_prop <- n_prop + 1
          
          # Uyum indeksleri kaydet
          fi <- grab_fit(fit)
          fit_list[[length(fit_list) + 1]] <- data.frame(
            condition    = nn,
            cfi          = fi["cfi"],          tli          = fi["tli"],
            rmsea        = fi["rmsea"],        srmr         = fi["srmr"],
            cfi_scaled   = fi["cfi.scaled"],   tli_scaled   = fi["tli.scaled"],
            rmsea_scaled = fi["rmsea.scaled"],
            stringsAsFactors = FALSE
          )
          
          pe <- extract_params(fit, items, factors)
          if (use_ordered) pe <- align_threshold_keys(pe, d, items, cat_list)
          # bench_est'te karşılığı olmayan parametreler NA döner;
          # bu satırları kapsama/bias hesabından dışla ama kaydı bozmadan bırak
          pe$bench     <- bench_est[pe$key]
          pe$condition <- nn
          pe$iter      <- i
          # Referans değeri bilinmeyen satırları at
          pe <- pe[!is.na(pe$bench), , drop = FALSE]
          if (nrow(pe) == 0) next
          long_list[[length(long_list) + 1]] <- pe
        }
        
        health_list[[length(health_list) + 1]] <- data.frame(
          condition = nn,
          requested = K,
          incomplete = n_incomp,
          converged = n_conv,
          proper    = n_prop,
          used      = n_prop,
          incomp_rate = round(100 * n_incomp / K, 1),
          conv_rate = round(100 * n_conv / K, 1),
          prop_rate = round(100 * n_prop / K, 1)
        )
      }
    })
    
    if (length(long_list) == 0) {
      showNotification(
        "No usable results from any replication. Consider increasing sample sizes or simplifying the model.",
        type = "error", duration = 10)
      sim_state$long <- NULL; return()
    }
    
    long_df <- dplyr::bind_rows(long_list)

    # Hicbir kullanilabilir replikasyon uretmeyen kosullar sessizce
    # kaybolmasin; kullaniciya acikca bildir.
    empty_conds <- setdiff(ns, unique(long_df$condition))
    if (length(empty_conds) > 0) {
      showNotification(
        HTML(paste0(
          "<b>No usable replication at n = ",
          paste(empty_conds, collapse = ", "), ".</b><br>",
          "With ordered estimation every item must show all of its response ",
          "categories in a resample. Check the 'Empty Category' column in the ",
          "condition health table, and consider unticking the strict option or ",
          "collapsing rare categories.")),
        type = "warning", duration = NULL)
    }
    
    # -- ADIM 3: Parametre duzeyi ozet (referansa gore) --
    param_summ <- long_df %>%
      dplyr::group_by(condition, family, key, lhs, op, rhs) %>%
      dplyr::summarise(
        bench    = dplyr::first(bench),
        mean_est = mean(est),
        bias     = mean(est) - dplyr::first(bench),
        rel_bias = ifelse(abs(dplyr::first(bench)) > 0.01,
                          (mean(est) - dplyr::first(bench)) / dplyr::first(bench) * 100,
                          NA_real_),
        emp_se   = sd(est),
        rmse     = sqrt(mean((est - dplyr::first(bench))^2)),
        coverage = mean(bench >= ci.lower & bench <= ci.upper, na.rm = TRUE) * 100,
        .groups  = "drop"
      )
    
    # -- Aile duzeyi ozet --
    fam_summ <- param_summ %>%
      dplyr::group_by(condition, family) %>%
      dplyr::summarise(
        k                 = dplyr::n(),
        mean_bias         = mean(bias, na.rm = TRUE),
        mean_abs_relbias  = mean(abs(rel_bias), na.rm = TRUE),
        mean_emp_se       = mean(emp_se, na.rm = TRUE),
        mean_rmse         = mean(rmse, na.rm = TRUE),
        mean_coverage     = mean(coverage, na.rm = TRUE),
        .groups           = "drop"
      )
    
    # -- Uyum indeksi ozeti --
    fit_summ <- if (length(fit_list) > 0) {
      fd <- dplyr::bind_rows(fit_list)
      fd %>%
        dplyr::group_by(condition) %>%
        dplyr::summarise(
          mean_cfi   = mean(cfi,   na.rm = TRUE),
          sd_cfi     = sd(cfi,     na.rm = TRUE),
          mean_tli   = mean(tli,   na.rm = TRUE),
          sd_tli     = sd(tli,     na.rm = TRUE),
          mean_rmsea = mean(rmsea, na.rm = TRUE),
          sd_rmsea   = sd(rmsea,   na.rm = TRUE),
          mean_srmr  = mean(srmr,  na.rm = TRUE),
          sd_srmr    = sd(srmr,    na.rm = TRUE),
          mean_cfi_scaled   = mean(cfi_scaled,   na.rm = TRUE),
          sd_cfi_scaled     = sd(cfi_scaled,     na.rm = TRUE),
          mean_tli_scaled   = mean(tli_scaled,   na.rm = TRUE),
          sd_tli_scaled     = sd(tli_scaled,     na.rm = TRUE),
          mean_rmsea_scaled = mean(rmsea_scaled, na.rm = TRUE),
          sd_rmsea_scaled   = sd(rmsea_scaled,   na.rm = TRUE),
          .groups    = "drop"
        )
    } else NULL
    
    # Kaydet
    sim_state$long       <- long_df
    sim_state$param_summ <- param_summ
    sim_state$fam_summ   <- fam_summ
    sim_state$fit_summ   <- fit_summ
    sim_state$health     <- dplyr::bind_rows(health_list)
    sim_state$bench_tbl  <- pe_bench
    sim_state$bench_fit  <- bench_fit
    sim_state$bench_n    <- N_full
    sim_state$seed       <- seed
    
    showNotification(
      sprintf("\u2705 Simulation complete. %d condition(s) \u00d7 %d replications.", length(ns), K),
      type = "message", duration = 6)
  })
  
  # ---------------------------------------------------------------------------
  # REFERANS BILGISI
  # ---------------------------------------------------------------------------
  output$benchmark_info <- renderUI({
    if (is.null(sim_state$bench_fit)) {
      return(div(class = "bench-badge",
                 HTML("Not yet estimated. Full-sample parameter estimates will serve as population truth (reference values) once the simulation is run.")))
    }
    bf <- sim_state$bench_fit
    div(class = "bench-badge",
        HTML(sprintf(
          "<b>Full sample:</b> N = %d &nbsp;|&nbsp; <b>Free parameters:</b> %d<br>
         <b>Reference model fit (scaled):</b> CFI = %.3f &nbsp; TLI = %.3f &nbsp; RMSEA = %.3f &nbsp; SRMR = %.3f<br>
         <span style='font-size:11.5px;'>Unscaled (report only if the estimator is not WLSMV): CFI = %.3f &nbsp; TLI = %.3f &nbsp; RMSEA = %.3f</span><br>
         <span style='font-size:11.5px;'>Identification: <code>std.lv = TRUE</code> (factor variances fixed to 1) &mdash; all loadings freely estimated and comparable across conditions. Resampling: bootstrap with replacement.</span>",
          sim_state$bench_n, as.integer(bf["npar"]),
          bf["cfi.scaled"], bf["tli.scaled"], bf["rmsea.scaled"], bf["srmr"],
          bf["cfi"], bf["tli"], bf["rmsea"]
        ))
    )
  })
  
  output$benchmark_table_ui <- renderUI({
    req(sim_state$bench_tbl)
    div(class = "scroll-box", tableOutput("benchmark_table"))
  })
  
  output$benchmark_table <- renderTable({
    req(sim_state$bench_tbl)
    bt <- sim_state$bench_tbl
    bt <- bt[order(match(bt$family, FAM_ORDER), bt$key), ]
    df_out <- data.frame(
      Parameter        = bt$key,
      Family           = bt$family,
      ReferenceValue   = formatC(bt$est, format = "f", digits = 4),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
    setNames(df_out, c("Parameter", "Parameter Family", "Reference Value"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", width = "100%")
  
  # ---------------------------------------------------------------------------
  # GRAFIK METRIGI SECICI
  # ---------------------------------------------------------------------------
  output$plot_metric_ui <- renderUI({
    req(sim_state$fam_summ)
    div(class = "plot-index-row",
        tags$label("Metric to display:"),
        div(style = "width:260px;",
            selectInput("plot_metric", NULL,
                        choices = c("RMSE"                        = "mean_rmse",
                                    "Empirical Standard Error"    = "mean_emp_se",
                                    "|Relative Bias| (%)"         = "mean_abs_relbias",
                                    "Coverage (%)"                = "mean_coverage"),
                        selected = "mean_rmse", width = "100%")
        )
    )
  })
  
  # ---------------------------------------------------------------------------
  # CIKTI 1: Geri-kazanim grafigi (reactive + renderPlot + indirme)
  # ---------------------------------------------------------------------------
  recovery_gg <- reactive({
    req(sim_state$fam_summ, input$plot_metric)
    fs    <- sim_state$fam_summ
    metr  <- input$plot_metric
    disp  <- c("Factor Loadings", "Thresholds", "Residual Variances", "Factor Covariances")
    fs    <- fs[fs$family %in% disp, ]
    if (nrow(fs) == 0) return(NULL)
    
    lab <- c(mean_rmse = "RMSE", mean_emp_se = "Empirical standard error",
             mean_abs_relbias = "|Relative bias| (%)", mean_coverage = "Coverage (%)")[metr]
    is_cov <- metr == "mean_coverage"
    
    fs$condition_num <- as.numeric(as.character(fs$condition))
    fs$metric_val    <- fs[[metr]]
    fs$family        <- factor(fs$family, levels = FAM_ORDER)
    
    fs_plot <- fs[, c("condition_num", "family", "metric_val")]
    
    all_breaks <- sort(unique(fs_plot$condition_num))
    all_labels <- as.character(all_breaks)
    
    p <- ggplot(fs_plot, aes(x = condition_num, y = metric_val,
                             color = family, group = family)) +
      geom_line(aes(linetype = family), linewidth = 0.8) +
      geom_point(aes(shape = family), size = 2.6) +
      scale_x_continuous(breaks = all_breaks, labels = all_labels)
    
    if (is_cov) {
      p <- p +
        geom_hline(yintercept = 95, linetype = "dashed",
                   color = "black", linewidth = 0.4)
    }
    
    p +
      scale_color_manual(values = c(
        "Factor Loadings"    = "#3949ab",
        "Thresholds"         = "#e65100",
        "Item Intercepts"    = "#00897b",
        "Residual Variances" = "#7b1fa2",
        "Factor Covariances" = "#8e24aa")) +
      scale_linetype_manual(values = c(
        "Factor Loadings"    = "solid",
        "Thresholds"         = "dashed",
        "Item Intercepts"    = "dotdash",
        "Residual Variances" = "longdash",
        "Factor Covariances" = "twodash")) +
      scale_shape_manual(values = c(
        "Factor Loadings"    = 16,
        "Thresholds"         = 17,
        "Item Intercepts"    = 15,
        "Residual Variances" = 8,
        "Factor Covariances" = 18)) +
      labs(x = "Sample size (n)", y = lab,
           color = "Parameter family", linetype = "Parameter family",
           shape = "Parameter family") +
      theme_apa()
  })
  
  output$recovery_plot <- renderPlot({ req(recovery_gg()); recovery_gg() }, bg = "white")
  
  output$dl_recovery_png <- downloadHandler(
    filename = function() paste0("parameter_recovery_", Sys.Date(), ".png"),
    content  = function(file)
      ggsave(file, plot = recovery_gg(), device = "png",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  output$dl_recovery_pdf <- downloadHandler(
    filename = function() paste0("parameter_recovery_", Sys.Date(), ".pdf"),
    content  = function(file)
      ggsave(file, plot = recovery_gg(), device = "pdf",
             width = 18, height = 12, units = "cm")
  )
  output$dl_recovery_tiff <- downloadHandler(
    filename = function() paste0("parameter_recovery_", Sys.Date(), ".tiff"),
    content  = function(file)
      ggsave(file, plot = recovery_gg(), device = "tiff",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  
  # ---------------------------------------------------------------------------
  # CIKTI 2: Kosul sagligi tablosu
  # ---------------------------------------------------------------------------
  output$health_table <- renderTable({
    req(sim_state$health)
    h <- sim_state$health[order(sim_state$health$condition), ]
    df_out <- data.frame(
      SampleN      = h$condition,
      RequestedIter = h$requested,
      Incomplete   = h$incomplete,
      Converged    = h$converged,
      ProperSol    = h$proper,
      Used         = h$used,
      IncompRate   = fmt(h$incomp_rate, 1),
      ConvergRate  = fmt(h$conv_rate, 1),
      ProperRate   = fmt(h$prop_rate, 1),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    setNames(df_out, c(
      "Sample (n)",
      "Requested Iter.",
      "Empty Category",
      "Converged",
      "Proper Solution",
      "Used",
      "Empty Category (%)",
      "Convergence (%)",
      "Proper Solution (%)"
    ))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m", width = "100%")
  
  # ---------------------------------------------------------------------------
  # CIKTI 3: Parametre geri-kazanim ozet tablosu (aile duzeyi)
  # ---------------------------------------------------------------------------
  output$recovery_table <- renderTable({
    req(sim_state$fam_summ, sim_state$bench_tbl)
    fs   <- sim_state$fam_summ
    disp <- c("Factor Loadings", "Thresholds", "Residual Variances", "Factor Covariances")
    fs   <- fs[fs$family %in% disp, ]
    fs   <- fs[order(fs$condition, match(fs$family, FAM_ORDER)), ]
    
    # Family-level reference values (full sample)
    bt        <- sim_state$bench_tbl[sim_state$bench_tbl$family %in% disp, ]
    bench_ref <- tapply(bt$est, bt$family, function(x) mean(abs(x), na.rm = TRUE))
    bench_k   <- tapply(bt$est, bt$family, length)
    n_label   <- paste0("Full (N=", sim_state$bench_n, ")")
    
    # Full-sample reference rows
    ref_fams <- intersect(names(bench_ref), disp)
    ref_fams <- ref_fams[order(match(ref_fams, FAM_ORDER))]
    ref_rows <- data.frame(
      SampleN       = rep(n_label, length(ref_fams)),
      ParamFamily   = ref_fams,
      ParamCount    = as.numeric(bench_k[ref_fams]),
      MeanReference = fmt(bench_ref[ref_fams], 4),
      MeanBias      = rep("0.0000", length(ref_fams)),
      MeanAbsRelB   = rep("0.00",   length(ref_fams)),
      MeanEmpSE     = rep("\u2014",  length(ref_fams)),
      MeanRMSE      = rep("0.0000", length(ref_fams)),
      MeanCoverage  = rep("(~95)",  length(ref_fams)),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    
    mean_ref <- fmt(bench_ref[fs$family], 4)
    
    main_rows <- data.frame(
      SampleN       = as.character(fs$condition),
      ParamFamily   = as.character(fs$family),
      ParamCount    = fs$k,
      MeanReference = mean_ref,
      MeanBias      = fmt(fs$mean_bias, 4),
      MeanAbsRelB   = fmt(fs$mean_abs_relbias, 2),
      MeanEmpSE     = fmt(fs$mean_emp_se, 4),
      MeanRMSE      = fmt(fs$mean_rmse, 4),
      MeanCoverage  = fmt(fs$mean_coverage, 1),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    
    df_out <- rbind(ref_rows, main_rows)
    
    setNames(df_out, c(
      "Sample (n)",
      "Parameter Family",
      "No. Params",
      "Mean |Reference|",
      "Mean Bias",
      "|Rel. Bias| (%)",
      "Emp. SE",
      "RMSE",
      "Coverage (%)"
    ))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m", width = "100%")
  
  # ---------------------------------------------------------------------------
  # CIKTI: Mutlak uyum indeksleri grafigi (RMSEA + SRMR)
  # ---------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # CIKTI: Mutlak uyum indeksleri (RMSEA + SRMR) — reactive + renderPlot + indirme
  # ---------------------------------------------------------------------------
  fit_abs_gg <- reactive({
    req(sim_state$fit_summ, sim_state$bench_fit)
    fs    <- sim_state$fit_summ
    bf    <- sim_state$bench_fit
    n_full    <- as.numeric(sim_state$bench_n)
    ref_rmsea <- as.numeric(bf["rmsea.scaled"])
    ref_srmr  <- as.numeric(bf["srmr"])
    
    boot_abs <- data.frame(
      condition = rep(as.numeric(fs$condition), 2),
      indeks    = c(rep("RMSEA (scaled)", nrow(fs)), rep("SRMR", nrow(fs))),
      mean_val  = c(fs$mean_rmsea_scaled, fs$mean_srmr),
      sd_val    = c(fs$sd_rmsea_scaled,   fs$sd_srmr),
      is_ref    = FALSE,
      stringsAsFactors = FALSE
    )
    ref_pt_abs <- data.frame(
      condition = rep(n_full, 2),
      indeks    = c("RMSEA (scaled)", "SRMR"),
      mean_val  = c(ref_rmsea, ref_srmr),
      sd_val    = c(0, 0),
      is_ref    = TRUE,
      stringsAsFactors = FALSE
    )
    all_abs <- rbind(boot_abs, ref_pt_abs)
    
    all_breaks <- sort(unique(all_abs$condition))
    all_labels <- ifelse(all_breaks == n_full,
                         paste0("Full\n(N=", n_full, ")"),
                         as.character(all_breaks))
    
    ggplot(all_abs, aes(x = condition, y = mean_val,
                        color = indeks, group = indeks)) +
      geom_ribbon(data = subset(all_abs, !is_ref),
                  aes(ymin = mean_val - sd_val,
                      ymax = mean_val + sd_val,
                      fill = indeks), alpha = 0.12, color = NA) +
      geom_line(aes(linetype = indeks), linewidth = 0.8) +
      geom_point(data = subset(all_abs, !is_ref), size = 2.6) +
      geom_point(data = subset(all_abs,  is_ref), shape = 18, size = 4.5) +
      geom_vline(xintercept = n_full, linetype = "dotted",
                 color = "gray55", linewidth = 0.8) +
      scale_x_continuous(breaks = all_breaks, labels = all_labels) +
      scale_color_manual(values = c("RMSEA (scaled)" = "#c62828", "SRMR" = "#6a1b9a")) +
      scale_fill_manual(values  = c("RMSEA (scaled)" = "#c62828", "SRMR" = "#6a1b9a")) +
      scale_linetype_manual(values = c("RMSEA (scaled)" = "solid", "SRMR" = "dashed")) +
      labs(
        x = "Sample size (n)",
        y = "Fit index value",
        color = "Index", fill = "Index", linetype = "Index"
      ) +
      theme_apa()
  })
  
  output$fit_abs_plot <- renderPlot({ req(fit_abs_gg()); fit_abs_gg() }, bg = "white")
  
  output$dl_fit_abs_png <- downloadHandler(
    filename = function() paste0("absolute_fit_RMSEA_SRMR_", Sys.Date(), ".png"),
    content  = function(file)
      ggsave(file, plot = fit_abs_gg(), device = "png",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  output$dl_fit_abs_pdf <- downloadHandler(
    filename = function() paste0("absolute_fit_RMSEA_SRMR_", Sys.Date(), ".pdf"),
    content  = function(file)
      ggsave(file, plot = fit_abs_gg(), device = "pdf",
             width = 18, height = 12, units = "cm")
  )
  output$dl_fit_abs_tiff <- downloadHandler(
    filename = function() paste0("absolute_fit_RMSEA_SRMR_", Sys.Date(), ".tiff"),
    content  = function(file)
      ggsave(file, plot = fit_abs_gg(), device = "tiff",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  
  # ---------------------------------------------------------------------------
  # CIKTI: Artimsal uyum indeksleri grafigi (CFI + TLI)
  # ---------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # CIKTI: Artimsal uyum indeksleri (CFI + TLI) — reactive + renderPlot + indirme
  # ---------------------------------------------------------------------------
  fit_inc_gg <- reactive({
    req(sim_state$fit_summ, sim_state$bench_fit)
    fs    <- sim_state$fit_summ
    bf    <- sim_state$bench_fit
    n_full  <- as.numeric(sim_state$bench_n)
    ref_cfi <- as.numeric(bf["cfi.scaled"])
    ref_tli <- as.numeric(bf["tli.scaled"])
    
    boot_inc <- data.frame(
      condition = rep(as.numeric(fs$condition), 2),
      indeks    = c(rep("CFI (scaled)", nrow(fs)), rep("TLI (scaled)", nrow(fs))),
      mean_val  = c(fs$mean_cfi_scaled, fs$mean_tli_scaled),
      sd_val    = c(fs$sd_cfi_scaled,   fs$sd_tli_scaled),
      is_ref    = FALSE,
      stringsAsFactors = FALSE
    )
    ref_pt_inc <- data.frame(
      condition = rep(n_full, 2),
      indeks    = c("CFI (scaled)", "TLI (scaled)"),
      mean_val  = c(ref_cfi, ref_tli),
      sd_val    = c(0, 0),
      is_ref    = TRUE,
      stringsAsFactors = FALSE
    )
    all_inc <- rbind(boot_inc, ref_pt_inc)
    
    all_breaks <- sort(unique(all_inc$condition))
    all_labels <- ifelse(all_breaks == n_full,
                         paste0("Full\n(N=", n_full, ")"),
                         as.character(all_breaks))
    
    ggplot(all_inc, aes(x = condition, y = mean_val,
                        color = indeks, group = indeks)) +
      geom_ribbon(data = subset(all_inc, !is_ref),
                  aes(ymin = mean_val - sd_val,
                      ymax = mean_val + sd_val,
                      fill = indeks), alpha = 0.12, color = NA) +
      geom_line(aes(linetype = indeks), linewidth = 0.8) +
      geom_point(data = subset(all_inc, !is_ref), size = 2.6) +
      geom_point(data = subset(all_inc,  is_ref), shape = 18, size = 4.5) +
      geom_vline(xintercept = n_full, linetype = "dotted",
                 color = "gray55", linewidth = 0.8) +
      scale_x_continuous(breaks = all_breaks, labels = all_labels) +
      scale_color_manual(values = c("CFI (scaled)" = "#1565c0", "TLI (scaled)" = "#00695c")) +
      scale_fill_manual(values  = c("CFI (scaled)" = "#1565c0", "TLI (scaled)" = "#00695c")) +
      scale_linetype_manual(values = c("CFI (scaled)" = "solid", "TLI (scaled)" = "dashed")) +
      labs(
        x = "Sample size (n)",
        y = "Fit index value",
        color = "Index", fill = "Index", linetype = "Index"
      ) +
      theme_apa()
  })
  
  output$fit_inc_plot <- renderPlot({ req(fit_inc_gg()); fit_inc_gg() }, bg = "white")
  
  output$dl_fit_inc_png <- downloadHandler(
    filename = function() paste0("incremental_fit_CFI_TLI_", Sys.Date(), ".png"),
    content  = function(file)
      ggsave(file, plot = fit_inc_gg(), device = "png",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  output$dl_fit_inc_pdf <- downloadHandler(
    filename = function() paste0("incremental_fit_CFI_TLI_", Sys.Date(), ".pdf"),
    content  = function(file)
      ggsave(file, plot = fit_inc_gg(), device = "pdf",
             width = 18, height = 12, units = "cm")
  )
  output$dl_fit_inc_tiff <- downloadHandler(
    filename = function() paste0("incremental_fit_CFI_TLI_", Sys.Date(), ".tiff"),
    content  = function(file)
      ggsave(file, plot = fit_inc_gg(), device = "tiff",
             width = 18, height = 12, units = "cm", dpi = 300, bg = "white")
  )
  
  # ---------------------------------------------------------------------------
  # RAW DATA DOWNLOADS
  # ---------------------------------------------------------------------------
  output$dl_raw <- downloadHandler(
    filename = function() paste0("raw_iterations_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$long)
      out <- sim_state$long
      out$bias <- out$est - out$bench
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$dl_param <- downloadHandler(
    filename = function() paste0("parameter_level_summary_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$param_summ)
      write.csv(sim_state$param_summ, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$dl_fam <- downloadHandler(
    filename = function() paste0("family_level_summary_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$fam_summ)
      write.csv(sim_state$fam_summ, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$dl_fit_summ <- downloadHandler(
    filename = function() paste0("fit_index_summary_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$fit_summ)
      write.csv(sim_state$fit_summ, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---------------------------------------------------------------------------
  # TABLO INDIRMELERI
  # ---------------------------------------------------------------------------
  output$dl_bench_csv <- downloadHandler(
    filename = function() paste0("reference_parameters_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$bench_tbl)
      bt <- sim_state$bench_tbl[order(match(sim_state$bench_tbl$family, FAM_ORDER),
                                      sim_state$bench_tbl$key), ]
      out <- data.frame(
        Parameter      = bt$key,
        Family         = bt$family,
        ReferenceValue = round(bt$est, 4),
        stringsAsFactors = FALSE
      )
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dl_health_csv <- downloadHandler(
    filename = function() paste0("condition_health_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$health)
      h <- sim_state$health[order(sim_state$health$condition), ]
      out <- data.frame(
        Sample_n         = h$condition,
        Requested_iter   = h$requested,
        Converged        = h$converged,
        Proper_solution  = h$proper,
        Used             = h$used,
        Empty_category   = h$incomplete,
        Empty_cat_pct    = h$incomp_rate,
        Convergence_pct  = h$conv_rate,
        Proper_soln_pct  = h$prop_rate,
        stringsAsFactors = FALSE
      )
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dl_recovery_csv <- downloadHandler(
    filename = function() paste0("parameter_recovery_summary_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(sim_state$fam_summ, sim_state$bench_tbl)
      fs   <- sim_state$fam_summ
      disp <- c("Factor Loadings", "Thresholds", "Residual Variances", "Factor Covariances")
      fs   <- fs[fs$family %in% disp, ]
      fs   <- fs[order(fs$condition, match(fs$family, FAM_ORDER)), ]
      bt        <- sim_state$bench_tbl[sim_state$bench_tbl$family %in% disp, ]
      bench_ref <- tapply(bt$est, bt$family, function(x) mean(abs(x), na.rm = TRUE))
      bench_k   <- tapply(bt$est, bt$family, length)
      n_label   <- paste0("Full (N=", sim_state$bench_n, ")")
      ref_fams  <- intersect(names(bench_ref), disp)
      ref_fams  <- ref_fams[order(match(ref_fams, FAM_ORDER))]
      ref_rows <- data.frame(
        Sample_n          = rep(n_label, length(ref_fams)),
        Parameter_family  = ref_fams,
        Param_count       = as.numeric(bench_k[ref_fams]),
        Mean_reference    = round(bench_ref[ref_fams], 4),
        Mean_bias         = 0,
        Mean_rel_bias_pct = 0,
        Mean_emp_se       = NA,
        Mean_rmse         = 0,
        Mean_coverage_pct = NA,
        stringsAsFactors = FALSE
      )
      main_rows <- data.frame(
        Sample_n          = as.character(fs$condition),
        Parameter_family  = as.character(fs$family),
        Param_count       = fs$k,
        Mean_reference    = round(bench_ref[fs$family], 4),
        Mean_bias         = round(fs$mean_bias, 4),
        Mean_rel_bias_pct = round(fs$mean_abs_relbias, 2),
        Mean_emp_se       = round(fs$mean_emp_se, 4),
        Mean_rmse         = round(fs$mean_rmse, 4),
        Mean_coverage_pct = round(fs$mean_coverage, 1),
        stringsAsFactors = FALSE
      )
      write.csv(rbind(ref_rows, main_rows), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---------------------------------------------------------------------------
  # HOW TO USE — info message
  # ---------------------------------------------------------------------------
  output$info_message <- renderUI({
    if (is.null(sim_state$long)) {
      div(class = "info-box",
          HTML("<strong>&#x1F4D6; How to Use</strong>"),
          tags$ol(
            tags$li("Upload your CSV data file from the left panel (items as columns)."),
            tags$li(HTML("Drag variables into the <b>F1</b> and <b>F2</b> factor boxes; lavaan syntax is generated automatically.")),
            tags$li(HTML("Select the <b>recovery metric</b> to display in the plot.") ),
            tags$li(HTML("Enter the <b>sample sizes</b> to compare, separated by commas (e.g., 100, 200, 300, 500).")),
            tags$li(HTML("Set the number of <b>replications</b> and the <b>random seed</b> for reproducibility.")),
            tags$li(HTML("Click <b>'Run Simulation'</b>.")),
            tags$li(HTML("Full-sample parameter estimates serve as the <b>population truth (reference values)</b>; each condition is evaluated against these."))
          )
      )
    }
  })

  # ---------------------------------------------------------------------------
  # APA 7 DOCX HELPER
  # ---------------------------------------------------------------------------
  make_apa_docx <- function(df, tbl_num, tbl_title, tbl_note = NULL) {
    ft <- flextable::flextable(df) |>
      flextable::font(fontname = "Times New Roman", part = "all") |>
      flextable::fontsize(size = 12, part = "all") |>
      flextable::align(align = "center", part = "header") |>
      flextable::align(j = 1, align = "left",   part = "body") |>
      flextable::align(j = seq(2, ncol(df)), align = "center", part = "body") |>
      flextable::bold(part = "header") |>
      # APA 7: top border, below-header border, bottom border; no vertical lines
      flextable::border_remove() |>
      flextable::hline_top(border = officer::fp_border(width = 1.5), part = "header") |>
      flextable::hline(border = officer::fp_border(width = 1),   part = "header") |>
      flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "body") |>
      flextable::autofit()

    doc <- officer::read_docx() |>
      # Table number: bold
      officer::body_add_par(
        paste0("Table ", tbl_num),
        style = "Normal"
      ) |>
      officer::body_add_fpar(
        officer::fpar(officer::ftext(paste0("Table ", tbl_num),
                                    prop = officer::fp_text(bold = TRUE, font.size = 12,
                                                            font.family = "Times New Roman"))),
        style = "Normal"
      )

    # Remove the placeholder paragraph added above (body_add_fpar adds fresh)
    # Simpler: use body_add_par with run_bold for table number
    doc <- officer::read_docx()
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(paste0("Table ", tbl_num),
                       prop = officer::fp_text(bold = TRUE, font.size = 12,
                                               font.family = "Times New Roman"))
      )
    )
    # Table title: italics
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(tbl_title,
                       prop = officer::fp_text(italic = TRUE, font.size = 12,
                                               font.family = "Times New Roman"))
      )
    )
    # Table itself
    doc <- flextable::body_add_flextable(doc, ft)
    # Note
    if (!is.null(tbl_note) && nchar(tbl_note) > 0) {
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(paste0("Note. ", tbl_note),
                         prop = officer::fp_text(italic = TRUE, font.size = 10,
                                                 font.family = "Times New Roman"))
        )
      )
    }
    doc
  }

  # ---------------------------------------------------------------------------
  # DOCX DOWNLOADS — APA 7
  # ---------------------------------------------------------------------------

  # Table 1: Reference Model Parameters
  output$dl_bench_docx <- downloadHandler(
    filename = function() paste0("Table1_Reference_Parameters_", Sys.Date(), ".docx"),
    content  = function(file) {
      req(sim_state$bench_tbl)
      bt <- sim_state$bench_tbl[order(match(sim_state$bench_tbl$family, FAM_ORDER),
                                       sim_state$bench_tbl$key), ]
      df <- data.frame(
        "Parameter"        = bt$key,
        "Parameter Family" = bt$family,
        "Reference Value"  = formatC(bt$est, format = "f", digits = 4),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      doc <- make_apa_docx(
        df      = df,
        tbl_num = 1,
        tbl_title = "Reference Model Parameter Estimates (Full Sample)",
        tbl_note  = paste0(
          "Estimator: WLSMV; identification: std.lv = TRUE (factor variances fixed to 1). ",
          "N = ", sim_state$bench_n, ". ",
          "Reference values serve as population truth for parameter recovery evaluation."
        )
      )
      print(doc, target = file)
    }
  )

  # Table 2: Condition Health
  output$dl_health_docx <- downloadHandler(
    filename = function() paste0("Table2_Condition_Health_", Sys.Date(), ".docx"),
    content  = function(file) {
      req(sim_state$health)
      h <- sim_state$health[order(sim_state$health$condition), ]
      df <- data.frame(
        "Sample (n)"          = h$condition,
        "Requested Iter."     = h$requested,
        "Converged"           = h$converged,
        "Proper Solution"     = h$proper,
        "Used"                = h$used,
        "Convergence (%)"     = fmt(h$conv_rate, 1),
        "Proper Solution (%)" = fmt(h$prop_rate, 1),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      doc <- make_apa_docx(
        df      = df,
        tbl_num = 2,
        tbl_title = "Simulation Condition Health: Convergence and Proper-Solution Rates",
        tbl_note  = paste0(
          "Convergence (%) = proportion of replications in which the model converged. ",
          "Proper Solution (%) = proportion of converged replications with no negative ",
          "residual variances or Heywood cases. Used = number of replications retained for recovery analysis."
        )
      )
      print(doc, target = file)
    }
  )

  # Table 3: Parameter Recovery Summary
  output$dl_recovery_docx <- downloadHandler(
    filename = function() paste0("Table3_Parameter_Recovery_", Sys.Date(), ".docx"),
    content  = function(file) {
      req(sim_state$fam_summ, sim_state$bench_tbl)
      fs   <- sim_state$fam_summ
      disp <- c("Factor Loadings", "Thresholds", "Residual Variances", "Factor Covariances")
      fs   <- fs[fs$family %in% disp, ]
      fs   <- fs[order(fs$condition, match(fs$family, FAM_ORDER)), ]
      bt        <- sim_state$bench_tbl[sim_state$bench_tbl$family %in% disp, ]
      bench_ref <- tapply(bt$est, bt$family, function(x) mean(abs(x), na.rm = TRUE))
      bench_k   <- tapply(bt$est, bt$family, length)
      n_label   <- paste0("Full (N=", sim_state$bench_n, ")")
      ref_fams  <- intersect(names(bench_ref), disp)
      ref_fams  <- ref_fams[order(match(ref_fams, FAM_ORDER))]
      ref_rows <- data.frame(
        "Sample (n)"        = rep(n_label, length(ref_fams)),
        "Parameter Family"  = ref_fams,
        "No. Params"        = as.numeric(bench_k[ref_fams]),
        "Mean |Reference|" = fmt(bench_ref[ref_fams], 4),
        "Mean Bias"         = rep("0.0000", length(ref_fams)),
        "|Rel. Bias| (%)"  = rep("0.00",   length(ref_fams)),
        "Emp. SE"           = rep("\u2014",  length(ref_fams)),
        "RMSE"              = rep("0.0000", length(ref_fams)),
        "Coverage (%)"      = rep("(~95)",  length(ref_fams)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      mean_ref <- fmt(bench_ref[fs$family], 4)
      main_rows <- data.frame(
        "Sample (n)"        = as.character(fs$condition),
        "Parameter Family"  = as.character(fs$family),
        "No. Params"        = fs$k,
        "Mean |Reference|" = mean_ref,
        "Mean Bias"         = fmt(fs$mean_bias, 4),
        "|Rel. Bias| (%)"  = fmt(fs$mean_abs_relbias, 2),
        "Emp. SE"           = fmt(fs$mean_emp_se, 4),
        "RMSE"              = fmt(fs$mean_rmse, 4),
        "Coverage (%)"      = fmt(fs$mean_coverage, 1),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      df  <- rbind(ref_rows, main_rows)
      doc <- make_apa_docx(
        df      = df,
        tbl_num = 3,
        tbl_title = "Parameter Recovery Summary by Sample Size and Parameter Family",
        tbl_note  = paste0(
          "Mean |Reference| = mean absolute full-sample estimate. ",
          "Mean Bias = mean(estimate) \u2212 reference. ",
          "|Rel. Bias| (%) = |Mean Bias / Reference| \u00d7 100. ",
          "Emp. SE = empirical standard deviation of replication estimates. ",
          "RMSE = root mean squared error. ",
          "Coverage (%) = percentage of 95% CIs containing the reference value. ",
          "Criterion: |Relative Bias| < 5% negligible, 5\u201310% acceptable (Hoogland & Boomsma, 1998); ",
          "Coverage within 91\u201398% acceptable (Muth\u00e9n & Muth\u00e9n, 2002)."
        )
      )
      print(doc, target = file)
    }
  )

} # end server

# =============================================================================
shinyApp(ui = ui, server = server)