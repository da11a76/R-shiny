library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(moments) # Untuk skewness dan kurtosis
library(nortest) # Untuk lillie.test
library(tseries) # Untuk jarque.test
library(tidyverse)
# Pastikan Anda telah menginstal semua library di atas.

# =====================
# 1. THEME & FONT (Teal & Emas)
# =====================
theme_app <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#00A388",
  secondary = "#2F4858",
  success = "#2ECC71",
  info = "#3498DB",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display")
)

ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(
    title = tags$span("🔬 Normality Lab", class = "title-font-header"),
    tags$li(class = "dropdown", style = "padding: 8px; color: #444;", 
            tags$b("Analisis Distribusi Normal"))
  ),
  
  # SIDEBAR
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
                /* Warna Sidebar Baru */
                .main-sidebar { background-color: #2F4858 !important; } 
                .sidebar-menu li.active a { border-left: 5px solid #00A388 !important; }
                .logo { background-color: #2F4858 !important; }

                /* Estetika Font dan Box */
                .content-wrapper { background: #F0FDF5 !important; }
                .box { border-radius: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.08); }
                .box-header h3.box-title { color: #00A388; font-weight: 600; }
                .icon-decor { font-size: 5em; color: #FFB600; opacity: 0.15; position: absolute; z-index: 0; pointer-events: none; }
                .d1 { bottom: -10px; right: -10px; }
                .d2 { top: -10px; left: -10px; }
                .tab-title-accent { color: #2F4858; font-family: 'Playfair Display'; font-weight: 700; border-bottom: 2px solid #00A388; padding-bottom: 5px; margin-bottom: 20px; }
            "))
    ),
    sidebarMenu(
      menuItem("🏠 Home & Setup", tabName = "home"),
      menuItem("📊 Deskripsi Data", tabName = "desc"),
      menuItem("📈 Visualisasi Distribusi", tabName = "visual"),
      menuItem("✅ Uji Formal Normalitas", tabName = "formal"), 
      menuItem("🎯 Skewness & Kurtosis", tabName = "skk"),
      menuItem("👥 Analisis Grouping", tabName = "groups"),
      menuItem("⭐ Kesimpulan Final", tabName = "final")
    )
  ),
  
  # BODY
  dashboardBody(
    
    tabItems(
      
      # ===================== 
      # 1. HOME & SETUP
      # =====================
      tabItem("home",
              tags$h2("Pengaturan Data", class="tab-title-accent"),
              fluidRow(
                box(
                  title = tags$span(icon("upload"), " Unggah dan Pilih Variabel"),
                  width = 6, tags$i(class="icon-decor fas fa-database d1"),
                  fileInput("file_data", "Pilih file CSV / Excel", accept = c(".csv", ".xlsx", ".xls")),
                  uiOutput("select_variable"),
                  uiOutput("select_group")
                ),
                box(
                  title = tags$span(icon("cogs"), " Opsi Global"),
                  width = 6, tags$i(class="icon-decor fas fa-sliders-h d2"),
                  sliderInput("alpha", "Tingkat Signifikansi (Alpha)", 0.01, 0.1, 0.05, step = 0.005),
                  tags$p("Alpha digunakan sebagai batas keputusan untuk Uji Formal."),
                  hr(),
                  tags$p(icon("info-circle"), " Semua analisis di tab lain akan bergantung pada variabel yang Anda pilih di sini.")
                )
              )
      ),
      
      # =====================
      # 2. DESKRIPSI DATA
      # =====================
      tabItem("desc",
              tags$h2("Deskripsi Statistik Data", class="tab-title-accent"),
              fluidRow(
                box(title=tags$span(icon("table"), " Preview Data (10 Baris Pertama)"),
                    width = 7, tags$i(class="icon-decor fas fa-eye d1"),
                    div(style="overflow-x: auto;", tableOutput("data_preview"))),
                box(title=tags$span(icon("calculator"), " Statistik Kunci"),
                    width = 5, tags$i(class="icon-decor fas fa-chart-line d2"),
                    div(style="font-size:1.1em;", tableOutput("summary_stats")),
                    uiOutput("centrality_note"))
              )
      ),
      
      # =====================
      # 3. VISUALISASI DISTRIBUSI
      # =====================
      tabItem("visual",
              tags$h2("Visualisasi Distribusi", class="tab-title-accent"),
              fluidRow(
                box(title=tags$span(icon("sliders-h"), " Opsi Plot"),
                    width = 12,
                    sliderInput("bins","Jumlah Bins Histogram",10,100,30),
                    checkboxInput("show_density","Tampilkan Density Plot di Histogram",FALSE),
                    checkboxInput("overlay_normal","Overlay Kurva Normal",TRUE)
                )
              ),
              fluidRow(
                box(title=tags$span(icon("chart-bar"), " Histogram"),
                    width = 6, plotlyOutput("hist_plot", height = "350px")),
                box(title=tags$span(icon("chart-line"), " Density Plot"),
                    width = 6, plotlyOutput("density_plot", height = "350px"))
              ),
              fluidRow(
                box(title=tags$span(icon("grip-lines"), " Q-Q Plot"),
                    width = 6, plotlyOutput("qq_plot", height = "350px")),
                box(title=tags$span(icon("chart-area"), " ECDF vs Normal CDF"),
                    width = 6, plotlyOutput("ecdf_plot", height = "350px"))
              )
      ),
      
      # =====================
      # 4. UJI FORMAL NORMALITAS
      # =====================
      tabItem("formal",
              tags$h2("Uji Formal Normalitas", class="tab-title-accent"),
              uiOutput("sample_info"),
              hr(),
              fluidRow(
                box(
                  title = tags$span(icon("vial"), " Pengaturan Uji"),
                  width = 4, tags$i(class="icon-decor fas fa-flask d1"),
                  uiOutput("test_selector")
                  ,
                  tags$p("H₀: Data terdistribusi normal. H₁: Data tidak normal."),
                  
                  # START PERBAIKAN: Mengganti input$alpha dengan uiOutput
                  uiOutput("alpha_display")
                  # END PERBAIKAN
                ),
                
                # BOX BARU: Ringkasan Data & Rekomendasi
                box(
                  title = tags$span(icon("info-circle"), " Ringkasan Data & Rekomendasi"),
                  width = 8, tags$i(class="icon-decor fas fa-lightbulb d2"),
                  uiOutput("test_data_summary")
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("table"), " Hasil Uji Statistik"),
                  width = 12, tags$i(class="icon-decor fas fa-microscope d2"),
                  div(style="overflow-x: auto;", tableOutput("test_results_table")), # OUTPUT BARU
                  hr(),
                  uiOutput("ks_warning"),
                  uiOutput("test_interpretation") # OUTPUT BARU
                )
              ),
              fluidRow(
                box(title = tags$span(icon("terminal"), " Output Mentah (Debug)"),
                    width = 12,
                    verbatimTextOutput("raw_test_output"))
              )
      ),
      
      # =====================
      # 5. SKEWNESS & KURTOSIS
      # =====================
      tabItem("skk",
              tags$h2("Analisis Skewness dan Kurtosis", class="tab-title-accent"),
              fluidRow(
                box(
                  title = tags$span(icon("chart-scatter"), " Plot Skewness vs Kurtosis"),
                  width = 8, tags$i(class="icon-decor fas fa-dot-circle d1"),
                  tags$p("Normalitas didekati saat Skewness ≈ 0 dan Kurtosis ≈ 3 (Kurtosis Fisher ≈ 0)"),
                  plotlyOutput("sk_kurt_plot", height = "400px")
                ),
                box(
                  title = tags$span(icon("tag"), " Nilai Metrik"),
                  width = 4, tags$i(class="icon-decor fas fa-calculator d2"),
                  uiOutput("skk_notes"),
                  hr(),
                  tags$b("Jarak dari Normal (0, 0):"),
                  textOutput("sk_kurt_distance")
                )
              )
      ),
      
      # =====================
      # 7. ANALISIS GROUPING
      # =====================
      tabItem("groups",
              tags$h2("Analisis Normalitas Berdasarkan Grouping", class="tab-title-accent"),
              tags$p("Analisis ini membandingkan distribusi variabel yang dipilih di antara kategori-kategori variabel grouping."),
              fluidRow(
                box(title=tags$span(icon("chart-bar"), " Histogram per Group"),
                    width = 6, plotlyOutput("hist_groups")),
                box(title=tags$span(icon("grip-lines"), " Q-Q Plot per Group"),
                    width = 6, plotlyOutput("qq_groups"))
              ),
              fluidRow(
                box(title=tags$span(icon("table"), " Uji Shapiro-Wilk per Group"),
                    width = 12, tableOutput("group_test_table"))
              )
      ),
      
      # =====================
      # 8. KESIMPULAN FINAL
      # =====================
      tabItem("final",
              tags$h2("Kesimpulan Akhir Normalitas", class="tab-title-accent"),
              fluidRow(
                box(title=tags$span(icon("trophy"), "Normalitas"),
                    width = 4, 
                    uiOutput("normality_gauge")),
                box(title=tags$span(icon("comments"), " Kesimpulan"),
                    width = 4, 
                    uiOutput("final_conclusion"))
              ),
              fluidRow(
                box(title=tags$span(icon("download"), " Unduh"),
                    width = 6, 
                    downloadButton("download_report", "Unduh Laporan HTML Sederhana"),
                    downloadButton("download_data", "Unduh Data Mentah (.csv)")),
                box(title=tags$span(icon("terminal"), " Log Sesi"),
                    width = 6, 
                    verbatimTextOutput("session_log"))
              )
      )
    )
  )
)

evaluate_normality <- function(x, alpha) {
  n <- length(x)
  safe_p <- function(f) tryCatch(f(x)$p.value, error = function(e) NA)
  
  sh <- if(n >= 3 && n <= 5000) safe_p(shapiro.test) else NA
  li <- if(n >= 6) safe_p(lillie.test) else NA
  jb <- if(n >= 20) safe_p(tseries::jarque.test) else NA
  
  main_test <- if (n < 30) sh else if (n < 30) li else jb
  main_name <- if (n < 30) "Shapiro-Wilk" else if (n < 30) "Lilliefors" else "Jarque-Bera"
  
  decision <- if (is.na(main_test)) "INSUFFICIENT"
  else if (main_test > alpha) "APPROX_NORMAL"
  else "NOT_NORMAL"
  
  list(
    n = n,
    sh = sh, li = li, jb = jb,
    main_test = main_test,
    main_name = main_name,
    decision = decision
  )
}

# =========================
# Server
# =========================

# =================================================================
# 2. SERVER: LOGIKA LENGKAP DAN PERBAIKAN Q-Q PLOT
# =================================================================


# =================================================================
# 2. SERVER: LOGIKA LENGKAP DAN PERBAIKAN Q-Q PLOT
# =================================================================

server <- function(input, output, session) {
  
  # 1. LOAD DATA
  raw_data <- reactive({
    req(input$file_data)
    
    ext <- tools::file_ext(input$file_data$name)
    df <- tryCatch({
      if (tolower(ext) == "csv") {
        read.csv(input$file_data$datapath, stringsAsFactors = FALSE)
      } else if (tolower(ext) %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file_data$datapath)
      } else {
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Error membaca file:", e$message), type = "error")
      NULL
    })
    
    req(!is.null(df))
    return(as.data.frame(df))
  })
  
  # 2. UI selectors
  output$select_variable <- renderUI({
    df <- raw_data()
    nums <- names(df)[sapply(df, is.numeric)]
    if (length(nums) == 0) {
      return(tags$em("Tidak ada variabel numerik di dataset."))
    }
    selectInput("var", "Pilih variabel numerik", nums)
  })
  
  output$select_group <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    if (length(cats) == 0) return(NULL)
    selectInput("group_var", "Pilih variabel grouping (opsional)", choices = c("None", cats))
  })
  
  # PERBAIKAN 1: Display Alpha di Tab Formal
  output$alpha_display <- renderUI({
    req(input$alpha)
    tags$em(paste0("Batas penolakan (Alpha): ", input$alpha))
  })
  
  # 3. selected data (vector)
  selected_data <- reactive({
    req(input$var)
    df <- raw_data() 
    req(!is.null(df))
    
    x <- df[[input$var]]
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    
    # PERBAIKAN KOKOH: Memastikan data minimal untuk diplot/uji
    req(length(x) >= 3) 
    
    x
  })
  sample_size <- reactive({
    length(selected_data())
  })
  
  # =====================
  # 4. summary outputs
  # =====================
  output$data_preview <- renderTable({
    df <- raw_data(); req(df)
    head(df, 10)
  })
  
  output$summary_stats <- renderTable({
    x <- selected_data(); req(x)
    data.frame(
      Statistik = c("N (Jumlah Data)", "Mean", "Median", "SD", "CV", "Skewness", "Kurtosis (Excess)", "Min", "Max", "IQR"),
      Nilai = c(
        length(x), # Menambahkan N
        round(mean(x), 6),
        round(median(x), 6),
        round(sd(x), 6),
        round(ifelse(mean(x) != 0, sd(x)/mean(x), NA), 6),
        round(skewness(x), 6),
        round(kurtosis(x) - 3, 6), # Menggunakan Kurtosis Excess (Fisher)
        round(min(x), 6),
        round(max(x), 6),
        round(IQR(x), 6)
      )
    )
  }, rownames = FALSE)
  
  output$centrality_note <- renderUI({
    x <- selected_data(); req(x)
    # Ambang batas 10% dari SD sebagai indikator signifikan perbedaan Mean-Median
    threshold <- 0.1 * sd(x)
    sk <- skewness(x)
    kt_ex <- kurtosis(x) - 3
    
    # Interpretasi Simetri (Mean vs Median)
    if (abs(mean(x) - median(x)) < threshold) {
      simetri_msg <- "<span style='color:#00A388;'>Mean ≈ Median → Distribusi **Simetris**</span>"
    } else if (mean(x) > median(x)) {
      simetri_msg <- "<span style='color:#E74C3C;'>Mean > Median → Indikasi **Skewness Positif**</span>"
    } else {
      simetri_msg <- "<span style='color:#E74C3C;'>Mean < Median → Indikasi **Skewness Negatif**</span>"
    }
    
    # Interpretasi Skewness dan Kurtosis (Rules of Thumb)
    sk_msg <- if (abs(sk) > 0.5) "Skewness (Abs > 0.5) **tinggi**." else "Skewness (Abs ≤ 0.5) **rendah**."
    kt_msg <- if (abs(kt_ex) > 0.5) "Kurtosis (Abs > 0.5) **ekstrem**." else "Kurtosis (Abs ≤ 0.5) **wajar**."
    
    # Gabungan
    HTML(paste0(
      "<div style='background-color: #F7F7F7; padding: 10px; border-radius: 5px; border-left: 3px solid #2E86C1;'>",
      "<h4>Kesimpulan Deskriptif</h4>",
      "<ul>",
      "<li>", simetri_msg, "</li>",
      "<li>**", sk_msg, "**</li>",
      "<li>**", kt_msg, "**</li>",
      "</ul>",
      "</div>"
    ))
  })
  
  # =====================
  # 5. plots 
  # =====================
  output$hist_plot <- renderPlotly({
    x <- selected_data(); req(x, input$bins)
    
    p <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = input$bins,
                     fill = "#00A388", color = "#2F4858", alpha = .7) +
      theme_minimal() +
      labs(title = paste("Histogram & Density dari", input$var), x = input$var, y = "Density")
    
    if (isTRUE(input$show_density)) {
      p <- p + geom_density(alpha = .3, color="#FFB600", linewidth=1.2)
    }
    if (isTRUE(input$overlay_normal)) {
      p <- p + stat_function(fun = dnorm, args = list(mean(x), sd(x)),
                             color = "#E74C3C", linewidth = 1, linetype="dashed")
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$density_plot <- renderPlotly({
    x <- selected_data(); req(x)
    p <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_density(fill = "#00A388", alpha = .35, color="#00A388") +
      theme_minimal() +
      labs(title = paste("Density Plot -", input$var), x = input$var, y = "Density")
    if (isTRUE(input$overlay_normal)) {
      p <- p + stat_function(fun = dnorm, args = list(mean(x), sd(x)), color = "#E74C3C", linewidth=1, linetype="dashed")
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$qq_plot <- renderPlotly({
    x <- selected_data(); 
    req(x) 
    
    df_plot <- data.frame(Sample = x)
    
    p <- ggplot(df_plot, aes(sample = Sample)) +
      stat_qq(color = "#2F4858", size=2) +
      stat_qq_line(color = "#E74C3C", linewidth=1) +
      theme_minimal() +
      labs(title = paste("Q-Q Plot -", input$var), x = "Theoretical Quantiles", y = "Sample Quantiles")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$ecdf_plot <- renderPlotly({
    x <- selected_data(); req(x)
    ec <- ecdf(x)
    xs <- seq(min(x), max(x), length.out = 200)
    df_plot <- data.frame(x = xs, ECDF = ec(xs), Normal = pnorm(xs, mean(x), sd(x)))
    p <- ggplot(df_plot) +
      geom_line(aes(x, ECDF, text=paste("ECDF Data:", round(ECDF,4))), size = 1, color="#00A388") +
      geom_line(aes(x, Normal, text=paste("Normal CDF:", round(Normal,4))), size = 1, linetype = "dashed", color = "#E74C3C") +
      theme_minimal() +
      labs(title = paste("ECDF vs Normal CDF -", input$var), x = input$var, y = "Probability")
    ggplotly(p, tooltip = c("x", "ECDF", "Normal")) %>% config(displayModeBar = FALSE)
  })
  
  # =====================
  # 6. metrics
  # =====================
  output$deviation_strip <- renderPlot({
    x <- selected_data(); req(x)
    qq <- qqnorm(x, plot.it = FALSE)
    par(mar = c(4, 4, 1, 1)) 
    plot(abs(qq$x - qq$y), type = "h", lwd = 3, col = "#3498DB",
         xlab = "Index", ylab = "|Deviation|", main = "")
  })
  
  output$max_qq_dev <- renderText({
    x <- selected_data(); req(x)
    qq <- qqnorm(x, plot.it = FALSE)
    round(max(abs(qq$x - qq$y)), 6)
  })
  
  output$area_density_diff <- renderText({
    x <- selected_data(); req(x)
    dens <- density(x)
    f <- dnorm(dens$x, mean(x), sd(x))
    round(sum(abs(dens$y - f)) * mean(diff(dens$x)), 8)
  })
  
  output$ecdf_max_diff <- renderText({
    x <- selected_data(); req(x)
    ec <- ecdf(x)
    xs <- seq(min(x), max(x), length.out = 200)
    round(max(abs(ec(xs) - pnorm(xs, mean(x), sd(x)))), 8)
  })
  
  output$sk_kurt_distance <- renderText({
    x <- selected_data(); req(x)
    round(sqrt(skewness(x)^2 + (kurtosis(x)-3)^2), 6) 
  })
  
  output$deviation_table <- renderTable({
    x <- selected_data(); req(x)
    qq <- qqnorm(x, plot.it = FALSE)
    data.frame(
      Index = seq_along(qq$x),
      Sample_Quantile = round(qq$x, 6),
      Theoretical = round(qq$y, 6),
      Deviation = round(abs(qq$x - qq$y), 6)
    )
  }, rownames = FALSE, striped = TRUE, hover = TRUE)
  
  # =====================
  # 7. tests
  # =====================
  # Logic Uji Goodness-of-Fit Chi-Square
  group_by_sturges <- function(x) {
    k <- ceiling(1 + log2(length(x)))
    hist(x, breaks = k, plot = FALSE)
  }
  chi_sq_test <- function(x) {
    # Pembagian data ke dalam 5-10 bins (sesuai aturan N/k >= 5)
    N <- length(x)
    k <- max(5, floor(N / 50)) # Aturan sederhana untuk jumlah bins
    k <- min(k, 15) # Batas maksimal bins
    
    # Menghitung frekuensi observasi dan frekuensi harapan
    hist_info <- hist(x, breaks = k, plot = FALSE)
    observed_freq <- hist_info$counts
    
    # Menghitung probabilitas (P(a < X < b)) untuk setiap bin di bawah Normal
    probabilities <- diff(pnorm(hist_info$breaks, mean(x), sd(x)))
    expected_freq <- probabilities * N
    
    # Menggabungkan bins jika frekuensi harapan terlalu kecil (< 5)
    while (any(expected_freq < 5) && length(expected_freq) > 2) {
      idx_small <- which.min(expected_freq)
      if (idx_small == length(expected_freq)) {
        # Gabung bin terakhir dengan bin sebelumnya
        expected_freq[idx_small - 1] <- expected_freq[idx_small - 1] + expected_freq[idx_small]
        observed_freq[idx_small - 1] <- observed_freq[idx_small - 1] + observed_freq[idx_small]
        expected_freq <- expected_freq[-idx_small]
        observed_freq <- observed_freq[-idx_small]
      } else {
        # Gabung bin kecil dengan bin setelahnya
        expected_freq[idx_small + 1] <- expected_freq[idx_small + 1] + expected_freq[idx_small]
        observed_freq[idx_small + 1] <- observed_freq[idx_small + 1] + observed_freq[idx_small]
        expected_freq <- expected_freq[-idx_small]
        observed_freq <- observed_freq[-idx_small]
      }
    }
    
    # Jika masih ada bin < 5 setelah penggabungan, uji tidak dapat diandalkan
    if (any(expected_freq < 5)) {
      return(NULL)
    }
    
    # Derajat kebebasan = jumlah bins - 1 (estimasi mean) - 1 (estimasi sd) - 1
    # df = length(expected_freq) - 3 (jika mean & sd diestimasi)
    df_chi <- length(expected_freq) - 3
    if (df_chi < 1) return(NULL) # Pastikan df > 0
    
    # Hitung statistik Chi-Square
    chi_sq_stat <- sum((observed_freq - expected_freq)^2 / expected_freq)
    p_val <- pchisq(chi_sq_stat, df = df_chi, lower.tail = FALSE)
    
    list(
      statistic = c(`X-squared` = chi_sq_stat),
      p.value = p_val,
      method = "Chi-squared Goodness-of-Fit Test",
      parameter = c(df = df_chi, `N bins` = length(expected_freq))
    )
  }
  
  sample_category <- reactive({
    n <- length(selected_data())
    if (n <= 50) "Sampel Kecil (N ≤ 50)" else "Sampel Besar (N > 50)"
  })
  
  output$sample_info <- renderUI({
    x <- selected_data(); req(x)
    n <- length(x)
    cat <- sample_category()
    
    HTML(paste0(
      "<div style='background:#F7F9FC; padding:12px; border-left:5px solid #3C8DBC;'>",
      "<h4>📊 Ringkasan Data</h4>",
      "<p><b>Jumlah Data (N):</b> ", n, "</p>",
      "<p><b>Kategori Sampel:</b> ", cat, "</p>",
      "</div>"
    ))
  })
  
  output$test_selector <- renderUI({
    n <- sample_size()
    req(n)
    
    if (n <= 50) {
      choices <- c(
        "Shapiro–Wilk" = "shapiro",
        "Lilliefors"   = "lillie"
      )
    } else {
      choices <- c(
        "Jarque–Bera"          = "jb",
        "Kolmogorov–Smirnov"   = "ks",
        "Chi-square GOF"       = "chi"
      )
    }
    
    selectInput(
      "selected_test",
      "Pilih Uji Normalitas",
      choices = choices
    )
  })
  
  safe_jarque <- function(x) {
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    
    # Syarat minimal
    if (length(x) < 20) return(NULL)
    if (sd(x) == 0) return(NULL)
    
    tryCatch(
      tseries::jarque.test(x),
      error = function(e) NULL
    )
  }
  
  
  run_tests <- reactive({
    x <- selected_data()
    req(x, input$selected_test)
    
    test <- input$selected_test
    
    if (test == "shapiro") {
      return(shapiro.test(x))
    }
    
    if (test == "lillie") {
      return(lillie.test(x))
    }
    
    jarque_bera_manual <- function(x) {
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      
      n <- length(x)
      if (n < 20) return(NULL)
      if (sd(x) == 0) return(NULL)
      
      m <- mean(x)
      s <- sd(x)
      
      skewness <- mean((x - m)^3) / s^3
      kurtosis <- mean((x - m)^4) / s^4
      
      JB <- n / 6 * (skewness^2 + (kurtosis - 3)^2 / 4)
      p_value <- 1 - pchisq(JB, df = 2)
      
      structure(
        list(
          statistic = JB,
          p.value = p_value,
          method = "Jarque-Bera Test (Manual)",
          parameter = 2
        ),
        class = "htest"
      )
    }
    
    if (test == "jb") {
      res <- jarque_bera_manual(x)
      req(res)
      return(res)
    }
    
    
    if (test == "ks") {
      return(ks.test(x, "pnorm", mean(x), sd(x)))
    }
    
    if (test == "chi") {
      return(chi_sq_test(x))
    }
    
    NULL
  })
  
  
  # Reactive untuk Rekomendasi Uji
  test_recommendations <- reactive({
    x <- selected_data(); req(x)
    N <- length(x)
    
    if (N < 3) {
      return(list(
        status = "Data Terlalu Sedikit",
        recommendation = "Normalitas tidak dapat diuji secara statistik. Min. N=3 untuk Shapiro-Wilk.",
        icon = "fas fa-exclamation-triangle"
      ))
    } else if (N < 30) {
      return(list(
        status = "Sampel Kecil (N < 30)",
        recommendation = "Uji Paling Kuat: **Shapiro-Wilk** (terbaik untuk N kecil). Visualisasikan dengan Q-Q Plot. Lilliefors juga bisa digunakan jika N > 5.",
        icon = "fas fa-leaf"
      ))
    } else if (N >= 30 && N <= 5000) {
      return(list(
        status = "Sampel Menengah (300 < N ≤ 5000)",
        recommendation = "Uji yang Direkomendasikan: **Shapiro-Wilk** (masih baik), **Lilliefors/Kolmogorov-Smirnov**, dan **Jarque-Bera** (jika N≥20).",
        icon = "fas fa-balance-scale"
      ))
    } else if (N > 5000) {
      return(list(
        status = "Sampel Besar (N > 5000)",
        recommendation = "Uji yang Direkomendasikan: **Kolmogorov-Smirnov**, **Jarque-Bera**, atau **Chi-square Goodness-of-Fit**. Waspada: Normalitas sering ditolak pada N besar.",
        icon = "fas fa-city"
      ))
    }
  })
  
  # Output baru: Ringkasan Data dan Rekomendasi Uji
  output$test_data_summary <- renderUI({
    x <- selected_data(); req(x)
    N <- length(x)
    rec <- test_recommendations()
    
    HTML(paste0(
      "<div style='background-color: #ECF0F1; padding: 15px; border-radius: 8px; border-left: 5px solid #00A388;'>",
      "<h4><i class='", rec$icon, "'></i> Status Data & Rekomendasi</h4>",
      "<p style='margin-bottom: 5px;'>**Jumlah Data (N):** <span style='font-size:1.2em; font-weight:700;'>", N, "</span></p>",
      "<p style='margin-bottom: 10px;'>**Kelompok Sampel:** <span style='font-weight:600;'>", rec$status, "</span></p>",
      "**Rekomendasi Uji:** ", rec$recommendation,
      "</div>"
    ))
  })
  output$ks_warning <- renderUI({
    req(input$selected_test)
    
    if (input$selected_test == "ks") {
      div(
        style = "color: #b30000; background-color: #fff3f3; 
               padding: 10px; border-radius: 5px;",
        strong("⚠️ Peringatan Uji Kolmogorov–Smirnov"),
        p("Uji KS sensitif terhadap estimasi parameter dan ukuran sampel."),
        p("Dalam aplikasi ini, uji KS digunakan sebagai uji pendukung dan tidak dijadikan dasar utama penentuan normalitas.")
      )
    }
  })
  
  # PERBAIKAN 3: Tabel Hasil Uji Formal
  output$test_results_table <- renderTable({
    res <- run_tests()
    req(res)
    
    if (!is.null(res$error)) {
      return(data.frame(
        Uji = res$method,
        Statistik = NA,
        P_Value = NA,
        Keputusan = res$error
      ))
    }
    
    keputusan <- ifelse(
      res$p.value > input$alpha,
      "NORMAL (Gagal Tolak H₀)",
      "TIDAK NORMAL (Tolak H₀)"
    )
    
    data.frame(
      Uji = res$method,
      Statistik = round(as.numeric(res$statistic)[1], 6),
      P_Value = formatC(res$p.value, format = "f", digits = 5),
      Keputusan = ifelse(
        res$p.value > input$alpha,
        "NORMAL (Gagal Tolak H₀)",
        "TIDAK NORMAL (Tolak H₀)"
      )
    )
  }, rownames = FALSE)
  
  
  # PERBAIKAN 4: Interpretasi Hasil Uji Formal
  output$test_interpretation <- renderUI({
    res <- run_tests()
    req(res)
    
    alpha <- input$alpha
    normal <- res$p.value > alpha
    color <- ifelse(normal, "#00A388", "#E74C3C")
    
    HTML(paste0(
      "<div style='border-left:5px solid ", color, "; padding:12px;'>",
      "<h4>Kesimpulan Uji</h4>",
      "<p><b>", res$method, "</b></p>",
      "<p>Keputusan: <b style='color:", color, ";'>",
      ifelse(normal, "NORMAL", "TIDAK NORMAL"),
      "</b></p>",
      "</div>"
    ))
  })
  
  
  
  output$raw_test_output <- renderPrint({ run_tests() })
  
  # =====================
  # 8. sk-kurt
  # =====================
  output$sk_kurt_plot <- renderPlotly({
    x <- selected_data(); req(x)
    sk <- skewness(x); kt_fish <- kurtosis(x) - 3
    df_skk <- data.frame(sk = sk, kt = kt_fish)
    
    p <- ggplot(df_skk, aes(x=sk, y=kt)) +
      geom_point(aes(text = paste("Skew:", round(sk, 3), "<br>Kurtosis:", round(kt, 3))), 
                 size = 6, color = "#E74C3C") +
      geom_point(x = 0, y = 0, color = "#00A388", size = 8, shape = 4, stroke = 2) + 
      annotate("text", x = 0, y = 0.5, label = "Normal", color = "#00A388", size = 4, fontface = "bold") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#AAB7B8") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#AAB7B8") +
      labs(title = "Plot Skewness vs Kurtosis (Kurtosis Fisher)", x = "Skewness", y = "Kurtosis Fisher (Excess)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$skk_notes <- renderUI({
    x <- selected_data(); req(x)
    sk <- round(skewness(x), 4)
    kt_fish <- round(kurtosis(x) - 3, 4)
    HTML(paste0("Skewness: <b>", sk, "</b><br>Kurtosis Fisher: <b>", kt_fish, "</b>"))
  })
  
  # =====================
  # 9. group comparison
  # =====================
  output$hist_groups <- renderPlotly({
    req(input$group_var, input$group_var != "None")
    df <- raw_data(); req(df)
    
    p <- ggplot(df, aes_string(x = input$var, fill = input$group_var)) +
      geom_histogram(position = "identity", alpha = .6, bins = input$bins) +
      labs(title=paste("Histogram Grouped by", input$group_var)) +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$qq_groups <- renderPlotly({
    req(input$group_var, input$group_var != "None")
    df <- raw_data(); req(df)
    
    p <- ggplot(df, aes(sample = .data[[input$var]], color = .data[[input$group_var]])) +
      stat_qq() + stat_qq_line() +
      facet_wrap(as.formula(paste("~", input$group_var))) +
      labs(title=paste("Q-Q Plot Grouped by", input$group_var)) +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$group_test_table <- renderTable({
    req(input$group_var, input$group_var != "None")
    df <- raw_data(); req(df)
    
    df[[input$group_var]] <- as.factor(df[[input$group_var]])
    
    group_data <- split(df[[input$var]], df[[input$group_var]])
    
    result <- data.frame(
      Group = names(group_data),
      N = sapply(group_data, function(x) length(na.omit(as.numeric(x)))),
      Shapiro_p = sapply(group_data, function(x) {
        xx <- na.omit(as.numeric(x))
        if (length(xx) >= 3 && length(xx) <= 5000) shapiro.test(xx)$p.value else NA
      }),
      stringsAsFactors = FALSE
    )
    result$Decision <- ifelse(result$N < 3, "N/A", ifelse(result$Shapiro_p < input$alpha, "NON-NORMAL", "NORMAL"))
    
    return(result)
  }, digits = 6, striped = TRUE, hover = TRUE)
  
  # =====================
  # 10. summary gauge & exports
  # =====================
  output$normality_gauge <- renderUI({
    x <- selected_data(); req(x)
    alpha <- input$alpha
    
    res <- evaluate_normality(x, alpha)
    
    if (res$decision == "APPROX_NORMAL") {
      HTML(paste0(
        "<h2 style='color:#2ECC71;'>Distribusi Mendekati Normal</h2>",
        "<p>Uji utama: <b>", res$main_name,
        "</b> (p-value = ", round(res$main_test, 5), ")</p>"
      ))
    } else if (res$decision == "NOT_NORMAL") {
      HTML(paste0(
        "<h2 style='color:#E74C3C;'>Distribusi Tidak Normal</h2>",
        "<p>Uji utama: <b>", res$main_name,
        "</b> (p-value = ", round(res$main_test, 5), ")</p>"
      ))
    } else {
      tags$p("Data tidak cukup untuk evaluasi normalitas.")
    }
  })
  
  output$score_breakdown <- renderTable({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    data.frame(
      Component = c("Shapiro p-value", "Lilliefors p-value", "Jarque-Bera p-value", "ECDF Max Diff (K-S Stat)"),
      Value = c(
        if(length(x) >= 3 && length(x) <= 5000) safe_p(shapiro.test, x) else NA,
        if(length(x) >= 6) safe_p(lillie.test, x) else NA,
        if(length(x) >= 20) safe_p(tseries::jarque.test, x) else NA,
        round(max(abs(ecdf(x)(x) - pnorm(x, mean(x), sd(x)))), 6)
      ),
      stringsAsFactors = FALSE
    )
  }, digits = 6, striped = TRUE, hover = TRUE)
  
  output$final_conclusion <- renderUI({
    x <- selected_data(); req(x)
    res <- evaluate_normality(x, input$alpha)
    
    if (res$decision == "APPROX_NORMAL") {
      HTML("<div style='color:#00A388; font-weight:600; font-size:1.1em;'>
         Secara keseluruhan, distribusi data <b>mendekati normal</b>.
         Statistik parametrik dapat dipertimbangkan dengan verifikasi asumsi lain.
         </div>")
    } else if (res$decision == "NOT_NORMAL") {
      HTML("<div style='color:#E74C3C; font-weight:600; font-size:1.1em;'>
         Distribusi data <b>tidak normal</b>.
         Disarankan transformasi data atau metode non-parametrik.
         </div>")
    } else {
      tags$p("Data tidak cukup untuk kesimpulan normalitas.")
    }
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("normality_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      
      x <- selected_data()
      n <- length(x)
      alpha <- input$alpha
      
      stats <- data.frame(
        Mean = mean(x),
        Median = median(x),
        SD = sd(x),
        Min = min(x),
        Max = max(x)
      )
      
      sh <- if (n >= 3 && n <= 5000) shapiro.test(x)$p.value else NA
      li <- if (n >= 6) lillie.test(x)$p.value else NA
      jb <- if (n >= 20) {
        tryCatch(tseries::jarque.test(x)$p.value, error = function(e) NA)
      } else NA
      
      fmt_p <- function(p) {
        if (is.na(p)) "NA"
        else round(p, 5)
      }
      
      keputusan <- function(p) {
        if (is.na(p)) "N/A"
        else if (p > alpha) "NORMAL (Gagal Tolak H₀)"
        else "TIDAK NORMAL (Tolak H₀)"
      }
      
      
      res <- evaluate_normality(x, alpha)
      
      final_text <- if (res$decision == "APPROX_NORMAL")
        "Distribusi data mendekati normal."
      else if (res$decision == "NOT_NORMAL")
        "Distribusi data menunjukkan penyimpangan dari normalitas."
      else
        "Data tidak cukup untuk dianalisis."
      
      
      html <- paste0(
        "<!DOCTYPE html>
      <html>
      <head>
        <meta charset='UTF-8'>
        <title>Normality Analysis Report</title>
        <style>
          body { font-family: Arial; margin: 40px; }
          h2 { color: #00A388; }
          table { border-collapse: collapse; width: 60%; }
          th, td { border: 1px solid #ccc; padding: 8px; text-align:center; }
          th { background-color: #f2f2f2; }
        </style>
      </head>
      <body>

      <h2>Normality Analysis Report</h2>

      <p><b>Sample Size (N):</b> ", n, "</p>
      <p><b>Alpha:</b> ", alpha, "</p>

      <h3>Descriptive Statistics</h3>
      <table>
        <tr><th>Mean</th><th>Median</th><th>SD</th><th>Min</th><th>Max</th></tr>
        <tr>
          <td>", round(stats$Mean,4), "</td>
          <td>", round(stats$Median,4), "</td>
          <td>", round(stats$SD,4), "</td>
          <td>", round(stats$Min,4), "</td>
          <td>", round(stats$Max,4), "</td>
        </tr>
      </table>

      <h3>Normality Tests</h3>
      <ul>
        <li>Shapiro-Wilk p-value: ", fmt_p(sh), " → <b>", keputusan(sh), "</b></li>
        <li>Lilliefors p-value: ", fmt_p(li), " → <b>", keputusan(li), "</b></li>
        <li>Jarque-Bera p-value: ", fmt_p(jb), " → <b>", keputusan(jb), "</b></li>
      </ul>

      <h3>Final Conclusion</h3>
      <p>",
        if (final_normal)
          "<b style='color:green;'>Data secara umum BERDISTRIBUSI NORMAL.</b>"
        else
          "<b style='color:red;'>Data menunjukkan indikasi TIDAK NORMAL.</b>",
        "</p>

      <hr>
      <p><i>Generated by Normality Lab Shiny App</i></p>

      </body>
      </html>"
      )
      
      writeLines(html, file)
    }
  )
  output$download_data <- downloadHandler(
    filename = function() paste0("data_normality_lab_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- raw_data();
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$session_log <- renderPrint({
    list(
      Uploaded = !is.null(input$file_data),
      Variable_Selected = if (!is.null(input$var)) input$var else "None",
      N_Observations = length(selected_data()),
      Alpha = input$alpha,
      Timestamp = Sys.time()
    )
  })
}

shinyApp(ui, server)