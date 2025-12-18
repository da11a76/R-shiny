library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(moments) 
library(tseries) 
library(nortest)
PRIMARY_COLOR <- "#00A388"
SECONDARY_COLOR <- "#2F4858"
ACCENT_COLOR <- "#FFB600"

light_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = PRIMARY_COLOR,
  secondary = SECONDARY_COLOR,
  success = "#2ECC71",
  info = "#3498DB",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display", wght = "700")
)

# =====================
# 2. USER INTERFACE (UI)
# =====================

ui <- dashboardPage(
  
  # HEADER
  dashboardHeader(
    title = tags$span("🔬 Normality Lab", class = "title-font-header"),
    tags$li(class = "dropdown", style = "padding: 8px; color: #444;", 
    )
  ),
  
  # SIDEBAR
  dashboardSidebar(
    tags$head(
      tags$style(HTML(paste0("
                /* Warna Sidebar Baru */
                .main-sidebar { background-color: ", SECONDARY_COLOR, " !important; } 
                .sidebar-menu li.active a { border-left: 5px solid ", PRIMARY_COLOR, " !important; }
                .logo { background-color: ", SECONDARY_COLOR, " !important; }

                /* Estetika Font dan Box */
                .content-wrapper, .right-side { background: #F0FDF5 !important; }
                .box { border-radius: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.08); }
                .box-header h3.box-title { color: ", PRIMARY_COLOR, "; font-weight: 600; }
                
                .tab-title-accent { color: ", SECONDARY_COLOR, "; font-family: 'Playfair Display'; font-weight: 700; border-bottom: 2px solid ", PRIMARY_COLOR, "; padding-bottom: 5px; margin-bottom: 20px; }

                /* Style untuk Box 'Tentang Aplikasi' */
                #about-box { min-height: 250px; padding: 25px; position: relative; overflow: hidden; }
                #about-box .box-header { margin-bottom: 15px; }
                
                /* Teks lebih besar di About Box */
                .about-text-large { font-size: 1.15em; line-height: 1.6; }
                
                /* Hiasan Latar Belakang Home Lebih Ramai */
                .home-icon-bg { font-size: 150px; color: ", PRIMARY_COLOR, "; opacity: 0.05; position: absolute; top: 10px; right: 10px; transform: rotate(-10deg); z-index: 1; }
                .home-icon-bg-2 { font-size: 80px; color: ", ACCENT_COLOR, "; opacity: 0.08; position: absolute; bottom: 10%; left: 10%; transform: rotate(15deg); z-index: 1; }
                .home-icon-bg-3 { font-size: 90px; color: ", SECONDARY_COLOR, "; opacity: 0.05; position: absolute; top: 10%; left: 40%; z-index: 1; }
            ")))
    ),
    sidebarMenu(id = "tabs",
                menuItem("🏠 Home", tabName = "home"),
                menuItem("📊 Unggah & Deskripsi Data", tabName = "desc"),
                menuItem("📈 Visualisasi Distribusi", tabName = "visual"),
                menuItem("✅ Uji Formal Normalitas", tabName = "formal"), 
                menuItem("🎯 Skewness & Kurtosis", tabName = "skk"),
                menuItem("👥 Analisis Grouping", tabName = "groups"),
                menuItem("⭐ Kesimpulan Final", tabName = "final")
    )
  ),
  dashboardBody(
    
    theme = light_theme, 
    
    tabItems(
      
      # ===================== 
      # 1. HOME & SETUP
      # =====================
      tabItem("home",
              tags$h2("🔬 Normality Lab", class="tab-title-accent"),
              
              fluidRow(
                valueBoxOutput("vb_status_data", width = 4),
                valueBoxOutput("vb_sample_n", width = 4),
                valueBoxOutput("vb_alpha_level", width = 4)
              ),
              
              box(
                id = "about-box",
                width = 12,
                title = "Tentang Aplikasi",
                tags$i(class = "home-icon-bg fas fa-chart-bar"),
                tags$i(class = "home-icon-bg-2 fas fa-flask"),
                tags$i(class = "home-icon-bg-3 fas fa-balance-scale"),
                
                tags$div(class = "about-text-large",
                         tags$p(
                           "Normality Lab adalah aplikasi Shiny untuk mengevaluasi asumsi ",
                           strong("distribusi normal"),
                           " menggunakan pendekatan visual, statistik deskriptif, dan uji formal. Tujuan utama adalah mempermudah identifikasi normalitas data."
                         ),
                         tags$ul(
                           tags$li("Menyesuaikan uji dengan ukuran sampel (n < 30: Shapiro-Wilk; 30 < n <= 200: Jarque-Bera)."),
                           tags$li("Menampilkan interpretasi statistik & visual secara komprehensif."),
                           tags$li("Dirancang untuk analisis akademik & praktis.")
                         )
                ),
                tags$div(style = "height: 40px;")
              ),
              
              fluidRow(
                box(width = 12, actionButton("go_desc", "📂 Mulai Analisis: Unggah & Deskripsi Data", class = "btn-success", width = "100%"))
              )
      ),
      
      
      # =====================
      # 2. DESKRIPSI DATA (Pintasan diatur sebaris)
      # =====================
      tabItem("desc",
              tags$h2("Unggah & Deskripsi Data", class="tab-title-accent"),
              
              fluidRow(
                box(
                  title = tags$span(icon("upload"), " Unggah dan Pilih Variabel"),
                  width = 6, 
                  fileInput("file_data", "Pilih file CSV / Excel", accept = c(".csv", ".xlsx", ".xls")),
                  uiOutput("select_variable"),
                  uiOutput("select_group")
                ),
                
                box(
                  title = tags$span(icon("sliders-h"), " Parameter Analisis"),
                  width = 6, 
                  sliderInput("alpha", "Tingkat Signifikansi (Alpha)", 0.01, 0.1, 0.05, step = 0.005),
                  tags$p("Alpha digunakan sebagai batas keputusan pada uji formal normalitas.")
                )
              ),
              
              hr(),
              
              # PINTASAN DI SINI (sebaris, kiri dan kanan)
              fluidRow(
                box(width = 6, 
                    actionButton("go_visual", "📈 Visualisasi Distribusi", 
                                 class = "btn-success", width = "100%")),
                box(width = 6,
                    actionButton("go_formal", "✅ Uji Formal Normalitas",
                                 class = "btn-success", width = "100%"))
              ),
              
              hr(),
              
              fluidRow(
                box(
                  title=tags$span(icon("table"), " Preview Data (10 Baris Pertama)"),
                  width = 8, 
                  # Fitur scroll horizontal
                  div(style = "overflow-x: auto;", 
                      tableOutput("data_preview")
                  )
                ),
                box(
                  title=tags$span(icon("calculator"), " Statistik Deskriptif"),
                  width = 4,
                  tableOutput("summary_stats"),
                  uiOutput("centrality_note")
                )
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
                    checkboxInput("overlay_normal","Overlay Kurva Normal",FALSE)
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
              ),
              
              fluidRow(
                box(width = 12, tags$p(class="text-muted", "Next With Sidebar"))
              )
      ),
      
      # =====================
      # 4. Sisa Tab 
      # =====================
      tabItem("formal", 
              tags$h2("Uji Formal Normalitas", class="tab-title-accent"),
              uiOutput("sample_info"),
              hr(),
              fluidRow(
                box(title = tags$span(icon("vial"), " Pengaturan Uji"),
                    width = 4, tags$i(class="icon-decor fas fa-flask d1"), 
                    uiOutput("test_selector"), 
                    tags$p("H₀: Data Populasi mengikuti distribusi normal."),
                    tags$p("H₁: Data Populasi tidak mengikuti distribusi normal."),
                    uiOutput("alpha_display")),
                box(title = tags$span(icon("info-circle"), " Ringkasan Data & Rekomendasi"),
                    width = 8, tags$i(class="icon-decor fas fa-lightbulb d2"), uiOutput("test_data_summary"))
              ),
              fluidRow(
                box(title = tags$span(icon("table"), " Hasil Uji Statistik"),
                    width = 12, 
                    div(style="overflow-x: auto;", tableOutput("test_results_table")), 
                    uiOutput("test_interpretation")
              ))
      ),
      
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
              )
      ),
      
      # =====================
      # 8. KESIMPULAN FINAL
      # =====================
      tabItem("final",
              tags$h2("Kesimpulan Akhir Normalitas", class="tab-title-accent"),
              fluidRow(
                box(title=tags$span(icon("comments"), " Kesimpulan"),
                    width = 10, 
                    uiOutput("final_conclusion"))
              ),
              fluidRow(
                box(title=tags$span(icon("download"), " Unduh"),
                    width = 6, 
                    downloadButton("download_report", "Unduh Laporan HTML Sederhana"),
                    downloadButton("download_data", "Unduh Data Mentah (.csv)"))
              )
      )
    )
  )
)

evaluate_normality <- function(x, alpha = 0.05) {
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 3) {
    return(list(
      n = n,
      test_name = NA,
      p_value = NA,
      decision = "DATA_TIDAK_CUKUP"
    ))
  }
  
  if (n>=3 && n < 30) {
    test_name <- "Shapiro-Wilk"
    p_value <- tryCatch(
      shapiro.test(x)$p.value,
      error = function(e) NA
    )
  } else {
    test_name <- "Jarque-Bera"
    p_value <- tryCatch(
      tseries::jarque.test(x)$p.value,
      error = function(e) NA
    )
  }
  
  # Keputusan statistik murni
  if (is.na(p_value)) {
    decision <- "UJI_GAGAL"
  } else if (p_value > alpha) {
    decision <- "GAGAL_TOLAK_H0"
  } else {
    decision <- "TOLAK_H0"
  }
  
  list(
    n = n,
    test_name = test_name,
    p_value = p_value,
    decision = decision
  )
}


server <- function(input, output, session) {
  
  observeEvent(input$go_desc, {
    updateTabItems(session, "tabs", "desc")
  })
  
  observeEvent(input$go_visual, {
    updateTabItems(session, "tabs", "visual")
  })
  
  observeEvent(input$go_formal, {
    updateTabItems(session, "tabs", "formal")
  })
  
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
      simetri_msg <- "<span style='color:#00A388;'>Mean ≈ Median → Distribusi Simetris</span>"
    } else if (mean(x) > median(x)) {
      simetri_msg <- "<span style='color:#E74C3C;'>Mean > Median → Indikasi Skewness Positif</span>"
    } else {
      simetri_msg <- "<span style='color:#E74C3C;'>Mean < Median → Indikasi Skewness Negatif</span>"
    }
    
    # Interpretasi Skewness dan Kurtosis (Rules of Thumb)
    sk_msg <- if(abs(sk)==0){
      "Simetri"
    }  
      else if (abs(sk) < 0.5) {
      "rendah (mendekati simetri)"
    } else if (abs(sk) < 1) {
      "sedang"
    } else {
      "Kuat"
    }
    kt_msg <- if (abs(kt_ex) < 0.5) {
      "Kurtosis mendekati normal (mesokurtik)."
    } else if (kt_ex >= 0.5) {
      "Distribusi leptokurtik (puncak runcing, ekor berat)."
    } else {
      "Distribusi platykurtik (puncak datar, ekor ringan)."
    }
    # Gabungan
    HTML(paste0(
      "<div style='background-color: #F7F7F7; padding: 10px; border-radius: 5px; border-left: 3px solid #2E86C1;'>",
      "<h4>Kesimpulan Deskriptif</h4>",
      "<ul>",
      "<li>", simetri_msg, "</li>",
      "<li>", sk_msg, "</li>",
      "<li>", kt_msg, "</li>",
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
  
  interpret_visual_summary <- function(x) {
    
    if (length(x) < 3 || all(is.na(x))) {
      return(list(
        sk_msg = "Data belum cukup untuk interpretasi bentuk distribusi.",
        kt_msg = "Data belum cukup untuk interpretasi bentuk distribusi."
      ))
    }
    
    sk <- moments::skewness(x, na.rm = TRUE)
    kt <- moments::kurtosis(x, na.rm = TRUE) - 3
    
    sk_msg <- if (!is.na(sk) && abs(sk) <= 0.5) {
      "Distribusi relatif simetris."
    } else if (!is.na(sk) && sk > 0.5) {
      "Distribusi condong ke kanan (positive skew)."
    } else {
      "Distribusi condong ke kiri (negative skew)."
    }
    
    kt_msg <- if (!is.na(kt) && abs(kt) <= 0.5) {
      "Keruncingan mendekati distribusi normal."
    } else if (!is.na(kt) && kt > 0.5) {
      "Distribusi lebih runcing dari normal (leptokurtic)."
    } else {
      "Distribusi lebih datar dari normal (platykurtic)."
    }
    
    list(
      sk = sk,
      kt = kt,
      sk_msg = sk_msg,
      kt_msg = kt_msg
    )
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
  
  output$test_selector <- renderUI({
    n <- sample_size()
    req(n)
    
    if (n < 30) {
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
        recommendation = "Normalitas tidak dapat diuji secara statistik. Min. n=3 untuk Shapiro-Wilk.",
        icon = "fas fa-exclamation-triangle"
      ))
    } else if (N < 30) {
      return(list(
        status = "Sampel Kecil (n < 30)",
        recommendation = "Uji Paling Kuat: Shapiro-Wilk dan Lilliefors. Visualisasikan dengan Q-Q Plot.",
        icon = "fas fa-leaf"
      ))
    } else if (N >= 30 && N <= 100) {
      return(list(
        status = "Sampel Besar (30 < n ≤ 100)",
        recommendation = "Uji yang Direkomendasikan: Shapiro-Wilk (masih baik), Lilliefors/Kolmogorov-Smirnov dan Jarque-Bera (jika n≥20).",
        icon = "fas fa-balance-scale"
      ))
    } else if (N > 100) {
      return(list(
        status = "Sampel sangat besar  (n > 100)",
        recommendation = "Uji yang Direkomendasikan: Jarque-Bera atau Chi-square Goodness-of-Fit. Waspada: Normalitas sering ditolak pada n besar.",
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
      "<p style='margin-bottom: 5px;'>Jumlah Data (n): <span style='font-size:1.2em; font-weight:700;'>", N , "</span></p>",
      "<p style='margin-bottom: 10px;'>Kelompok Sampel: <span style='font-weight:600;'>", rec$status, "</span></p>",
      "Rekomendasi Uji: ", rec$recommendation,
      "</div>"
    ))
  })

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
  
  # =====================
  final_test_result <- reactive({
    x <- selected_data()
    req(x, input$selected_test)
    
    test_res <- run_tests()
    req(test_res)
    
    n <- length(x)
    test_name <- test_res$method
    pval <- test_res$p.value
    
    decision <- ifelse(
      pval > input$alpha,
      "GAGAL_TOLAK_H0",
      "TOLAK_H0"
    )
    
    # ===== VALIDITAS UJI =====
    valid <- TRUE
    warning <- NULL
    
    if (grepl("Jarque", test_name) && n > 200) {
      warning <- "Uji Jarque–Bera sangat sensitif pada ukuran sampel besar. Penyimpangan kecil dari normalitas dapat terdeteksi signifikan."
    }
    
    if (grepl("Kolmogorov", test_name)) {
      warning <- "Uji Kolmogorov–Smirnov kurang ideal jika parameter distribusi (mean & SD) diestimasi dari data."
    }
 
    
    list(
      n = n,
      test_name = test_name,
      p_value = pval,
      decision = decision,
      valid = is.null(warning),
      warning = warning
    )
  })
  
  
  output$final_conclusion <- renderUI({
    x <- selected_data(); req(x)
    res <- final_test_result(); req(res)
    vis <- interpret_visual_summary(x)
    
    normal <- res$decision == "GAGAL_TOLAK_H0"
    color <- ifelse(normal, "#00A388", "#E74C3C")
    
    HTML(paste0(
      "<div style='padding:15px; background:#F9FBFC; border-radius:8px;
                border-left:5px solid ", color, ";'>",
      
      "<h4>📌 Kesimpulan Statistik</h4>",
      "<p><b>Uji yang digunakan:</b> ", res$test_name, "</p>",
      "<p><b>P-value:</b> ", round(res$p_value, 5), "</p>",
      "<p><b>Keputusan:</b> ",
      ifelse(normal,
             "<b style='color:#00A388;'>NORMAL (Gagal Tolak H₀)</b>",
             "<b style='color:#E74C3C;'>TIDAK NORMAL (Tolak H₀)</b>"
      ),
      "</p>",
      
      if (!is.null(res$warning)) {
        paste0(
          "<div style='margin-top:10px; color:#856404;
                    background:#fff3cd; padding:10px;
                    border-radius:5px;'>",
          "⚠️ ", res$warning,
          "</div>"
        )
      } else "",
      
      "<h4 style='margin-top:15px;'>📊 Interpretasi Visual</h4>",
      "<ul>",
      "<li>", vis$sk_msg, "</li>",
      "<li>", vis$kt_msg, "</li>",
      "</ul>",
      
      "<p style='font-size:12px; color:#6c757d;'>
     Catatan: Uji formal sangat sensitif pada ukuran sampel besar.
     Visualisasi digunakan sebagai pendukung interpretasi.
     </p>",
      
      "</div>"
    ))
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("normality_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      
      x <- selected_data()
      if(length(x) < 3) {
        showNotification("Data tidak cukup untuk analisis normalitas.", type = "error")
        return(NULL)
      }
      n <- length(x)
      alpha <- input$alpha
      
      stats <- data.frame(
        Mean = mean(x),
        Median = median(x),
        SD = sd(x),
        Min = min(x),
        Max = max(x)
      )
      
      sh <- li <- jb <- ks <- chisq <- NA
      
      # Small sample (n ≤ 30)
      if (n <= 30) {
        sh <- shapiro.test(x)$p.value
        li <- if (n >= 6) lillie.test(x)$p.value else NA
      }
      
      # Medium sample (30 < n ≤ 100)
      if (n > 30 && n <= 100) {
        jb <- tryCatch(
          tseries::jarque.test(x)$p.value,
          error = function(e) NA
        )
        
        ks <- tryCatch(
          ks.test(x, "pnorm", mean(x), sd(x))$p.value,
          error = function(e) NA
        )
        
        bins <- floor(sqrt(n))
        chisq <- tryCatch({
          observed <- table(cut(x, bins))
          expected <- rep(length(x) / length(observed), length(observed))
          chisq.test(observed, p = expected / sum(expected))$p.value
        }, error = function(e) NA)
      }
      
      # Large sample (n > 100)
      if (n > 100) {
        jb <- tryCatch(
          tseries::jarque.test(x)$p.value,
          error = function(e) NA
        )
        
        bins <- floor(sqrt(n))
        chisq <- tryCatch({
          observed <- table(cut(x, bins))
          expected <- rep(length(x) / length(observed), length(observed))
          chisq.test(observed, p = expected / sum(expected))$p.value
        }, error = function(e) NA)
      }
      fmt_p <- function(p) {
        if (is.na(p)) "NA"
        else round(p, 5)
      }
      
      keputusan <- function(p) {
        if (is.na(p)) "N/A"
        else if (p > alpha) "NORMAL (Gagal Tolak H₀)"
        else "TIDAK NORMAL (Tolak H₀)"
      }
      tests_html <- ""
      
      # n ≤ 30
      if (n <= 30) {
        tests_html <- paste0(
          "<ul>",
          "<li>Shapiro-Wilk p-value: ", fmt_p(sh), " → <b>", keputusan(sh), "</b></li>",
          "<li>Lilliefors p-value: ", fmt_p(li), " → <b>", keputusan(li), "</b></li>",
          "</ul>"
        )
      }
      
      # 30 < n ≤ 100
      if (n > 30 && n <= 100) {
        tests_html <- paste0(
          "<ul>",
          "<li>Jarque-Bera p-value: ", fmt_p(jb), " → <b>", keputusan(jb), "</b></li>",
          "<li>Kolmogorov–Smirnov p-value: ", fmt_p(ks), " → <b>", keputusan(ks), "</b></li>",
          "<li>Chi-square GoF p-value: ", fmt_p(chisq), " → <b>", keputusan(chisq), "</b></li>",
          "</ul>"
        )
      }
      
      # n > 100
      if (n > 100) {
        tests_html <- paste0(
          "<ul>",
          "<li>Jarque-Bera p-value: ", fmt_p(jb), " → <b>", keputusan(jb), "</b></li>",
          "<li>Chi-square GoF p-value: ", fmt_p(chisq), " → <b>", keputusan(chisq), "</b></li>",
          "</ul>"
        )
      }
      
      res <- evaluate_normality(x, alpha)
      
      final_text <- if (res$decision == "GAGAL_TOLAK_H0")
        "Distribusi data mendekati normal."
      else if (res$decision == "TOLAK_H0")
        "Distribusi data menunjukkan penyimpangan dari normalitas."
      else
        "Data tidak cukup untuk dianalisis."
      final_normal <- if(!is.null(res$decision) && res$decision == "GAGAL_TOLAK_H0") TRUE else FALSE
      
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

      <p><b>Sample Size (n):</b> ", n, "</p>
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
    ", tests_html, "


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
}

shinyApp(ui, server)