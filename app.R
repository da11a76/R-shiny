library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(nortest)
library(tidyverse)

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
            tags$b("Analisis Distribusi Normal Komprehensif"))
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
      menuItem("🔍 Detail Deviasi", tabName = "metrics"),
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
              fluidRow(
                box(
                  title = tags$span(icon("vial"), " Pengaturan Uji"),
                  width = 4, tags$i(class="icon-decor fas fa-flask d1"),
                  selectInput("selected_test", "Pilih Uji",
                              choices = c("All", "Shapiro", "Lilliefors", "Jarque-Bera", "Chi-square"),
                              selected = "All"),
                  tags$p("H₀: Data terdistribusi normal. H₁: Data tidak normal."),
                  tags$em(paste0("Batas penolakan (Alpha): ", 0.05))
                ),
                box(
                  title = tags$span(icon("table"), " Hasil Uji Statistik"),
                  width = 8, tags$i(class="icon-decor fas fa-microscope d2"),
                  div(style="overflow-x: auto;", tableOutput("test_results_table")),
                  hr(),
                  uiOutput("test_interpretation")
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
                  tags$b("Jarak dari Normal (0, 3):"),
                  textOutput("sk_kurt_distance")
                )
              )
      ),
      
      # =====================
      # 6. DETAIL DEVIASI
      # =====================
      tabItem("metrics",
              tags$h2("Metrik Deviasi Normalitas", class="tab-title-accent"),
              fluidRow(
                box(title=tags$span(icon("chart-line"), " Max QQ Deviasi"), 
                    width = 4, tags$p("Deviasi Maksimum dari titik Q-Q ke garis normal."),
                    tags$b(textOutput("max_qq_dev"))),
                box(title=tags$span(icon("chart-bar"), " Area Density Diff"), 
                    width = 4, tags$p("Perbedaan Area antara Density Data dan Density Normal."),
                    tags$b(textOutput("area_density_diff"))),
                box(title=tags$span(icon("chart-area"), " ECDF Max Diff"),
                    width = 4, tags$p("Perbedaan Maksimal Fungsi Distribusi Kumulatif Empiris vs Normal."),
                    tags$b(textOutput("ecdf_max_diff")))
              ),
              fluidRow(
                box(title=tags$span(icon("table"), " Tabel Deviasi Q-Q"),
                    width = 6, 
                    div(style="overflow-x: auto;", tableOutput("deviation_table"))),
                box(title=tags$span(icon("chart-bar"), " Strip Deviasi Q-Q"),
                    width = 6, 
                    plotOutput("deviation_strip", height = "350px"))
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
                box(title=tags$span(icon("trophy"), " Skor Normalitas"),
                    width = 4, 
                    tags$p("Rata-rata tertimbang p-value uji utama (dikonversi ke 100)."),
                    uiOutput("normality_gauge")),
                box(title=tags$span(icon("list"), " Rincian Skor"),
                    width = 4, 
                    tableOutput("score_breakdown")),
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
      Statistik = c("Mean", "Median", "SD", "CV", "Skewness", "Kurtosis (Fisher)", "Min", "Max", "IQR"),
      Nilai = c(
        round(mean(x), 6),
        round(median(x), 6),
        round(sd(x), 6),
        round(ifelse(mean(x) != 0, sd(x)/mean(x), NA), 6),
        round(skewness(x), 6),
        round(kurtosis(x) - 3, 6),
        round(min(x), 6),
        round(max(x), 6),
        round(IQR(x), 6)
      )
    )
  }, rownames = FALSE)
  
  output$centrality_note <- renderUI({
    x <- selected_data(); req(x)
    if (abs(mean(x) - median(x)) < 0.1 * sd(x))
      HTML("<div style='color:#00A388; font-weight:600;'>Mean ≈ Median → simetris (Baik)</div>")
    else
      HTML("<div style='color:#E74C3C; font-weight:600;'>Mean ≠ Median → indikasi skewness (Waspada)</div>")
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
  
  # PERBAIKAN Q-Q PLOT: Mengatasi "object 'x' not found"
  output$qq_plot <- renderPlotly({
    x <- selected_data(); 
    req(x) 
    
    # Membuat data frame eksplisit dengan nama kolom 'Sample'
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
      geom_line(aes(x, ECDF, text="ECDF Data"), size = 1, color="#00A388") +
      geom_line(aes(x, Normal, text="Normal CDF"), size = 1, linetype = "dashed", color = "#E74C3C") +
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
  run_tests <- reactive({
    req(input$selected_test)
    x <- selected_data()
    
    sel <- input$selected_test
    res <- list()
    
    if ((sel == "Shapiro" || sel == "All") && length(x) >= 3 && length(x) <= 5000) {
      res$Shapiro <- shapiro.test(x)
    }
    
    if ((sel == "Lilliefors" || sel == "All") && length(x) > 5) {
      res$Lilliefors <- lillie.test(x)
    }
    
    if ((sel == "Jarque-Bera" || sel == "All") && length(x) >= 20) {
      res$`Jarque-Bera` <- jarque.test(x)
    }
    
    if ((sel == "Chi-square" || sel == "All") && length(x) >= 5) {
      res$`Kolmogorov-Smirnov` <- ks.test(scale(x), "pnorm")
    }
    
    res
  })
  
  output$test_results_table <- renderTable({
    tests <- run_tests()
    req(length(tests) > 0)
    
    alpha <- input$alpha
    
    data.frame(
      Test = names(tests),
      Statistic = sapply(tests, function(t) round(unname(t$statistic), 6)),
      p_value = sapply(tests, function(t) round(t$p.value, 6)),
      Decision = ifelse(
        sapply(tests, function(t) t$p.value) < alpha,
        "Reject H0 (NON-NORMAL)",
        "Fail to Reject H0 (NORMAL)"
      ),
      row.names = NULL
    )
  }, striped = TRUE, hover = TRUE)
  
  output$test_interpretation <- renderUI({
    tests <- run_tests()
    req(length(tests) > 0)
    
    alpha <- input$alpha
    n_reject <- sum(sapply(tests, function(t) t$p.value < alpha))
    
    if (n_reject == 0) {
      HTML("<div style='color:#00A388; font-weight:600; background-color: #E6F7E9; padding: 10px; border-radius: 5px;'>
           Semua uji yang dipilih gagal menolak H₀. Data dianggap **NORMAL**.
           </div>")
    } else {
      HTML(paste0(
        "<div style='color:#E74C3C; font-weight:600; background-color: #FEEEEE; padding: 10px; border-radius: 5px;'>",
        n_reject, " dari ", length(tests),
        " uji yang dipilih menolak H₀. Terdapat indikasi data **TIDAK NORMAL**.</div>"
      ))
    }
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
    result$Decision <- ifelse(result$Shapiro_p < input$alpha, "NON-NORMAL", "NORMAL")
    
    return(result)
  }, digits = 6, striped = TRUE, hover = TRUE)
  
  # =====================
  # 10. summary gauge & exports
  # =====================
  output$normality_gauge <- renderUI({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    pvals <- c(
      if(length(x) >= 3) safe_p(shapiro.test, x) else NA, 
      if(length(x) > 5) safe_p(lillie.test, x) else NA,
      if(length(x) >= 20) safe_p(jarque.test, x) else NA
    )
    pvals <- pvals[!is.na(pvals)]
    
    if(length(pvals) == 0) return(tags$p("Data terlalu sedikit untuk diuji."))
    
    score <- round(mean(pmin(pvals * 100, 100)), 1)
    
    color <- ifelse(score > 70, "#2ECC71", ifelse(score > 40, "#FFB600", "#E74C3C")) 
    HTML(paste0("<h2 style='color:", color, "; font-size:3em;'>", score, "/100</h2>"))
  })
  
  output$score_breakdown <- renderTable({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    data.frame(
      Component = c("Shapiro p-value", "Lilliefors p-value", "Jarque-Bera p-value", "ECDF Max Diff (K-S Stat)"),
      Value = c(
        safe_p(shapiro.test, x),
        safe_p(lillie.test, x),
        safe_p(jarque.test, x),
        round(max(abs(ecdf(x)(x) - pnorm(x, mean(x), sd(x)))), 6)
      ),
      stringsAsFactors = FALSE
    )
  }, digits = 6, striped = TRUE, hover = TRUE)
  
  output$final_conclusion <- renderUI({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    sh <- safe_p(shapiro.test, x)
    li <- safe_p(lillie.test, x)
    jb <- safe_p(jarque.test, x)
    
    alpha <- input$alpha
    
    is_normal_count <- sum(c(sh > alpha, li > alpha, jb > alpha), na.rm = TRUE)
    
    if (is_normal_count >= 2) 
      HTML("<div style='color:#00A388; font-weight:600; font-size:1.1em;'>Secara keseluruhan, **NORMAL**. Gunakan statistik parametrik.</div>")
    else
      HTML("<div style='color:#E74C3C; font-weight:600; font-size:1.1em;'>Terdapat indikasi kuat **TIDAK NORMAL**. Pertimbangkan uji non-parametrik.</div>")
  })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("Normality_Report_", Sys.Date(), ".html"),
    content = function(file) {
      writeLines("<h3>Report Sederhana</h3>", file)
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