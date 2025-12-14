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