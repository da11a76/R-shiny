# ui.Rwoh
library(shiny)
library(plotly)
# jika ingin tooltips: library(shinyBS)
# jika ingin valueBox/infoBox (dari shinydashboard): library(shinydashboard)

shinyUI(
  fluidPage(
    # ====== Header ======
    tags$head(
      tags$style(HTML("
        /* kecilkan margin untuk tampilan rapih */
        .small-sub { font-size: 12px; color: #666; margin-top: -8px; }
        .summary-box { background: #f8f9fa; padding: 8px; border-radius: 6px; }
      "))
    ),
    fluidRow(
      column(9,
             titlePanel("Normality Lab — Diagnostik Distribusi")
      ),
      column(3, align = "right",
             # contoh logo (server harus menyediakan file di www/ )
             tags$img(src = "logo.png", height = "40px"),
             tags$div(class = "small-sub", "Upload data → pilih variabel → lihat EDA & uji formal")
      )
    ),
    hr(),
    
    # ====== Layout utama: Sidebar + Main ======
    sidebarLayout(
      sidebarPanel(
        # ---- Kelompok A: Input Data ----
        h4("Input Data"),
        fileInput("file_data", "Upload CSV", accept = ".csv"),
        textAreaInput("manual_data", "Input manual (pisah koma)", placeholder = "1,2,3,...", rows = 3),
        selectInput("builtin_dataset", "Pilih dataset bawaan", choices = c("mtcars","iris"), selected = "mtcars"),
        # dinamis: daftar variabel numerik (dihasilkan oleh server)
        uiOutput("select_variable"),
        # dinamis: pilihan grouping (jika ada)
        uiOutput("select_group"),
        hr(),
        
        # ---- Kelompok B: Visual / EDA Controls ----
        h4("Visual / EDA"),
        checkboxInput("show_hist", "Histogram", value = TRUE),
        checkboxInput("show_density", "Density plot", value = TRUE),
        checkboxInput("show_qq", "Q–Q plot", value = TRUE),
        checkboxInput("show_ecdf", "ECDF vs Normal", value = FALSE),
        sliderInput("bins", "Jumlah bins (histogram)", min = 5, max = 80, value = 25),
        hr(),
        
        # ---- Kelompok C: Diagnostic Controls ----
        h4("Diagnostic"),
        checkboxInput("overlay_normal", "Overlay kurva normal", value = TRUE),
        checkboxInput("show_deviation_shading", "Tampilkan shading deviasi (histogram)", value = TRUE),
        checkboxInput("show_qq_distance", "Tampilkan Q–Q distance bar", value = TRUE),
        checkboxInput("show_sk_kurt_map", "Show Skewness–Kurtosis Map", value = TRUE),
        hr(),
        
        # ---- Kelompok D: Uji Formal ----
        h4("Uji Formal"),
        radioButtons("selected_test", "Pilih uji formal (atau All)",
                     choices = c("Shapiro","Lilliefors","Jarque-Bera","Chi-square","All"),
                     selected = "All"),
        numericInput("alpha", "Significance level (α)", value = 0.05, min = 0.001, max = 0.2, step = 0.005),
        hr(),
        
        # ---- Kelompok E: Output & Export ----
        h4("Output & Export"),
        checkboxInput("show_summary","Tampilkan ringkasan statistik", value = TRUE),
        downloadButton("download_report", "Download Laporan (PDF/HTML)"),
        br(), br(),
        downloadButton("download_plots", "Download Plots (ZIP)"),
        hr(),
        
        # UX tips
        helpText("Tip: gunakan slider bins untuk mengecek sensitivitas histogram terhadap binning"),
        # placeholder untuk future tooltips (attach with shinyBS::bsTooltip di server)
        tags$small("Tooltips: gunakan shinyBS::bsTooltip() untuk membantu pemula.")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "main_tabs",
          
          # ===== Tab 1: Deskripsi Data =====
          tabPanel("Deskripsi Data",
                   br(),
                   fluidRow(
                     column(8,
                            h4("Preview Data"),
                            tableOutput("data_preview")
                     ),
                     column(4,
                            div(class = "summary-box",
                                h5("Ringkasan"),
                                tags$b("Observasi:"), verbatimTextOutput("n_obs"),
                                tags$b("NA count:"), verbatimTextOutput("n_na"),
                                br(),
                                uiOutput("centrality_note") # e.g. "mean ≈ median"
                            )
                     )
                   ),
                   hr(),
                   h4("Ringkasan Statistik"),
                   tableOutput("summary_stats")
          ),
          
          # ===== Tab 2: Visualisasi =====
          tabPanel("Visualisasi",
                   br(),
                   fluidRow(
                     column(8,
                            conditionalPanel("input.show_hist == true",
                                             plotlyOutput("hist_plot", height = "360px")
                            ),
                            conditionalPanel("input.show_density == true",
                                             plotlyOutput("density_plot", height = "240px")
                            )
                     ),
                     column(4,
                            conditionalPanel("input.show_qq == true",
                                             plotlyOutput("qq_plot", height = "300px")
                            ),
                            conditionalPanel("input.show_qq_distance == true",
                                             plotlyOutput("qq_distance_bar", height = "200px")
                            ),
                            conditionalPanel("input.show_ecdf == true",
                                             plotlyOutput("ecdf_plot", height = "240px")
                            ),
                            hr(),
                            uiOutput("visual_notes")
                     )
                   )
          ),
          
          # ===== Tab 3: Deviation Metrics =====
          tabPanel("Deviation Metrics",
                   br(),
                   fluidRow(
                     # small info/value boxes (can be replaced with shinydashboard valueBox)
                     column(3,
                            wellPanel(
                              h5("Max Q–Q deviation"),
                              textOutput("max_qq_dev")
                            )
                     ),
                     column(3,
                            wellPanel(
                              h5("Area |f - f_normal|"),
                              textOutput("area_density_diff")
                            )
                     ),
                     column(3,
                            wellPanel(
                              h5("Max |ECDF - CDF|"),
                              textOutput("ecdf_max_diff")
                            )
                     ),
                     column(3,
                            wellPanel(
                              h5("Skew-Kurt distance"),
                              textOutput("sk_kurt_distance")
                            )
                     )
                   ),
                   hr(),
                   h4("Breakdown per Kuantil"),
                   tableOutput("deviation_table"),
                   br(),
                   plotOutput("deviation_strip", height = "80px")
          ),
          
          # ===== Tab 4: Uji Formal =====
          tabPanel("Uji Formal",
                   br(),
                   h4("Hasil Uji"),
                   tableOutput("test_results_table"),
                   br(),
                   uiOutput("test_interpretation"),
                   br(),
                   verbatimTextOutput("raw_test_output")
          ),
          
          # ===== Tab 5: Skewness–Kurtosis Map =====
          tabPanel("Skew-Kurt Map",
                   br(),
                   plotlyOutput("sk_kurt_plot", height = "450px"),
                   br(),
                   uiOutput("skk_notes")
          ),
          
          # ===== Tab 6: Compare Groups (dinamis) =====
          # tampilkan tab ini hanya bila server men-generate isi di uiOutput("compare_groups_tab")
          uiOutput("compare_groups_tab"),
          
          # ===== Tab 7: Summary & Score =====
          tabPanel("Summary & Score",
                   br(),
                   fluidRow(
                     column(6,
                            h4("Normality Gauge"),
                            htmlOutput("normality_gauge")
                     ),
                     column(6,
                            h4("Score breakdown"),
                            tableOutput("score_breakdown")
                     )
                   ),
                   hr(),
                   h4("Final Conclusion"),
                   htmlOutput("final_conclusion")
          ),
          
          # ===== Tab 8: Export & Logs =====
          tabPanel("Export",
                   br(),
                   downloadButton("download_report", "Download laporan"),
                   br(), br(),
                   downloadButton("download_data", "Download data summary (.csv)"),
                   hr(),
                   h4("Session Log"),
                   verbatimTextOutput("session_log")
          )
        ) # end tabsetPanel
      ) # end mainPanel
    ) # end sidebarLayout
    
    # Footer / bantuan singkat
    ,hr(),
    fluidRow(
      column(12,
             tags$small("Butuh bantuan? Klik tombol 'Help' di pojok kanan atas (atau lihat tab Dokumentasi).")
      )
    )
  ) # end fluidPage
) # end shinyUI
