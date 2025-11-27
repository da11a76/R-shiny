library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)

# ggplot2 and other packages are included here because some UI elements (like renderPlotly) might need them,
# although traditionally they belong more to the server side. They are included in server.R too for safety.
library(ggplot2)
library(nortest)
library(moments)
library(tidyverse)
# readxl package is primarily needed in the server, but for the sake of completeness, 
# the check from the original file is noted here, though not implemented to stop execution here.

# Theme
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2E86C1",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# =====================
# UI Definition
# =====================
ui <- dashboardPage(
  dashboardHeader(title = "Normality Lab"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Deskripsi Data", tabName = "desc", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Uji Formal", tabName = "formal", icon = icon("check-circle")),
      menuItem("Deviation Metrics", tabName = "metrics", icon = icon("tachometer-alt")),
      menuItem("Skew-Kurtosis", tabName = "skk", icon = icon("dot-circle")),
      menuItem("Export", tabName = "export", icon = icon("file-export"))
    )
  ),
  dashboardBody(
    # --- BAGIAN INI MENAMBAHKAN BACKGROUND ---
    tags$head(tags$style(HTML('
      /* Perhatikan tanda kutip pembuka di atas (setelah HTML) */
      
      .content-wrapper, .right-side {
        background-image: url("download (1).jpg");
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
        background-position: center;
      }
      
      .box {
        background-color: rgba(255, 255, 255, 0.9) !important; 
        border-top: 3px solid #2E86C1;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }

      section.content-header > h1 {
        color: #fff; 
        text-shadow: 2px 2px 4px #000000;
      }
      
      /* Perhatikan tanda kutip penutup di bawah ini (sebelum tutup kurung) */
    '))),
    theme = theme,
    tabItems(
      
      # HOME: upload / manual input
      tabItem(tabName = "home",
              fluidRow(
                box(width = 6, title = "Upload / Input Data", solidHeader = TRUE,
                    fileInput("file_data", "Upload file (CSV / XLSX / XLS)",
                              accept = c(".csv", ".xlsx", ".xls")),
                    uiOutput("select_variable"),
                    uiOutput("select_group")
                ),
                box(width = 6, title = "Instruksi", solidHeader = TRUE,
                    h4("Langkah Analisis"),
                    tags$ol(
                      tags$li("Upload file CSV atau Excel (.xlsx/.xls)"),
                      tags$li("Pilih variabel numerik yang ingin dianalisis."),
                      tags$li("Buka tab Visualisasi / Uji Formal / Metrics untuk hasil.")
                    ),
                    hr(),
                    p("Aplikasi TIDAK akan menjalankan analisis sampai variabel dipilih.")
                )
              )
      ),
      
      # DESKRIPSI
      tabItem(tabName = "desc",
              conditionalPanel("input.var != null",
                               fluidRow(
                                 box(width = 8, title = "Preview Data", solidHeader = TRUE,
                                     tableOutput("data_preview")
                                 ),
                                 box(width = 4, title = "Ringkasan Statistik", solidHeader = TRUE,
                                     tableOutput("summary_stats"),
                                     uiOutput("centrality_note")
                                 )
                               )
              ),
              conditionalPanel("input.var == null",
                               h3("⚠ Pilih variabel terlebih dahulu di menu Home."))
      ),
      
      # VISUALISASI
      tabItem(tabName = "visual",
              conditionalPanel("input.var != null",
                               fluidRow(
                                 box(width = 4, title = "Opsi Visual", solidHeader = TRUE,
                                     checkboxInput("show_hist", "Histogram", TRUE),
                                     checkboxInput("show_density", "Density plot", TRUE),
                                     checkboxInput("show_qq", "Q–Q plot", TRUE),
                                     checkboxInput("show_ecdf", "ECDF vs Normal", FALSE),
                                     checkboxInput("overlay_normal", "Overlay kurva normal", TRUE),
                                     sliderInput("bins", "Jumlah bins", min = 5, max = 80, value = 25)
                                 ),
                                 box(width = 8, title = "Plots", solidHeader = TRUE,
                                     conditionalPanel("input.show_hist == true", plotlyOutput("hist_plot")),
                                     conditionalPanel("input.show_density == true", plotlyOutput("density_plot")),
                                     conditionalPanel("input.show_qq == true", plotlyOutput("qq_plot")),
                                     conditionalPanel("input.show_ecdf == true", plotlyOutput("ecdf_plot"))
                                 )
                               )
              ),
              conditionalPanel("input.var == null",
                               h3("⚠ Pilih variabel terlebih dahulu di menu Home."))
      ),
      
      # UJI FORMAL
      tabItem(tabName = "formal",
              conditionalPanel("input.var != null",
                               fluidRow(
                                 box(width = 6, title = "Pengaturan Uji", solidHeader = TRUE,
                                     radioButtons("selected_test", "Pilih uji formal (atau All)",
                                                  choices = c("Shapiro", "Lilliefors", "Jarque-Bera", "Chi-square", "All"),
                                                  selected = "All"),
                                     numericInput("alpha", "Significance level (α)", 0.05, min = 0.001, max = 0.2, step = 0.005)
                                 ),
                                 box(width = 6, title = "Hasil Uji Normalitas", solidHeader = TRUE,
                                     tableOutput("test_results_table"),
                                     uiOutput("test_interpretation")
                                 )
                               ),
                               fluidRow(
                                 box(width = 12, title = "Output Lengkap (raw)", solidHeader = TRUE,
                                     verbatimTextOutput("raw_test_output")
                                 )
                               )
              ),
              conditionalPanel("input.var == null",
                               h3("⚠ Pilih variabel terlebih dahulu di menu Home."))
      ),
      
      # METRICS
      tabItem(tabName = "metrics",
              conditionalPanel("input.var != null",
                               box(width = 12, title = "Deviation Metrics", solidHeader = TRUE,
                                   fluidRow(
                                     column(3, wellPanel(h5("Max Q–Q deviation"), textOutput("max_qq_dev"))),
                                     column(3, wellPanel(h5("Area |f - f_normal|"), textOutput("area_density_diff"))),
                                     column(3, wellPanel(h5("Max |ECDF - CDF|"), textOutput("ecdf_max_diff"))),
                                     column(3, wellPanel(h5("Skew-Kurt distance"), textOutput("sk_kurt_distance")))
                                   ),
                                   hr(),
                                   h4("Breakdown per Kuantil"),
                                   tableOutput("deviation_table"),
                                   br(),
                                   plotOutput("deviation_strip", height = "80px")
                               )
              ),
              conditionalPanel("input.var == null",
                               h3("⚠ Pilih variabel terlebih dahulu di menu Home."))
      ),
      
      # SK-KURT
      tabItem(tabName = "skk",
              conditionalPanel("input.var != null",
                               box(width = 12, title = "Skewness & Kurtosis", solidHeader = TRUE,
                                   plotlyOutput("sk_kurt_plot", height = "420px"),
                                   uiOutput("skk_notes"),
                                   hr(),
                                   h4("Normality Gauge"),
                                   uiOutput("normality_gauge"),
                                   tableOutput("score_breakdown"),
                                   uiOutput("final_conclusion")
                               )
              ),
              conditionalPanel("input.var == null",
                               h3("⚠ Pilih variabel terlebih dahulu di menu Home."))
      ),
      
      # EXPORT
      tabItem(tabName = "export",
              box(width = 6, title = "Export", solidHeader = TRUE,
                  downloadButton("download_data", "Download Data Summary (.csv)"),
                  br(), br(),
                  downloadButton("download_report", "Download Report (HTML)")
              ),
              box(width = 6, title = "Session Log", solidHeader = TRUE,
                  verbatimTextOutput("session_log")
              )
      )
    )
  )
)