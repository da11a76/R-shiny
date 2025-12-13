library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)

# =====================
# THEME
# =====================
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2E86C1",
  base_font = font_google("Inter"),
  heading_font = font_google("Playfair Display")
)

# =====================
# UI
# =====================
ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$span("Normality Lab", class = "title-font")
  ),
  
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
    
    # =====================
    # CSS & FONT
    # =====================
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=Inter:wght@300;400;500&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        body {
          font-family: 'Inter', sans-serif;
        }

        .title-font {
          font-family: 'Playfair Display', serif;
          font-size: 22px;
          font-weight: 700;
        }

        h1, h2, h3, h4, h5 {
          font-family: 'Playfair Display', serif;
        }

        /* BACKGROUND */
        .content-wrapper, .right-side {
          background-image: url('bg.jpg');
          background-size: cover;
          background-position: center;
          background-attachment: fixed;
        }

        /* BOX */
        .box {
          background: rgba(255,255,245,0.93) !important;
          border-radius: 18px;
          border-top: 4px solid #2E86C1;
          box-shadow: 0 12px 25px rgba(0,0,0,0.15);
          animation: fadeUp 0.6s ease;
        }

        @keyframes fadeUp {
          from { opacity: 0; transform: translateY(25px); }
          to { opacity: 1; transform: translateY(0); }
        }

        /* DECOR IMAGES */
        .decor {
          position: fixed;
          opacity: 0.25;
          z-index: 0;
          pointer-events: none;
        }

        .decor1 { bottom: 30px; left: 30px; width: 120px; }
        .decor2 { top: 90px; right: 40px; width: 140px; }
        .decor3 { top: 300px; left: 20px; width: 90px; }
        .decor4 { bottom: 220px; right: 20px; width: 100px; }
        .decor5 { top: 160px; left: 45%; width: 80px; }
        .decor6 { bottom: 60px; right: 45%; width: 110px; }
      "))
    ),
    
    # =====================
    # DECOR ELEMENTS
    # =====================
    tags$img(src = "decor1.png", class = "decor decor1"),
    tags$img(src = "decor2.png", class = "decor decor2"),
    tags$img(src = "decor3.png", class = "decor decor3"),
    tags$img(src = "decor4.png", class = "decor decor4"),
    tags$img(src = "decor5.png", class = "decor decor5"),
    tags$img(src = "decor6.png", class = "decor decor6"),
    
    theme = theme,
    
    tabItems(
      
      # =====================
      # HOME
      # =====================
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 6, title = "Upload / Input Data", solidHeader = TRUE,
            fileInput("file_data", "Upload file (CSV / XLSX / XLS)",
                      accept = c(".csv", ".xlsx", ".xls")),
            uiOutput("select_variable"),
            uiOutput("select_group")
          ),
          box(
            width = 6, title = "Instruksi", solidHeader = TRUE,
            h4("Langkah Analisis"),
            tags$ol(
              tags$li("Upload file CSV atau Excel"),
              tags$li("Pilih variabel numerik"),
              tags$li("Buka tab analisis untuk hasil")
            )
          )
        )
      ),
      
      # =====================
      # DESKRIPSI
      # =====================
      tabItem(
        tabName = "desc",
        conditionalPanel(
          "input.var != null",
          fluidRow(
            box(8, title = "Preview Data", tableOutput("data_preview")),
            box(4, title = "Ringkasan Statistik",
                tableOutput("summary_stats"),
                uiOutput("centrality_note"))
          )
        ),
        conditionalPanel("input.var == null",
                         h3("⚠ Pilih variabel terlebih dahulu di menu Home.")
        )
      ),
      
      # =====================
      # VISUALISASI
      # =====================
      tabItem(
        tabName = "visual",
        conditionalPanel(
          "input.var != null",
          fluidRow(
            box(
              4, title = "Opsi Visual",
              checkboxInput("show_hist", "Histogram", TRUE),
              checkboxInput("show_density", "Density plot", TRUE),
              checkboxInput("show_qq", "Q–Q plot", TRUE),
              checkboxInput("show_ecdf", "ECDF vs Normal", FALSE),
              checkboxInput("overlay_normal", "Overlay kurva normal", TRUE),
              sliderInput("bins", "Jumlah bins", 5, 80, 25)
            ),
            box(
              8, title = "Plots",
              conditionalPanel("input.show_hist", plotlyOutput("hist_plot")),
              conditionalPanel("input.show_density", plotlyOutput("density_plot")),
              conditionalPanel("input.show_qq", plotlyOutput("qq_plot")),
              conditionalPanel("input.show_ecdf", plotlyOutput("ecdf_plot"))
            )
          )
        ),
        conditionalPanel("input.var == null",
                         h3("⚠ Pilih variabel terlebih dahulu di menu Home.")
        )
      ),
      
      # =====================
      # EXPORT
      # =====================
      tabItem(
        tabName = "export",
        box(6, title = "Export",
            downloadButton("download_data", "Download Data (.csv)"),
            br(), br(),
            downloadButton("download_report", "Download Report (HTML)")
        ),
        box(6, title = "Session Log",
            verbatimTextOutput("session_log"))
      )
    )
  )
)
