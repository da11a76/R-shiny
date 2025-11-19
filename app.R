library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(DT)
# ========= CHIBI PICTURES (Stabil & Tidak Expired) =========
chibi <- list(
  home      = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi1.png",
  deskripsi = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi2.png",
  visual    = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi3.png",
  formal    = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi4.png",
  dev       = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi5.png",
  skew      = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi6.png",
  export    = "https://raw.githubusercontent.com/hmdrandom/chibi-assets/main/chibi7.png"
)

# ========= STYLE =========
custom_css <- "
body {
  background: linear-gradient(45deg,#ffd6e8,#ffecc7,#d8ffec,#d6e5ff);
}
.box {
  border-radius: 15px !important;
}
.chibi-header {
  width: 80px;
  margin-bottom: 10px;
  filter: drop-shadow(0px 3px 5px rgba(0,0,0,0.2));
}
.title-text {
  font-size: 28px;
  font-weight: bold;
  margin-top: -10px;
}
"

# ========= UI =========
ui <- dashboardPage(
  dashboardHeader(title = "Normality Lab"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Deskripsi Data", tabName = "desk", icon = icon("table")),
      menuItem("Visualisasi", tabName = "viz", icon = icon("chart-line")),
      menuItem("Uji Formal", tabName = "formal", icon = icon("check-circle")),
      menuItem("Deviation Metrics", tabName = "dev", icon = icon("bullseye")),
      menuItem("Skew-Kurtosis", tabName = "skew", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML(custom_css)),
    
    tabItems(
      
      # ========= HOME =========
      tabItem("home",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$home, class = "chibi-header"),
                       div(class = "title-text", "Welcome to Normality Lab")
                )
              )
      ),
      
      # ========= DESKRIPSI =========
      tabItem("desk",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$deskripsi, class = "chibi-header"),
                       div(class = "title-text", "Upload Data")
                )
              ),
              fileInput("file", "Upload file CSV / XLSX / XLS"),
              tableOutput("preview")
      ),
      
      # ========= VISUALISASI =========
      tabItem("viz",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$visual, class = "chibi-header"),
                       div(class = "title-text", "Visualisasi Data")
                )
              ),
              uiOutput("col_viz"),
              plotOutput("histogram")
      ),
      
      # ========= UJI FORMAL =========
      tabItem("formal",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$formal, class = "chibi-header"),
                       div(class = "title-text", "Uji Normalitas")
                )
              ),
              uiOutput("col_formal"),
              verbatimTextOutput("formal_test")
      ),
      
      # ========= DEVIATION METRICS =========
      tabItem("dev",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$dev, class = "chibi-header"),
                       div(class = "title-text", "Deviation Metrics")
                )
              ),
              uiOutput("col_dev"),
              verbatimTextOutput("dev_out")
      ),
      
      # ========= SKEW-KURTOSIS =========
      tabItem("skew",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$skew, class = "chibi-header"),
                       div(class = "title-text", "Skewness & Kurtosis")
                )
              ),
              uiOutput("col_skew"),
              verbatimTextOutput("skew_out")
      ),
      
      # ========= EXPORT =========
      tabItem("export",
              fluidRow(
                column(12, align = "center",
                       tags$img(src = chibi$export, class = "chibi-header"),
                       div(class = "title-text", "Export Hasil")
                )
              ),
              downloadButton("download", "Download CSV")
      )
    )
  )
)

# ========= SERVER =========
server <- function(input, output, session) {
  
  # ===== LOAD DATA =====
  data_file <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext %in% c("xlsx", "xls")) {
      read_excel(input$file$datapath)
    } else {
      return(NULL)
    }
  })
  
  # Preview
  output$preview <- renderTable({
    head(data_file(), 10)
  })
  
  # Dynamic selectors
  output$col_viz  <- renderUI({ req(data_file()); selectInput("col_viz_sel", "Pilih Kolom", names(data_file())) })
  output$col_formal <- renderUI({ req(data_file()); selectInput("col_formal_sel", "Pilih Kolom", names(data_file())) })
  output$col_dev <- renderUI({ req(data_file()); selectInput("col_dev_sel", "Pilih Kolom", names(data_file())) })
  output$col_skew <- renderUI({ req(data_file()); selectInput("col_skew_sel", "Pilih Kolom", names(data_file())) })
  
  # ===== HISTOGRAM =====
  output$histogram <- renderPlot({
    req(input$col_viz_sel)
    ggplot(data_file(), aes_string(input$col_viz_sel)) + 
      geom_histogram(bins = 30, fill = "#ffb3d9") +
      theme_minimal()
  })
  
  # ===== FORMAL TEST =====
  output$formal_test <- renderPrint({
    req(input$col_formal_sel)
    x <- data_file()[[input$col_formal_sel]]
    shapiro.test(x)
  })
  
  # ===== DEV METRICS =====
  output$dev_out <- renderPrint({
    req(input$col_dev_sel)
    x <- data_file()[[input$col_dev_sel]]
    mean_dev <- mean(abs(x - mean(x)))
    sd_dev   <- sd(x)
    list(Mean_Deviation = mean_dev, SD = sd_dev)
  })
  
  # ===== SKEW-KURTOSIS =====
  output$skew_out <- renderPrint({
    req(input$col_skew_sel)
    library(e1071)
    x <- data_file()[[input$col_skew_sel]]
    list(Skewness = skewness(x), Kurtosis = kurtosis(x))
  })
  
  # ===== EXPORT =====
  output$download <- downloadHandler(
    filename = function() "export_normality.csv",
    content = function(file) {
      write.csv(data_file(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)