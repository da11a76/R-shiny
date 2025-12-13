library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(moments)
library(nortest)
library(tidyverse)
library(readxl) 

# =====================
# SERVER Function
# =====================
server <- function(input, output, session) {
  
  # 1. LOAD DATA (no builtin datasets)
  raw_data <- reactive({
    
    # file upload support: csv or excel
    if (!is.null(input$file_data)) {
      ext <- tools::file_ext(input$file_data$name)
      df <- tryCatch({
        if (tolower(ext) == "csv") {
          read.csv(input$file_data$datapath, stringsAsFactors = FALSE)
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
          readxl::read_excel(input$file_data$datapath)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      if (!is.null(df)) return(as.data.frame(df))
    }
    
    # if nothing provided yet, return NULL (no default dataset)
    return(NULL)
  })
  
  # 2. UI selectors (reactive to uploaded data)
  output$select_variable <- renderUI({
    df <- raw_data()
    if (is.null(df)) {
      tagList(
        tags$div(style = "color:#777;", "Belum ada data — unggah file terlebih dahulu.")
      )
    } else {
      nums <- names(df)[sapply(df, is.numeric)]
      if (length(nums) == 0) {
        tags$em("Tidak ada variabel numerik di dataset.")
      } else {
        selectInput("var", "Pilih variabel numerik", nums)
      }
    }
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
    req(length(x) > 0)
    x
  })
  
  # 4. summary outputs
  output$data_preview <- renderTable({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    head(df, 10)
  })
  
  output$summary_stats <- renderTable({
    x <- selected_data()
    data.frame(
      Mean = round(mean(x), 6),
      Median = round(median(x), 6),
      SD = round(sd(x), 6),
      CV = ifelse(mean(x) != 0, round(sd(x)/mean(x), 6), NA),
      Skewness = round(skewness(x), 6),
      Kurtosis = round(kurtosis(x), 6),
      Min = round(min(x), 6),
      Max = round(max(x), 6),
      IQR = round(IQR(x), 6)
    )
  }, rownames = FALSE)
  
  output$centrality_note <- renderUI({
    x <- selected_data()
    if (abs(mean(x) - median(x)) < 0.1 * sd(x))
      HTML("<div style='color:green;'>Mean ≈ Median → simetris</div>")
    else
      HTML("<div style='color:red;'>Mean ≠ Median → indikasi skewness</div>")
  })
  
  # 5. plots
  output$hist_plot <- renderPlotly({
    x <- selected_data(); req(x)
    p <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = input$bins,
                     fill = "#A7C7E7", color = "#2C3E50", alpha = .8) +
      theme_minimal() +
      labs(title = paste("Histogram -", input$var), x = input$var, y = "Density")
    
    if (isTRUE(input$show_density)) {
      p <- p + geom_density(alpha = .3, inherit.aes = FALSE, aes(x = x))
    }
    if (isTRUE(input$overlay_normal)) {
      p <- p + stat_function(fun = dnorm, args = list(mean(x), sd(x)),
                             color = "#E74C3C", size = 1)
    }
    ggplotly(p)
  })
  
  output$density_plot <- renderPlotly({
    x <- selected_data(); req(x)
    p <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_density(fill = "#2ECC71", alpha = .35) +
      theme_minimal() +
      labs(title = paste("Density -", input$var), x = input$var, y = "Density")
    if (isTRUE(input$overlay_normal)) {
      p <- p + stat_function(fun = dnorm, args = list(mean(x), sd(x)), color = "#E74C3C")
    }
    ggplotly(p)
  })
  
  output$qq_plot <- renderPlotly({
    x <- selected_data(); req(x)
    p <- ggplot(data.frame(x = x), aes(sample = x)) +
      stat_qq(color = "#34495E") +
      stat_qq_line(color = "#E74C3C") +
      theme_minimal() +
      labs(title = paste("Q-Q Plot -", input$var))
    ggplotly(p)
  })
  
  output$ecdf_plot <- renderPlotly({
    x <- selected_data(); req(x)
    ec <- ecdf(x)
    xs <- seq(min(x), max(x), length.out = 200)
    df <- data.frame(x = xs, ECDF = ec(xs), Normal = pnorm(xs, mean(x), sd(x)))
    p <- ggplot(df) +
      geom_line(aes(x, ECDF), size = 1, linetype = "solid") +
      geom_line(aes(x, Normal), size = 1, linetype = "dashed", color = "#E74C3C") +
      theme_minimal() +
      labs(title = paste("ECDF vs Normal -", input$var), x = input$var)
    ggplotly(p)
  })
  
  output$deviation_strip <- renderPlot({
    x <- selected_data(); req(x)
    qq <- qqnorm(x, plot.it = FALSE)
    plot(abs(qq$x - qq$y), type = "h", lwd = 3, col = "#3498DB",
         xlab = "Index", ylab = "|Deviation|", main = "Deviation Strip")
  })
  
  # 6. metrics
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
    round(sqrt(skewness(x)^2 + kurtosis(x)^2), 6)
  })
  
  output$deviation_table <- renderTable({
    x <- selected_data(); req(x)
    qq <- qqnorm(x, plot.it = FALSE)
    data.frame(
      idx = seq_along(qq$x),
      sample_quantile = round(qq$x, 6),
      theoretical = round(qq$y, 6),
      deviation = round(abs(qq$x - qq$y), 6)
    )
  }, rownames = FALSE)
  # 7. tests (REAKTIF SESUAI PILIHAN UJI)
  
  run_tests <- reactive({
    
    req(input$selected_test)   # 🔥 INI KUNCINYA
    x <- selected_data()
    
    sel <- input$selected_test
    res <- list()
    
    if (sel == "Shapiro" || sel == "All") {
      if (length(x) >= 3 && length(x) <= 5000) {
        res$Shapiro <- shapiro.test(x)
      }
    }
    
    if (sel == "Lilliefors" || sel == "All") {
      res$Lilliefors <- lillie.test(x)
    }
    
    if (sel == "Jarque-Bera" || sel == "All") {
      res$`Jarque-Bera` <- jarque.test(x)
    }
    
    if (sel == "Chi-square" || sel == "All") {
      res$`Chi-square` <- ks.test(scale(x), "pnorm")
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
        "Reject H0",
        "Fail to Reject H0"
      ),
      row.names = NULL
    )
  })
  
  output$test_interpretation <- renderUI({
    
    tests <- run_tests()
    req(length(tests) > 0)
    
    alpha <- input$alpha
    n_reject <- sum(sapply(tests, function(t) t$p.value < alpha))
    
    if (n_reject == 0) {
      HTML("<div style='color:green; font-weight:600;'>
         Semua uji yang dipilih gagal menolak H₀ → data konsisten dengan normalitas.
         </div>")
    } else {
      HTML(paste0(
        "<div style='color:red; font-weight:600;'>",
        n_reject, " dari ", length(tests),
        " uji yang dipilih menolak H₀ → indikasi data tidak normal.</div>"
      ))
    }
  })
  
  output$raw_test_output <- renderPrint({
    run_tests()
  })
  
  
  # 8. sk-kurt
  output$sk_kurt_plot <- renderPlotly({
    x <- selected_data(); req(x)
    sk <- skewness(x); kt <- kurtosis(x)
    df <- data.frame(sk = sk, kt = kt)
    p <- ggplot(df, aes(sk, kt)) +
      geom_point(size = 6, color = "#2E86C1") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      theme_minimal() +
      labs(title = "Skewness - Kurtosis", x = "Skewness", y = "Kurtosis")
    ggplotly(p)
  })
  
  output$skk_notes <- renderUI({
    x <- selected_data(); req(x)
    HTML(paste0("Skewness: <b>", round(skewness(x), 4),
                "</b><br>Kurtosis: <b>", round(kurtosis(x), 4), "</b>"))
  })
  
  # 9. group comparison
  output$hist_groups <- renderPlotly({
    req(input$group_var, input$group_var != "None")
    df <- raw_data()
    ggplotly(
      ggplot(df, aes_string(x = input$var, fill = input$group_var)) +
        geom_histogram(position = "identity", alpha = .5, bins = input$bins) +
        theme_minimal()
    )
  })
  
  output$qq_groups <- renderPlotly({
    req(input$group_var, input$group_var != "None")
    df <- raw_data()
    ggplotly(
      ggplot(df, aes(sample = .data[[input$var]])) +
        stat_qq() + stat_qq_line() +
        facet_wrap(as.formula(paste("~", input$group_var))) +
        theme_minimal()
    )
  })
  
  output$group_test_table <- renderTable({
    req(input$group_var, input$group_var != "None")
    df <- raw_data()
    group_data <- split(df[[input$var]], df[[input$group_var]])
    data.frame(
      Group = names(group_data),
      Shapiro_p = sapply(group_data, function(x) {
        xx <- na.omit(as.numeric(x))
        if (length(xx) >= 3) shapiro.test(xx)$p.value else NA
      }),
      stringsAsFactors = FALSE
    )
  }, digits = 6)
  
  
  # 10. summary gauge & exports
  output$normality_gauge <- renderUI({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    pvals <- c(safe_p(shapiro.test, x), safe_p(lillie.test, x), safe_p(jarque.test, x))
    score <- round(mean(pmin(pvals * 100, 100), na.rm = TRUE), 1)
    color <- ifelse(score > 70, "green", ifelse(score > 40, "orange", "red"))
    HTML(paste0("<h2 style='color:", color, ";'>", score, "/100</h2>"))
  })
  
  output$score_breakdown <- renderTable({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    data.frame(
      Component = c("Shapiro p", "Lilliefors p", "Jarque-Bera p", "ECDF max diff"),
      Value = c(
        safe_p(shapiro.test, x),
        safe_p(lillie.test, x),
        safe_p(jarque.test, x),
        round(max(abs(ecdf(x)(x) - pnorm(x, mean(x), sd(x)))), 6)
      ),
      stringsAsFactors = FALSE
    )
  }, digits = 6)
  
  output$final_conclusion <- renderUI({
    x <- selected_data(); req(x)
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    sh <- safe_p(shapiro.test, x)
    li <- safe_p(lillie.test, x)
    jb <- safe_p(jarque.test, x)
    if (!is.na(sh) && !is.na(li) && !is.na(jb) && sh > 0.05 && li > 0.05 && jb > 0.05)
      HTML("<div style='color:green; font-weight:600;'>Secara keseluruhan data tidak menunjukkan penyimpangan signifikan dari normalitas.</div>")
    else
      HTML("<div style='color:red; font-weight:600;'>Terdapat indikasi kuat bahwa data tidak normal.</div>")
  })
  
  output$download_report <- downloadHandler(
    filename = function() "report.html",
    content = function(file) {
      writeLines("<h3>Report sederhana - implementasi cepat</h3>", file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() "data_summary.csv",
    content = function(file) {
      df <- raw_data()
      if (is.null(df)) df <- data.frame()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$session_log <- renderPrint({
    list(
      uploaded = !is.null(input$file_data),
      manual = !is.null(input$manual_data) && nchar(trimws(input$manual_data)) > 0,
      var = if (!is.null(input$var)) input$var else NA,
      bins = if (!is.null(input$bins)) input$bins else NA,
      alpha = if (!is.null(input$alpha)) input$alpha else NA
    )
  })
  
}