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
    # Memastikan input test selector ada
    req(input$selected_test) 
    x <- selected_data(); req(x)
    
    sel <- input$selected_test
    res <- list()
    N <- length(x)
    
    # Shapiro-Wilk: 3 <= N <= 5000
    if ((sel == "Shapiro" || sel == "All") && N >= 3 && N <= 5000) {
      res$Shapiro <- shapiro.test(x)
    }
    
    # Lilliefors (Uji KS yang disesuaikan untuk estimasi parameter dari sampel)
    if ((sel == "Lilliefors" || sel == "All") && N > 5) {
      res$Lilliefors <- lillie.test(x)
    }
    
    # Jarque-Bera (Baik untuk N >= 20)
    if ((sel == "Jarque-Bera" || sel == "All") && N >= 20) {
      # Pastikan paket 'moments' dimuat. jarque.test adalah alias untuk jarque.bera.test
      res$`Jarque-Bera` <- jarque.test(x)
    }
    
    # Kolmogorov-Smirnov (Menggunakan mean dan SD sampel)
    if ((sel == "Kolmogorov-Smirnov" || sel == "All") && N >= 5) {
      res$`Kolmogorov-Smirnov` <- ks.test(x, "pnorm", mean(x), sd(x))
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
      # Logika Konsisten: Tolak H0 (NON-NORMAL) jika p-value < alpha
      Decision = ifelse(
        sapply(tests, function(t) t$p.value) < alpha,
        "TOLAK H0 (TIDAK NORMAL)",
        "GAGAL TOLAK H0 (NORMAL)"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  
  output$test_interpretation <- renderUI({
    tests <- run_tests()
    req(length(tests) > 0)
    
    alpha <- input$alpha
    n_total <- length(tests)
    n_reject <- sum(sapply(tests, function(t) t$p.value < alpha))
    
    # Pesan Uji Formal harus paling tegas
    if (n_reject == 0) {
      HTML(paste0("<div style='color:#00A388; font-weight:700; background-color: #E6F7E9; padding: 10px; border-radius: 5px; border: 1px solid #00A388;'>
      <i class='fa fa-check-circle'></i> Kesimpulan Uji Formal: Semua (", n_total, ") uji gagal menolak H₀. Data **KOMPATIBEL DENGAN NORMALITAS**.
      </div>"))
    } else if (n_reject == n_total) {
      HTML(paste0("<div style='color:#E74C3C; font-weight:700; background-color: #FEEEEE; padding: 10px; border-radius: 5px; border: 1px solid #E74C3C;'>
      <i class='fa fa-times-circle'></i> Kesimpulan Uji Formal: Semua (", n_total, ") uji menolak H₀. Data **SANGAT TIDAK NORMAL**.
      </div>"))
    } else {
      HTML(paste0("<div style='color:#F39C12; font-weight:700; background-color: #FFF8E1; padding: 10px; border-radius: 5px; border: 1px solid #F39C12;'>
      <i class='fa fa-exclamation-triangle'></i> Kesimpulan Uji Formal: ", n_reject, " dari ", n_total, " uji menolak H₀. Terdapat **INDIKASI KETIDAKNORMALAN** yang signifikan.
      </div>"))
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