
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
  
  run_tests <- reactive({
    # Memastikan input test selector ada
    req(input$selected_test) 
    x <- selected_data(); req(x)
    
    sel <- input$selected_test
    res <- list()
    N <- length(x)
    
    # Fungsi pembungkus untuk penanganan error
    safe_test <- function(test_func, x, min_n, max_n = Inf) {
      if (length(x) >= min_n && length(x) <= max_n) {
        tryCatch(test_func(x), error = function(e) list(statistic = NA, p.value = NA, method = paste("Error:", e$message)))
      } else {
        list(statistic = NA, p.value = NA, method = "Tidak Tersedia (N di luar jangkauan)")
      }
    }
    
    # Shapiro-Wilk: 3 <= N <= 5000
    if (sel == "Shapiro" || sel == "All") {
      res$Shapiro <- safe_test(shapiro.test, x, min_n = 3, max_n = 5000)
    }
    
    # Lilliefors (Uji KS yang disesuaikan untuk estimasi parameter dari sampel)
    if (sel == "Lilliefors" || sel == "All") {
      res$Lilliefors <- safe_test(lillie.test, x, min_n = 6) # lillie.test butuh N>=6
    }
    
    # Jarque-Bera (Baik untuk N >= 20)
    if (sel == "Jarque-Bera" || sel == "All") {
      res$`Jarque-Bera` <- safe_test(tseries::jarque.test, x, min_n = 20)
    }
    
    # Kolmogorov-Smirnov (Menggunakan mean dan SD sampel)
    if (sel == "Kolmogorov-Smirnov" || sel == "All") {
      res$`Kolmogorov-Smirnov` <- safe_test(function(data) ks.test(data, "pnorm", mean(data), sd(data)), x, min_n = 5)
    }
    
    # Chi-square Goodness-of-Fit (Baik untuk N besar, N bins harus memenuhi syarat)
    if (sel == "Chi-square" || sel == "All") {
      if (N >= 50) {
        chi_sq_res <- chi_sq_test(x)
        if (!is.null(chi_sq_res)) {
          res$`Chi-square` <- chi_sq_res
        } else {
          res$`Chi-square` <- list(statistic=NA, p.value=NA, method="Tidak bisa dijalankan: Harapan Freq. < 5")
        }
      } else {
        res$`Chi-square` <- list(statistic=NA, p.value=NA, method="Tidak Tersedia (N < 50 disarankan)")
      }
    }
    
    # Hapus yang tidak dipilih atau tidak tersedia
    if (sel != "All") {
      res <- res[names(res) == sel]
    }
    
    res
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
    } else if (N <= 50) {
      return(list(
        status = "Sampel Kecil (N ≤ 50)",
        recommendation = "Uji Paling Kuat: **Shapiro-Wilk** (terbaik untuk N kecil). Visualisasikan dengan Q-Q Plot. Lilliefors juga bisa digunakan jika N > 5.",
        icon = "fas fa-leaf"
      ))
    } else if (N > 50 && N <= 5000) {
      return(list(
        status = "Sampel Menengah (50 < N ≤ 5000)",
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
  
  # PERBAIKAN 3: Tabel Hasil Uji Formal
  output$test_results_table <- renderTable({
    results <- run_tests(); req(results)
    alpha <- input$alpha
    
    rows <- lapply(names(results), function(test_name) {
      res <- results[[test_name]]
      
      p_val <- if (is.null(res$p.value) || is.na(res$p.value)) NA else round(res$p.value, 6)
      stat_val <- if (is.null(res$statistic) || is.na(res$statistic[1])) NA else round(res$statistic[1], 6)
      
      # Tentukan keputusan
      decision <- if (is.na(p_val)) {
        "N/A"
      } else if (p_val > alpha) {
        "<span style='color:#00A388; font-weight:bold;'>NORMAL (Tolak H₀)</span>"
      } else {
        "<span style='color:#E74C3C; font-weight:bold;'>NON-NORMAL (Gagal Tolak H₀)</span>"
      }
      
      # Ambil nama statistik
      stat_name <- names(res$statistic)[1] %||% "Statistic"
      
      data.frame(
        Uji = test_name,
        N_Min = if (test_name == "Shapiro") "N≥3" else if (test_name == "Lilliefors") "N≥6" else if (test_name == "Jarque-Bera") "N≥20" else if (test_name == "Chi-square") "N≥50" else "N/A",
        Statistik_Uji = stat_val,
        P_Value = p_val,
        Keputusan = decision,
        stringsAsFactors = FALSE
      )
    })
    
    df_table <- bind_rows(rows)
    return(df_table)
  }, sanitize.text.function = function(x) x, striped = TRUE, hover = TRUE, align = 'llllr')
  
  # PERBAIKAN 4: Interpretasi Hasil Uji Formal
  output$test_interpretation <- renderUI({
    results <- run_tests(); req(results)
    alpha <- input$alpha
    
    decisions <- sapply(results, function(res) {
      if (is.null(res$p.value) || is.na(res$p.value)) {
        "NA"
      } else {
        ifelse(res$p.value > alpha, "Normal", "Non-Normal")
      }
    })
    
    normal_count <- sum(decisions == "Normal", na.rm = TRUE)
    total_count <- sum(decisions != "NA", na.rm = TRUE)
    
    if (total_count == 0) return(tags$p("Tidak ada uji yang dapat dijalankan dengan data ini."))
    
    majority_decision <- if (normal_count / total_count >= 0.5) {
      paste0("<b style='color:#00A388;'>Mayoritas Uji (", normal_count, "/", total_count, ") menyarankan Distribusi NORMAL.</b>")
    } else {
      paste0("<b style='color:#E74C3C;'>Mayoritas Uji (", total_count - normal_count, "/", total_count, ") menyarankan Distribusi NON-NORMAL.</b>")
    }
    
    HTML(paste0(
      "<div style='border: 1px solid #CCC; padding: 10px; border-radius: 5px; background-color: #FFF;'>",
      "<h4>Ringkasan Keputusan Uji Formal (Alpha = ", alpha, ")</h4>",
      "<p>", majority_decision, "</p>",
      "<p>Interpretasi: Jika P-Value < Alpha (", alpha, "), maka H₀ ditolak (Data TIDAK Normal). Sebaliknya, H₀ diterima (Data Normal).</p>",
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
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    pvals <- c(
      if(length(x) >= 3 && length(x) <= 5000) safe_p(shapiro.test, x) else NA, 
      if(length(x) >= 6) safe_p(lillie.test, x) else NA,
      if(length(x) >= 20) safe_p(tseries::jarque.test, x) else NA
    )
    pvals <- pvals[!is.na(pvals)]
    
    if(length(pvals) == 0) return(tags$p("Data terlalu sedikit untuk diuji."))
    
    # Skor rata-rata p-value (dibatasi maksimal 1.0) dikalikan 100
    score <- round(mean(pmin(pvals, 1.0) * 100), 1)
    
    color <- ifelse(score > 70, "#2ECC71", ifelse(score > 40, "#FFB600", "#E74C3C")) 
    HTML(paste0("<h2 style='color:", color, "; font-size:3em;'>", score, "/100</h2>"))
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
    safe_p <- function(f, x) tryCatch(f(x)$p.value, error = function(e) NA)
    
    sh <- if(length(x) >= 3 && length(x) <= 5000) safe_p(shapiro.test, x) else NA
    li <- if(length(x) >= 6) safe_p(lillie.test, x) else NA
    jb <- if(length(x) >= 20) safe_p(tseries::jarque.test, x) else NA
    
    alpha <- input$alpha
    
    is_normal_count <- sum(c(sh > alpha, li > alpha, jb > alpha), na.rm = TRUE)
    total_tests <- sum(!is.na(c(sh, li, jb)))
    
    if (total_tests == 0) return(tags$p("Tidak ada uji utama yang dapat dijalankan."))
    
    if (is_normal_count / total_tests >= 0.5) 
      HTML("<div style='color:#00A388; font-weight:600; font-size:1.1em;'>Secara keseluruhan, **NORMAL**. Gunakan statistik parametrik (t-test, ANOVA, Regresi Linier) jika asumsi lain terpenuhi.</div>")
    else
      HTML("<div style='color:#E74C3C; font-weight:600; font-size:1.1em;'>Terdapat indikasi kuat **TIDAK NORMAL**. Pertimbangkan transformasi data atau gunakan uji non-parametrik (Mann-Whitney, Kruskal-Wallis).</div>")
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("normality_report_", Sys.Date(), ".html")
    },
    content = function(file) {
    HEAD
      writeLines("DOWNLOAD OK", file)
      
      x <- selected_data()
      n <- length(x)
      
      stats <- data.frame(
        Mean = mean(x),
        Median = median(x),
        SD = sd(x),
        Min = min(x),
        Max = max(x)
      )
      
      sh <- shapiro.test(x)$p.value
      li <- lillie.test(x)$p.value
      jb <- jarque.bera.test(x)$p.value
      
      writeLines(c(
        "<h2>Normality Analysis Report</h2>",
        paste("<p><b>Sample size:</b>", n, "</p>"),
        
        "<h3>Descriptive Statistics</h3>",
        paste("<p>Mean:", round(stats$Mean,2), 
              "| SD:", round(stats$SD,2), "</p>"),
        
        "<h3>Normality Tests (α = 0.05)</h3>",
        "<ul>",
        paste("<li>Shapiro-Wilk p-value:", round(sh,4), "</li>"),
        paste("<li>Lilliefors p-value:", round(li,4), "</li>"),
        paste("<li>Jarque-Bera p-value:", round(jb,4), "</li>"),
        "</ul>",
        
        "<h3>Interpretation</h3>",
        "<p>Results should be interpreted cautiously, considering sample size and test sensitivity.</p>"
      ), file)

      # Logika R Markdown/HTML Report di sini
      writeLines("<h3>Report Sederhana (Implementasi Report Detail Ditinggalkan)</h3>", file)
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