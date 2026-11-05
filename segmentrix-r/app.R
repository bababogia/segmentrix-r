# =====================================================================
# Segmentrix-R  |  app.R
# Customer segmentation, RFM analysis and lifecycle analytics.
# By Bogiatzis Alexandros 01/11/26
# Run it:  open this file in RStudio and click "Run App"
#          (or in the R console:  shiny::runApp())
#
# Shiny automatically sources every file in the R/ folder BEFORE this
# file, so all functions (generate_synthetic_transactions, clean_transactions,
# fit_segments, ...) are available below.


suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)   # dplyr, ggplot2, tidyr, lubridate, ...
  library(data.table)
  library(plotly)
  library(reactable)
  library(cluster)
  library(factoextra)
})

# A fixed colour per segment so every chart uses the same colours
segment_colors <- function(levels_) {
  base <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd",
            "#8c564b", "#17becf", "#7f7f7f", "#bcbd22")
  stats::setNames(base[seq_along(levels_)], levels_)
}

# Colour ramp (green -> yellow -> red) for churn-risk cells
churn_ramp <- grDevices::colorRampPalette(c("#d9f0d3", "#fff7bc", "#f4a582"))(101)
churn_style <- function(value) {
  list(background = churn_ramp[round(max(0, min(1, value)) * 100) + 1])
}

# =====================================================================
# UI  (what the user SEES)
# =====================================================================
ui <- page_navbar(
  title    = "Segmentrix-R",
  theme    = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  # ---- Sidebar: controls shared by all tabs ----------------------------
  sidebar = sidebar(
    width = 320,
    h5("1. Data"),
    radioButtons("source", NULL,
                 c("Synthetic demo data" = "synthetic", "Upload my CSV" = "upload")),
    conditionalPanel(
      "input.source == 'synthetic'",
      numericInput("n_customers", "Number of synthetic customers", 1500,
                   min = 300, max = 10000, step = 100),
      numericInput("seed", "Random seed", 42)
    ),
    conditionalPanel(
      "input.source == 'upload'",
      fileInput("file", "CSV file", accept = ".csv"),
      helpText("Columns: CustomerID, InvoiceDate (YYYY-MM-DD), TransactionID, OrderValue, ProductCategory")
    ),
    hr(),
    h5("2. Clustering"),
    radioButtons("k_mode", "How many clusters (K)?",
                 c("Auto: best silhouette" = "silhouette",
                   "Auto: elbow method"    = "elbow",
                   "Manual"                = "manual")),
    conditionalPanel("input.k_mode == 'manual'",
                     sliderInput("k", "K", min = 2, max = 8, value = 4, step = 1)),
    hr(),
    h5("3. Migration"),
    numericInput("lookback", "Compare with N days ago", 90, min = 30, max = 365, step = 30),
    hr(),
    actionButton("run", "Run analysis", class = "btn-primary w-100", icon = icon("play"))
  ),

  # ---- Tab 1 ------------------------------------------------------------
  nav_panel(
    "Executive Overview", icon = icon("chart-pie"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Total Revenue", textOutput("kpi_revenue"),
                showcase = icon("sack-dollar"), theme = "primary"),
      value_box("Active Customers (last 90 days)", textOutput("kpi_active"),
                showcase = icon("users"), theme = "success"),
      value_box("Top Cohort Avg. Spend", textOutput("kpi_top_value"),
                textOutput("kpi_top_name"),
                showcase = icon("crown"), theme = "warning"),
      value_box("Average Order Value", textOutput("kpi_aov"),
                showcase = icon("receipt"), theme = "info")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Lifetime spend distribution by segment"),
           plotlyOutput("spend_plot", height = "400px")),
      card(full_screen = TRUE,
           card_header("RFM heatmap: average spend by Recency x Frequency score"),
           plotlyOutput("rfm_heatmap", height = "400px"))
    )
  ),

  # ---- Tab 2 ------------------------------------------------------------
  nav_panel(
    "Cluster Deep-Dive", icon = icon("diagram-project"),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE,
           card_header("Customer clusters in PCA space",
                       radioButtons("pca_dim", NULL, c("2D" = "2d", "3D" = "3d"), inline = TRUE)),
           plotlyOutput("pca_plot", height = "480px")),
      card(card_header("Choosing K: silhouette and elbow"),
           plotlyOutput("k_plot", height = "480px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Cluster quality: silhouette width by segment"),
           plotlyOutput("sil_plot", height = "320px")),
      card(card_header("What drives the PCA axes? (variable loadings)"),
           plotOutput("pca_var_plot", height = "320px"))
    ),
    card(card_header("Segment summary"), reactableOutput("segment_table"))
  ),

  # ---- Tab 3 ------------------------------------------------------------
  nav_panel(
    "Cohort Migration & Action Plan", icon = icon("route"),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Upgraded", textOutput("mv_up"),   showcase = icon("arrow-trend-up"),   theme = "success"),
      value_box("Stable",   textOutput("mv_same"), showcase = icon("equals"),           theme = "secondary"),
      value_box("Downgraded", textOutput("mv_down"), showcase = icon("arrow-trend-down"), theme = "danger")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header(textOutput("matrix_title")),
           plotlyOutput("migration_plot", height = "460px")),
      card(card_header("Migration alerts"), uiOutput("migration_insights"))
    ),
    card(card_header("Strategic recommendations (auto-generated)"), uiOutput("segment_recs")),
    card(
      card_header("Customer explorer",
                  downloadButton("download_csv", "Download CSV", class = "btn-sm btn-outline-primary")),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("f_segment", "Segment (empty = all)", choices = NULL, multiple = TRUE),
        selectInput("f_move", "Movement (empty = all)",
                    choices = c("Upgraded", "Stable", "Downgraded", "New"), multiple = TRUE),
        sliderInput("f_churn", "Minimum churn risk", min = 0, max = 1, value = 0, step = 0.05)
      ),
      reactableOutput("customer_table")
    )
  )
)

# =====================================================================
# SERVER  (what happens BEHIND the scenes)
# =====================================================================
server <- function(input, output, session) {

  # ---- The whole pipeline, run when "Run analysis" is clicked (and once at start) ----
  analysis <- reactive({
    if (input$source == "upload") req(input$file)

    withProgress(message = "Running Segmentrix pipeline", value = 0, {

      incProgress(0.10, detail = "Loading data")
      tx_raw <- if (input$source == "synthetic") {
        generate_synthetic_transactions(n_customers = input$n_customers, seed = input$seed)
      } else {
        load_transactions(input$file$datapath)
      }

      incProgress(0.15, detail = "Cleaning transactions")
      cleaned <- clean_transactions(tx_raw)
      validate(need(nrow(cleaned$data) > 0, "No valid transactions left after cleaning."))
      rp <- cleaned$report
      showNotification(
        sprintf("Loaded %s rows. Removed %s missing, %s refunds/zero-value and %s duplicate rows.",
                scales::comma(rp$rows_in), rp$dropped_missing,
                rp$dropped_nonpositive, rp$dropped_duplicates),
        type = "message")

      analysis_date <- max(cleaned$data$InvoiceDate) + 1

      incProgress(0.15, detail = "Calculating RFM")
      rfm <- compute_rfm(cleaned$data, analysis_date)
      validate(need(nrow(rfm) >= 50, "At least 50 distinct customers are needed for clustering."))

      incProgress(0.30, detail = "Optimising K and fitting K-means")
      k_manual <- if (input$k_mode == "manual") input$k else NULL
      k_method <- if (input$k_mode == "elbow") "elbow" else "silhouette"
      model <- fit_segments(rfm, k = k_manual, k_method = k_method)

      incProgress(0.15, detail = "Churn risk and migration")
      model$data <- model$data |>
        add_rfm_scores() |>
        compute_churn_risk() |>
        add_recommended_action()

      seg_summary <- build_segment_summary(model$data) |> add_narratives()
      migration   <- build_migration(cleaned$data, model, analysis_date, input$lookback)
      mig_insight <- generate_migration_insights(migration, input$lookback)

      customers <- model$data |>
        left_join(migration |> select(CustomerID, Prev_Segment, Movement, Vector),
                  by = "CustomerID")

      incProgress(0.15, detail = "Done")
      list(tx = cleaned$data, model = model, customers = customers,
           summary = seg_summary, migration = migration,
           mig_insights = mig_insight, lookback = input$lookback,
           pal = segment_colors(levels(model$data$Segment)))
    })
  }) |> bindEvent(input$run, ignoreNULL = FALSE)

  # Fill the segment filter once the analysis exists
  observeEvent(analysis(), {
    updateSelectInput(session, "f_segment", choices = levels(analysis()$customers$Segment))
  })

  # ---------------------------- TAB 1 ----------------------------------
  output$kpi_revenue <- renderText(scales::dollar(sum(analysis()$customers$Monetary), accuracy = 1))

  output$kpi_active <- renderText({
    d <- analysis()$customers
    sprintf("%s of %s", scales::comma(sum(d$Recency <= 90)), scales::comma(nrow(d)))
  })

  output$kpi_top_value <- renderText({
    s <- analysis()$summary
    scales::dollar(max(s$AvgSpend), accuracy = 1)
  })
  output$kpi_top_name <- renderText({
    s <- analysis()$summary
    as.character(s$Segment[which.max(s$AvgSpend)])
  })

  output$kpi_aov <- renderText({
    d <- analysis()$customers
    scales::dollar(sum(d$Monetary) / sum(d$Frequency), accuracy = 1)
  })

  output$spend_plot <- renderPlotly({
    a <- analysis()
    p <- ggplot(a$customers, aes(x = Monetary, fill = Segment)) +
      geom_histogram(bins = 40, colour = NA) +
      scale_x_log10(labels = scales::dollar) +
      scale_fill_manual(values = a$pal) +
      labs(x = "Lifetime spend (log scale)", y = "Customers", fill = NULL) +
      theme_minimal()
    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
  })

  output$rfm_heatmap <- renderPlotly({
    h <- analysis()$customers |>
      group_by(R_Score, F_Score) |>
      summarise(AvgSpend = mean(Monetary), Customers = n(), .groups = "drop") |>
      mutate(hover = sprintf("Recency score: %d<br>Frequency score: %d<br>Customers: %s<br>Avg spend: %s",
                             R_Score, F_Score, scales::comma(Customers),
                             scales::dollar(AvgSpend, accuracy = 1)))
    plot_ly(h, x = ~F_Score, y = ~R_Score, z = ~AvgSpend, type = "heatmap",
            colorscale = "Viridis", text = ~hover, hoverinfo = "text",
            colorbar = list(title = "Avg spend")) |>
      layout(xaxis = list(title = "Frequency score (5 = most frequent)", dtick = 1),
             yaxis = list(title = "Recency score (5 = most recent)", dtick = 1))
  })

  # ---------------------------- TAB 2 ----------------------------------
  output$pca_plot <- renderPlotly({
    a <- analysis()
    d <- a$customers
    if (nrow(d) > 5000) d <- slice_sample(d, n = 5000)   # keep the browser fast
    d <- mutate(d, hover = paste0(CustomerID, "<br>", Segment,
                                  "<br>Recency: ", Recency, " days",
                                  "<br>Orders: ", Frequency,
                                  "<br>Spend: ", scales::dollar(Monetary, accuracy = 1)))
    ve <- round(100 * a$model$pca$sdev^2 / sum(a$model$pca$sdev^2), 1)

    if (input$pca_dim == "3d") {
      plot_ly(d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Segment, colors = a$pal,
              type = "scatter3d", mode = "markers", text = ~hover, hoverinfo = "text",
              marker = list(size = 3, opacity = 0.7)) |>
        layout(scene = list(xaxis = list(title = sprintf("PC1 (%.1f%%)", ve[1])),
                            yaxis = list(title = sprintf("PC2 (%.1f%%)", ve[2])),
                            zaxis = list(title = sprintf("PC3 (%.1f%%)", ve[3]))))
    } else {
      plot_ly(d, x = ~PC1, y = ~PC2, color = ~Segment, colors = a$pal,
              type = "scatter", mode = "markers", text = ~hover, hoverinfo = "text",
              marker = list(size = 6, opacity = 0.65)) |>
        layout(xaxis = list(title = sprintf("PC1 (%.1f%% of variance)", ve[1])),
               yaxis = list(title = sprintf("PC2 (%.1f%% of variance)", ve[2])))
    }
  })

  output$k_plot <- renderPlotly({
    m  <- analysis()$model
    ke <- m$k_eval
    sel <- ke[ke$k == m$k, ]

    p1 <- plot_ly(ke, x = ~k, y = ~avg_silhouette, type = "scatter", mode = "lines+markers",
                  name = "Silhouette") |>
      layout(yaxis = list(title = "Avg silhouette (higher = better)"))
    p2 <- plot_ly(ke, x = ~k, y = ~tot_withinss, type = "scatter", mode = "lines+markers",
                  name = "Within-cluster SS") |>
      layout(yaxis = list(title = "Within-cluster SS (elbow)"), xaxis = list(title = "K", dtick = 1))
    if (nrow(sel) > 0) {
      p1 <- p1 |> add_markers(data = sel, x = ~k, y = ~avg_silhouette,
                              marker = list(size = 14, color = "red"), name = "Selected K")
      p2 <- p2 |> add_markers(data = sel, x = ~k, y = ~tot_withinss,
                              marker = list(size = 14, color = "red"), name = "Selected K")
    }
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) |>
      layout(showlegend = FALSE,
             title = list(text = sprintf("Selected K = %d  (silhouette suggests %d, elbow suggests %d)",
                                         m$k, m$k_silhouette, m$k_elbow),
                          font = list(size = 12)))
  })

  output$sil_plot <- renderPlotly({
    a  <- analysis()
    sb <- a$model$silhouette_by_segment |>
      mutate(Segment = factor(Segment, levels = rev(levels(a$customers$Segment))))
    plot_ly(sb, x = ~AvgSilhouette, y = ~Segment, color = ~Segment, colors = a$pal,
            type = "bar", orientation = "h") |>
      layout(showlegend = FALSE,
             xaxis = list(title = "Average silhouette width"),
             title = list(text = sprintf("Overall average silhouette: %.2f", a$model$overall_silhouette),
                          font = list(size = 12)))
  })

  output$pca_var_plot <- renderPlot({
    fviz_pca_var(analysis()$model$pca, col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE,
                 title = NULL)
  })

  output$segment_table <- renderReactable({
    a <- analysis()
    s <- a$summary |>
      mutate(Segment = as.character(Segment)) |>
      select(Segment, Customers, CustomerShare, Revenue, RevenueShare, AvgSpend,
             AvgOrderValue, AvgFrequency, AvgRecency, ChurnRisk, ValueAtRisk, Action)
    usd <- colFormat(currency = "USD", separators = TRUE, digits = 0)
    reactable(
      s, striped = TRUE, highlight = TRUE, defaultPageSize = 10,
      columns = list(
        Segment       = colDef(minWidth = 170,
                               style = function(value) list(color = a$pal[[value]], fontWeight = 600)),
        Customers     = colDef(format = colFormat(separators = TRUE)),
        CustomerShare = colDef(name = "% Customers", format = colFormat(percent = TRUE, digits = 1)),
        Revenue       = colDef(format = usd),
        RevenueShare  = colDef(name = "% Revenue", format = colFormat(percent = TRUE, digits = 1)),
        AvgSpend      = colDef(name = "Avg Spend", format = usd),
        AvgOrderValue = colDef(name = "Avg Order Value", format = usd),
        AvgFrequency  = colDef(name = "Avg Orders", format = colFormat(digits = 1)),
        AvgRecency    = colDef(name = "Avg Recency (days)", format = colFormat(digits = 0)),
        ChurnRisk     = colDef(name = "Churn Risk", format = colFormat(percent = TRUE, digits = 0),
                               style = churn_style),
        ValueAtRisk   = colDef(name = "Value at Risk", format = usd),
        Action        = colDef(name = "Recommended Action", minWidth = 260)
      )
    )
  })

  # ---------------------------- TAB 3 ----------------------------------
  move_count <- function(label) {
    sum(analysis()$migration$Movement == label)
  }
  output$mv_up   <- renderText(scales::comma(move_count("Upgraded")))
  output$mv_same <- renderText(scales::comma(move_count("Stable")))
  output$mv_down <- renderText(scales::comma(move_count("Downgraded")))

  output$matrix_title <- renderText(
    sprintf("Migration matrix: segment %d days ago (rows) vs. segment now (columns)", analysis()$lookback))

  output$migration_plot <- renderPlotly({
    a   <- analysis()
    mat <- migration_matrix(a$migration, levels(a$customers$Segment))
    z   <- unclass(mat)
    ann <- list()
    for (i in seq_len(nrow(z))) {
      for (j in seq_len(ncol(z))) {
        ann[[length(ann) + 1]] <- list(
          x = colnames(z)[j], y = rownames(z)[i], text = scales::comma(z[i, j]),
          showarrow = FALSE, font = list(color = if (z[i, j] > max(z) * 0.55) "white" else "#333"))
      }
    }
    plot_ly(x = colnames(z), y = rownames(z), z = z, type = "heatmap",
            colorscale = "Blues", reversescale = TRUE, showscale = FALSE,
            hovertemplate = "%{y} \u2192 %{x}<br>%{z} customers<extra></extra>") |>
      layout(annotations = ann,
             xaxis = list(title = "Segment now"),
             yaxis = list(title = "Segment before", autorange = "reversed"))
  })

  output$migration_insights <- renderUI({
    ins <- analysis()$mig_insights
    tagList(
      p(ins$overview),
      if (length(ins$risks) > 0) tagList(
        h6(icon("triangle-exclamation"), " Downgrade alerts"),
        tags$ul(lapply(ins$risks, tags$li))),
      if (length(ins$wins) > 0) tagList(
        h6(icon("circle-check"), " Positive movements"),
        tags$ul(lapply(ins$wins, tags$li)))
    )
  })

  output$segment_recs <- renderUI({
    s <- analysis()$summary
    panels <- lapply(seq_len(nrow(s)), function(i) {
      r  <- s[i, ]
      pb <- get_playbook(r$SegmentType)
      accordion_panel(
        title = sprintf("%s  |  %s of revenue  |  churn risk %s", r$Segment,
                        scales::percent(r$RevenueShare, accuracy = 0.1),
                        scales::percent(r$ChurnRisk, accuracy = 1)),
        value = as.character(r$Segment),
        p(r$Narrative),
        p(tags$strong("Action: "), pb$action, ". ", pb$detail),
        tags$small(class = "text-muted", "KPIs to track: ", pb$kpi)
      )
    })
    do.call(accordion, c(panels, list(open = TRUE, multiple = TRUE)))
  })

  filtered_customers <- reactive({
    d <- analysis()$customers
    if (length(input$f_segment) > 0) d <- filter(d, Segment %in% input$f_segment)
    if (length(input$f_move) > 0)    d <- filter(d, Movement %in% input$f_move)
    d |>
      filter(ChurnRisk >= input$f_churn) |>
      arrange(desc(ValueAtRisk)) |>
      select(CustomerID, Segment, Prev_Segment, Movement, Recency, Frequency,
             Monetary, AOV, TopCategory, ChurnRisk, ValueAtRisk, Action)
  })

  output$customer_table <- renderReactable({
    d   <- filtered_customers() |> mutate(Segment = as.character(Segment))
    usd <- colFormat(currency = "USD", separators = TRUE, digits = 0)
    reactable(
      d, searchable = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE,
      defaultPageSize = 15,
      columns = list(
        Prev_Segment = colDef(name = "Segment Before", minWidth = 150),
        Segment      = colDef(name = "Segment Now", minWidth = 150),
        Monetary     = colDef(name = "Lifetime Spend", format = usd),
        AOV          = colDef(format = usd),
        Recency      = colDef(name = "Recency (days)"),
        ChurnRisk    = colDef(name = "Churn Risk", format = colFormat(percent = TRUE, digits = 0),
                              style = churn_style),
        ValueAtRisk  = colDef(name = "Value at Risk", format = usd),
        Action       = colDef(name = "Recommended Action", minWidth = 240)
      )
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("segmentrix_customers_", Sys.Date(), ".csv"),
    content  = function(file) data.table::fwrite(filtered_customers(), file)
  )
}

shinyApp(ui, server)
