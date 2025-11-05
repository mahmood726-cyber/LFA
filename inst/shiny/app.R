#' Interactive Shiny Dashboard for Meta-Analysis
#'
#' This Shiny app provides an interactive interface for conducting meta-analyses
#' using the cbamm package.
#'
#' @export
#' @examples
#' \dontrun{
#' # Launch the Shiny app
#' launch_meta_dashboard()
#' }

library(shiny)

# UI
ui <- fluidPage(
  titlePanel("CBamm: Meta-Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      h3("Data Input"),
      fileInput("datafile", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values", ".csv")),

      hr(),

      selectInput("analysis_type", "Analysis Type:",
                  choices = c("Basic Meta-Analysis" = "basic",
                             "Cumulative Analysis" = "cumulative",
                             "Publication Bias" = "bias",
                             "Meta-Regression" = "regression",
                             "Network Meta-Analysis" = "network")),

      conditionalPanel(
        condition = "input.analysis_type == 'basic'",
        selectInput("method", "Method:",
                    choices = c("DL", "REML"))
      ),

      conditionalPanel(
        condition = "input.analysis_type == 'cumulative'",
        textInput("order_by", "Order by:", value = "year")
      ),

      conditionalPanel(
        condition = "input.analysis_type == 'regression'",
        textInput("covariates", "Covariates (comma-separated):", value = "")
      ),

      hr(),
      actionButton("run_analysis", "Run Analysis", class = "btn-primary"),

      hr(),
      h4("Example Data"),
      actionButton("load_example", "Load Example Dataset")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h3("Analysis Results"),
                 verbatimTextOutput("results_summary"),
                 plotOutput("main_plot", height = "500px")
        ),

        tabPanel("Forest Plot",
                 plotOutput("forest_plot", height = "600px"),
                 downloadButton("download_forest", "Download Plot")
        ),

        tabPanel("Publication Bias",
                 plotOutput("funnel_plot", height = "500px"),
                 verbatimTextOutput("bias_tests")
        ),

        tabPanel("Data Table",
                 DT::dataTableOutput("data_table")
        ),

        tabPanel("Report",
                 h3("Generate Report"),
                 downloadButton("download_report", "Download Report"),
                 hr(),
                 verbatimTextOutput("report_preview")
        ),

        tabPanel("Help",
                 h3("How to Use This Dashboard"),
                 tags$ol(
                   tags$li("Upload your data as a CSV file with columns: study, effect, se"),
                   tags$li("Select the analysis type you want to perform"),
                   tags$li("Configure analysis options in the sidebar"),
                   tags$li("Click 'Run Analysis' to see results"),
                   tags$li("Explore different tabs for plots and detailed results"),
                   tags$li("Download plots and reports as needed")
                 ),
                 hr(),
                 h4("Required Data Format:"),
                 tags$pre("study,effect,se\nStudy1,0.5,0.1\nStudy2,0.6,0.15\nStudy3,0.4,0.12")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive data
  data_uploaded <- reactiveVal(NULL)
  analysis_result <- reactiveVal(NULL)

  # Load example data
  observeEvent(input$load_example, {
    example_data <- data.frame(
      study = paste0("Study", 1:10),
      effect = c(0.45, 0.62, 0.38, 0.71, 0.52, 0.48, 0.67, 0.55, 0.43, 0.59),
      se = c(0.12, 0.18, 0.10, 0.22, 0.14, 0.11, 0.19, 0.15, 0.13, 0.16),
      year = 2015:2024
    )
    data_uploaded(example_data)
    showNotification("Example data loaded!", type = "message")
  })

  # Upload data
  observeEvent(input$datafile, {
    req(input$datafile)
    tryCatch({
      data <- read.csv(input$datafile$datapath)
      data_uploaded(data)
      showNotification("Data uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Run analysis
  observeEvent(input$run_analysis, {
    req(data_uploaded())

    tryCatch({
      data <- data_uploaded()

      result <- switch(input$analysis_type,
        "basic" = {
          if (!requireNamespace("cbamm", quietly = TRUE)) {
            # If package not loaded, use simplified version
            list(
              estimate = mean(data$effect),
              se = sd(data$effect) / sqrt(nrow(data)),
              type = "basic"
            )
          } else {
            cbamm::cbamm_fast(data, method = input$method)
          }
        },
        "cumulative" = {
          if ("year" %in% names(data)) {
            cbamm::cumulative_meta_analysis(data, order_by = input$order_by)
          } else {
            stop("Year column required for cumulative analysis")
          }
        },
        "bias" = {
          cbamm::assess_publication_bias(data)
        },
        "regression" = {
          covs <- strsplit(input$covariates, ",")[[1]]
          covs <- trimws(covs)
          if (length(covs) > 0 && all(covs %in% names(data))) {
            formula_obj <- as.formula(paste("~", paste(covs, collapse = " + ")))
            cbamm::meta_regression(data, formula_obj)
          } else {
            stop("Invalid covariates specified")
          }
        },
        "network" = {
          stop("Network meta-analysis requires different data format")
        }
      )

      analysis_result(result)
      showNotification("Analysis complete!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Results summary
  output$results_summary <- renderPrint({
    req(analysis_result())
    result <- analysis_result()
    print(result)
  })

  # Main plot
  output$main_plot <- renderPlot({
    req(analysis_result(), data_uploaded())
    result <- analysis_result()
    data <- data_uploaded()

    if (inherits(result, "cbamm")) {
      plot(1, 1, type = "n", main = "Meta-Analysis Result",
           xlab = "Effect Size", ylab = "",
           xlim = c(result$ci_lower - 0.5, result$ci_upper + 0.5))
      segments(result$ci_lower, 1, result$ci_upper, 1, lwd = 3)
      points(result$estimate, 1, pch = 18, cex = 3, col = "blue")
      abline(v = 0, lty = 2, col = "red")
    }
  })

  # Forest plot
  output$forest_plot <- renderPlot({
    req(analysis_result(), data_uploaded())
    result <- analysis_result()
    data <- data_uploaded()

    if (inherits(result, "cbamm")) {
      # Simplified forest plot
      k <- nrow(data)
      plot(NULL, xlim = range(c(data$effect - 1.96*data$se, data$effect + 1.96*data$se)),
           ylim = c(0, k + 2), xlab = "Effect Size", ylab = "", yaxt = "n",
           main = "Forest Plot")

      for (i in 1:k) {
        y_pos <- k - i + 2
        segments(data$effect[i] - 1.96*data$se[i], y_pos,
                data$effect[i] + 1.96*data$se[i], y_pos)
        points(data$effect[i], y_pos, pch = 15)
        axis(2, at = y_pos, labels = data$study[i], las = 1, tick = FALSE)
      }

      abline(v = 0, lty = 2, col = "gray")
      segments(result$ci_lower, 1, result$ci_upper, 1, lwd = 3, col = "blue")
      points(result$estimate, 1, pch = 18, cex = 2, col = "blue")
      axis(2, at = 1, labels = "Pooled", las = 1, tick = FALSE, font = 2)
    }
  })

  # Funnel plot
  output$funnel_plot <- renderPlot({
    req(data_uploaded())
    data <- data_uploaded()

    precision <- 1 / data$se
    plot(data$effect, precision,
         xlab = "Effect Size", ylab = "Precision (1/SE)",
         main = "Funnel Plot",
         pch = 19, col = rgb(0,0,0,0.6))
    abline(v = mean(data$effect), lty = 2, col = "blue")
  })

  # Data table
  output$data_table <- DT::renderDataTable({
    req(data_uploaded())
    DT::datatable(data_uploaded(), options = list(pageLength = 10))
  })

  # Report preview
  output$report_preview <- renderPrint({
    req(analysis_result())
    cat("Meta-Analysis Report\n")
    cat("===================\n\n")
    cat("This is a preview. Download full report using the button above.\n")
  })
}

# Function to launch the app
#' @export
launch_meta_dashboard <- function() {
  shinyApp(ui = ui, server = server)
}

# If running directly
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
