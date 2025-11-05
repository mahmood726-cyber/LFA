#' Reporting and Export Utilities
#'
#' Functions for generating reports and exporting meta-analysis results
#'
#' @name reporting
NULL

#' Generate Summary Table
#'
#' Creates a formatted summary table of meta-analysis results.
#'
#' @param result A cbamm result object or list of results
#' @param format Output format: "data.frame" (default), "markdown", or "latex"
#' @param digits Number of decimal places (default: 3)
#'
#' @return Summary table in requested format
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' result <- cbamm_fast(example_meta)
#' summary_table(result)
#' }
summary_table <- function(result, format = "data.frame", digits = 3) {
  if (inherits(result, "cbamm")) {
    # Single result
    table_data <- data.frame(
      Estimate = round(result$estimate, digits),
      SE = round(result$se, digits),
      CI_Lower = round(result$ci_lower, digits),
      CI_Upper = round(result$ci_upper, digits),
      P_value = format_p(result$p_value, digits = digits),
      K = result$k,
      I2 = sprintf("%.1f%%", result$I2),
      Tau2 = round(result$tau2, digits),
      stringsAsFactors = FALSE
    )
    rownames(table_data) <- "Pooled Effect"
  } else if (is.list(result)) {
    # Multiple results
    stop("Multiple results not yet implemented")
  } else {
    stop("Unsupported result object")
  }

  if (format == "markdown") {
    # Convert to markdown table
    output <- paste0("|", paste(names(table_data), collapse = " | "), "|\n")
    output <- paste0(output, "|", paste(rep("---", ncol(table_data)), collapse = " | "), "|\n")
    output <- paste0(output, "|", paste(table_data[1, ], collapse = " | "), "|\n")
    return(cat(output))
  } else if (format == "latex") {
    # Basic LaTeX table
    output <- "\\begin{tabular}{l"
    output <- paste0(output, paste(rep("r", ncol(table_data)), collapse = ""), "}\n")
    output <- paste0(output, "\\hline\n")
    output <- paste0(output, paste(names(table_data), collapse = " & "), " \\\\\n")
    output <- paste0(output, "\\hline\n")
    output <- paste0(output, paste(table_data[1, ], collapse = " & "), " \\\\\n")
    output <- paste0(output, "\\hline\n")
    output <- paste0(output, "\\end{tabular}")
    return(cat(output))
  } else {
    return(table_data)
  }
}


#' Export Study-Level Data
#'
#' Exports study-level data with calculated statistics to CSV or other formats.
#'
#' @param result A cbamm result object
#' @param file Output file path (if NULL, returns data frame)
#' @param include_weights Logical indicating whether to include study weights
#'
#' @return Data frame (invisibly if file is specified)
#'
#' @export
export_study_data <- function(result, file = NULL, include_weights = TRUE) {
  if (!inherits(result, "cbamm")) {
    stop("result must be a cbamm object")
  }

  export_data <- result$data

  if (include_weights) {
    export_data$weight_percent <- round(result$weights * 100, 2)
  }

  # Add confidence intervals if not present
  if (!"ci_lower" %in% names(export_data)) {
    export_data$ci_lower <- export_data$effect - 1.96 * export_data$se
    export_data$ci_upper <- export_data$effect + 1.96 * export_data$se
  }

  if (!is.null(file)) {
    write.csv(export_data, file = file, row.names = FALSE)
    cat(sprintf("Study data exported to %s\n", file))
  }

  invisible(export_data)
}


#' Generate Meta-Analysis Report
#'
#' Creates a comprehensive text report of meta-analysis results.
#'
#' @param result A cbamm result object
#' @param file Output file path (if NULL, prints to console)
#' @param include_studies Logical indicating whether to include individual studies
#' @param title Report title
#'
#' @return Character vector with report text (invisibly)
#'
#' @export
generate_report <- function(result, file = NULL, include_studies = TRUE,
                            title = "Meta-Analysis Report") {
  if (!inherits(result, "cbamm")) {
    stop("result must be a cbamm object")
  }

  report <- character()

  # Title and header
  report <- c(report, paste(rep("=", nchar(title)), collapse = ""))
  report <- c(report, title)
  report <- c(report, paste(rep("=", nchar(title)), collapse = ""))
  report <- c(report, "")
  report <- c(report, sprintf("Date: %s", Sys.Date()))
  report <- c(report, "")

  # Summary statistics
  report <- c(report, "SUMMARY RESULTS")
  report <- c(report, paste(rep("-", 50), collapse = ""))
  report <- c(report, sprintf("Number of studies: %d", result$k))
  report <- c(report, sprintf("Method: %s", result$method))
  report <- c(report, "")
  report <- c(report, sprintf("Pooled effect size: %.3f", result$estimate))
  report <- c(report, sprintf("95%% Confidence interval: (%.3f, %.3f)",
                              result$ci_lower, result$ci_upper))
  report <- c(report, sprintf("Standard error: %.3f", result$se))
  report <- c(report, sprintf("P-value: %s", format_p(result$p_value)))
  report <- c(report, "")

  # Heterogeneity
  report <- c(report, "HETEROGENEITY ASSESSMENT")
  report <- c(report, paste(rep("-", 50), collapse = ""))
  report <- c(report, sprintf("I² statistic: %.1f%%", result$I2))
  report <- c(report, sprintf("Tau² (between-study variance): %.4f", result$tau2))
  report <- c(report, sprintf("Cochran's Q: %.2f (df = %d, p = %s)",
                              result$Q, result$Q_df, format_p(result$Q_pval)))

  # Interpretation
  report <- c(report, "")
  heterogeneity_level <- if (result$I2 < 25) "low" else
                         if (result$I2 < 50) "moderate" else
                         if (result$I2 < 75) "substantial" else "considerable"
  report <- c(report, sprintf("Interpretation: %s heterogeneity",
                              tools::toTitleCase(heterogeneity_level)))
  report <- c(report, "")

  # Prediction interval
  pred_int <- prediction_interval(result$estimate, result$se, result$tau2, result$k)
  report <- c(report, sprintf("95%% Prediction interval: (%.3f, %.3f)",
                              pred_int$lower, pred_int$upper))
  report <- c(report, "")

  # Individual studies if requested
  if (include_studies) {
    report <- c(report, "INDIVIDUAL STUDIES")
    report <- c(report, paste(rep("-", 50), collapse = ""))

    for (i in 1:result$k) {
      study_name <- result$data$study[i]
      effect <- result$data$effect[i]
      se <- result$data$se[i]
      ci_lower <- effect - 1.96 * se
      ci_upper <- effect + 1.96 * se
      weight <- result$weights[i] * 100

      report <- c(report, sprintf("%s:", study_name))
      report <- c(report, sprintf("  Effect: %.3f (95%% CI: %.3f, %.3f), Weight: %.1f%%",
                                  effect, ci_lower, ci_upper, weight))
    }
  }

  # Footer
  report <- c(report, "")
  report <- c(report, paste(rep("=", 50), collapse = ""))
  report <- c(report, sprintf("Report generated by cbamm package version %s",
                              packageVersion("cbamm")))

  # Output
  if (!is.null(file)) {
    writeLines(report, con = file)
    cat(sprintf("Report saved to %s\n", file))
  } else {
    cat(paste(report, collapse = "\n"), "\n")
  }

  invisible(report)
}


#' Check PRISMA Completeness
#'
#' Provides a checklist for PRISMA (Preferred Reporting Items for Systematic
#' Reviews and Meta-Analyses) reporting guidelines.
#'
#' @param interactive Logical indicating whether to use interactive prompts
#'
#' @return List with PRISMA checklist items
#'
#' @export
prisma_checklist <- function(interactive = FALSE) {
  checklist <- list(
    title = "Title: Identify the report as a systematic review, meta-analysis, or both",
    abstract = "Abstract: Provide a structured summary including objectives, data sources, eligibility criteria, results, and conclusions",
    introduction = "Introduction: Describe the rationale and objectives",
    methods_protocol = "Methods - Protocol: Indicate if a review protocol exists and where it can be accessed",
    methods_eligibility = "Methods - Eligibility criteria: Specify study characteristics and report characteristics used as criteria for eligibility",
    methods_sources = "Methods - Information sources: Describe all sources (databases, registers) and dates searched",
    methods_search = "Methods - Search: Present full electronic search strategy for at least one database",
    methods_selection = "Methods - Study selection: State the process for selecting studies",
    methods_extraction = "Methods - Data collection process: Describe method of data extraction and confirmation",
    methods_data_items = "Methods - Data items: List and define all variables sought",
    methods_rob = "Methods - Risk of bias: Describe methods for assessing risk of bias",
    methods_synthesis = "Methods - Synthesis: Describe methods of handling data and combining results",
    results_selection = "Results - Study selection: Give numbers of studies screened, assessed, and included with reasons for exclusions",
    results_characteristics = "Results - Study characteristics: Cite each included study and present characteristics",
    results_rob = "Results - Risk of bias: Present assessments of risk of bias",
    results_individual = "Results - Individual studies: Present results for each study including confidence intervals",
    results_synthesis = "Results - Synthesis: Present results of meta-analysis including confidence intervals and measures of consistency",
    results_rob_across = "Results - Risk of bias across studies: Present results of assessment of publication bias",
    discussion = "Discussion: Summarize main findings, limitations, and interpretation",
    funding = "Funding: Describe sources of funding and role of funders"
  )

  if (interactive) {
    cat("\n=== PRISMA Reporting Checklist ===\n\n")
    cat("This is a simplified checklist. For complete guidelines, see:\n")
    cat("http://www.prisma-statement.org/\n\n")

    for (i in seq_along(checklist)) {
      cat(sprintf("%d. %s\n", i, checklist[[i]]))
    }
  }

  invisible(checklist)
}


#' Power Analysis for Meta-Analysis
#'
#' Estimates power to detect an effect of specified size given number of studies.
#'
#' @param k Number of studies
#' @param effect_size Expected effect size
#' @param heterogeneity Expected I² (as proportion, 0-1)
#' @param avg_n Average sample size per study
#' @param alpha Significance level (default: 0.05)
#'
#' @return List with power estimate and related statistics
#'
#' @export
#' @examples
#' \dontrun{
#' power_analysis_ma(k = 10, effect_size = 0.5, heterogeneity = 0.25, avg_n = 50)
#' }
power_analysis_ma <- function(k, effect_size, heterogeneity, avg_n, alpha = 0.05) {
  # Approximate within-study variance
  within_var <- 4 / avg_n  # Rough approximation for standardized mean difference

  # Between-study variance from I²
  # I² = τ² / (τ² + v), where v is typical within-study variance
  tau2 <- (heterogeneity * within_var) / (1 - heterogeneity)

  # Total variance of pooled estimate
  total_var <- (within_var + tau2) / k

  # Standard error
  se_pooled <- sqrt(total_var)

  # Non-centrality parameter
  ncp <- effect_size / se_pooled

  # Critical value
  z_crit <- qnorm(1 - alpha/2)

  # Power
  power <- pnorm(ncp - z_crit) + pnorm(-ncp - z_crit)

  result <- list(
    k = k,
    effect_size = effect_size,
    heterogeneity_I2 = heterogeneity * 100,
    avg_n = avg_n,
    power = power,
    se_pooled = se_pooled,
    detectable_effect = z_crit * se_pooled,
    interpretation = if (power >= 0.80) "Adequate power (≥80%)" else
                     if (power >= 0.50) "Moderate power (50-80%)" else
                     "Low power (<50%)"
  )

  return(result)
}
