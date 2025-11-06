#' S3 Methods for cbamm Objects
#'
#' @name methods
NULL

#' Print Method for cbamm Objects
#'
#' @param x A cbamm object
#' @param digits Number of digits to display (default: 3)
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#' @export
print.cbamm <- function(x, digits = 3, ...) {
  cat("\nRandom-Effects Meta-Analysis\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Number of studies: %d\n\n", x$k))

  cat("Pooled Estimate:\n")
  cat(sprintf("  Effect: %.*f\n", digits, x$estimate))
  cat(sprintf("  95%% CI: (%.*f, %.*f)\n", digits, x$ci_lower, digits, x$ci_upper))
  cat(sprintf("  SE: %.*f\n", digits, x$se))
  cat(sprintf("  p-value: %s\n\n", format_p(x$p_value, digits = digits)))

  cat("Heterogeneity:\n")
  cat(sprintf("  τ²: %.*f\n", digits, x$tau2))
  cat(sprintf("  I²: %.*f%%\n", 1, x$I2))
  cat(sprintf("  Q: %.*f (df = %d, p = %s)\n",
              2, x$Q, x$Q_df, format_p(x$Q_pval, digits = digits)))

  invisible(x)
}

#' Summary Method for cbamm Objects
#'
#' @param object A cbamm object
#' @param ... Additional arguments (not used)
#' @return A summary object
#' @export
summary.cbamm <- function(object, ...) {
  cat("\n=== Meta-Analysis Summary ===\n\n")
  print.cbamm(object, ...)

  cat("\nPrediction Interval (95%):\n")
  pred_int <- prediction_interval(object$estimate, object$se, object$tau2, object$k)
  cat(sprintf("  (%.*f, %.*f)\n", 3, pred_int$lower, 3, pred_int$upper))

  cat("\nStudy Weights:\n")
  weights_df <- data.frame(
    study = object$data$study,
    weight = round(object$weights * 100, 2)
  )
  weights_df <- weights_df[order(-weights_df$weight), ]
  print(head(weights_df, 10))

  if (nrow(weights_df) > 10) {
    cat(sprintf("\n... and %d more studies\n", nrow(weights_df) - 10))
  }

  invisible(object)
}

#' Print Method for Cumulative Meta-Analysis
#'
#' @param x A cbamm_cumulative object
#' @param digits Number of digits to display
#' @param ... Additional arguments
#' @return Invisibly returns the input object
#' @export
print.cbamm_cumulative <- function(x, digits = 3, ...) {
  cat("\nCumulative Meta-Analysis\n")
  cat(sprintf("Total studies: %d\n", x$n_studies))
  cat(sprintf("Ordered by: %s\n\n", x$order_by))

  cat("Final Pooled Estimate:\n")
  cat(sprintf("  Effect: %.*f\n", digits, x$final_estimate$estimate))
  cat(sprintf("  95%% CI: (%.*f, %.*f)\n",
              digits, x$final_estimate$ci_lower, digits, x$final_estimate$ci_upper))

  if (x$is_stable) {
    cat(sprintf("\nStability: Achieved at study %d\n", x$stability_index + 1))
    cat(sprintf("  (Changes < %.3f after this point)\n", x$stability_threshold))
  } else {
    cat("\nStability: Not achieved\n")
    cat("  (Estimates still changing with new studies)\n")
  }

  cat("\nCumulative Results (last 5 additions):\n")
  n_rows <- nrow(x$cumulative_results)
  start_row <- max(1, n_rows - 4)
  print(x$cumulative_results[start_row:n_rows, c("k", "estimate", "ci_lower", "ci_upper", "I2")])

  invisible(x)
}

#' Print Method for Meta-Regression
#'
#' @param x A cbamm_metareg object
#' @param digits Number of digits to display
#' @param ... Additional arguments
#' @return Invisibly returns the input object
#' @export
print.cbamm_metareg <- function(x, digits = 3, ...) {
  cat("\nMeta-Regression Analysis\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Number of studies: %d\n", x$k))
  cat(sprintf("Number of predictors: %d\n\n", x$p))

  cat("Coefficients:\n")
  coef_table <- data.frame(
    Estimate = round(x$coefficients, digits),
    SE = round(x$se, digits),
    CI_lower = round(x$ci_lower, digits),
    CI_upper = round(x$ci_upper, digits),
    p_value = sapply(x$p_values, format_p, digits = digits)
  )
  rownames(coef_table) <- x$coef_names
  print(coef_table)

  cat(sprintf("\nResidual Heterogeneity:\n"))
  cat(sprintf("  τ²: %.*f\n", digits, x$tau2))
  if (!is.na(x$R2)) {
    cat(sprintf("  R²: %.*f%% (variance explained)\n", 1, x$R2 * 100))
  }
  cat(sprintf("  Test of residual heterogeneity: QE = %.*f, df = %d, p = %s\n",
              2, x$QE, x$QE_df, format_p(x$QE_pval, digits = digits)))

  invisible(x)
}

#' Print Method for Subgroup Analysis
#'
#' @param x A cbamm_subgroup object
#' @param digits Number of digits to display
#' @param ... Additional arguments
#' @return Invisibly returns the input object
#' @export
print.cbamm_subgroup <- function(x, digits = 3, ...) {
  cat("\nSubgroup Meta-Analysis\n")
  cat(sprintf("Subgroup variable: %s\n", x$subgroup_var))
  cat(sprintf("Number of subgroups: %d\n\n", x$n_subgroups))

  cat("Subgroup Results:\n")
  for (name in names(x$subgroup_results)) {
    sg <- x$subgroup_results[[name]]
    cat(sprintf("\n  %s (k = %d):\n", name, sg$n_studies))
    cat(sprintf("    Effect: %.*f (95%% CI: %.*f, %.*f)\n",
                digits, sg$estimate, digits, sg$ci_lower, digits, sg$ci_upper))
    cat(sprintf("    I²: %.*f%%\n", 1, sg$I2))
  }

  if (!is.null(x$between_test)) {
    cat("\nTest for Subgroup Differences:\n")
    cat(sprintf("  Q_between = %.*f, df = %d, p = %s\n",
                2, x$between_test$Q, x$between_test$df,
                format_p(x$between_test$p_value, digits = digits)))
  }

  invisible(x)
}
