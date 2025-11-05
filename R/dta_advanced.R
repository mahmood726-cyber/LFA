#' Advanced Diagnostic Test Accuracy Methods
#'
#' State-of-the-art methods for meta-analysis of diagnostic test accuracy studies
#' Based on bivariate and HSROC models
#'
#' @name dta_advanced
NULL

#' Bivariate Meta-Analysis for Diagnostic Test Accuracy
#'
#' Implements a simplified bivariate random-effects model for jointly analyzing
#' sensitivity and specificity. This accounts for the negative correlation
#' between sensitivity and specificity that typically arises from threshold effects.
#'
#' @param data Data frame with columns: tp (true positives), fp (false positives),
#'   fn (false negatives), tn (true negatives)
#' @param method Method for estimation: "reitsma" (default) or "simple"
#'
#' @return Object of class "dta_bivariate" containing:
#'   \itemize{
#'     \item summary_sens: Summary sensitivity estimate and CI
#'     \item summary_spec: Summary specificity estimate and CI
#'     \item correlation: Estimated correlation between logit(sens) and logit(spec)
#'     \item study_results: Study-level estimates
#'     \item sroc: Summary ROC curve data
#'   }
#'
#' @references
#' Reitsma JB, Glas AS, Rutjes AWS, Scholten RJPM, Bossuyt PM, Zwinderman AH (2005).
#' Bivariate analysis of sensitivity and specificity produces informative summary
#' measures in diagnostic reviews. Journal of Clinical Epidemiology, 58(10):982-990.
#'
#' Rutter CM, Gatsonis CA (2001). A hierarchical regression approach to
#' meta-analysis of diagnostic test accuracy evaluations. Statistics in Medicine,
#' 20(19):2865-2884.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_dta)
#' bivariate_result <- dta_bivariate(example_dta)
#' print(bivariate_result)
#' }
dta_bivariate <- function(data, method = "reitsma") {
  # Validate data
  required_cols <- c("tp", "fp", "fn", "tn")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  k <- nrow(data)
  if (k < 4) {
    stop("Need at least 4 studies for bivariate DTA meta-analysis")
  }

  # Calculate sensitivity and specificity for each study
  sens <- data$tp / (data$tp + data$fn)
  spec <- data$tn / (data$tn + data$fp)

  # Logit transformations
  # Add continuity correction for 0 or 1 values
  sens_adj <- pmax(0.001, pmin(0.999, sens))
  spec_adj <- pmax(0.001, pmin(0.999, spec))

  logit_sens <- log(sens_adj / (1 - sens_adj))
  logit_spec <- log(spec_adj / (1 - spec_adj))

  # Variance of logit(sensitivity)
  var_logit_sens <- 1 / data$tp + 1 / data$fn

  # Variance of logit(specificity)
  var_logit_spec <- 1 / data$tn + 1 / data$fp

  if (method == "simple") {
    # Simplified approach: separate univariate meta-analyses
    # Meta-analysis of logit(sensitivity)
    ma_sens <- cbamm_fast(data.frame(
      study = if ("study" %in% names(data)) data$study else paste0("Study", 1:k),
      effect = logit_sens,
      se = sqrt(var_logit_sens)
    ), verbose = FALSE)

    # Meta-analysis of logit(specificity)
    ma_spec <- cbamm_fast(data.frame(
      study = if ("study" %in% names(data)) data$study else paste0("Study", 1:k),
      effect = logit_spec,
      se = sqrt(var_logit_spec)
    ), verbose = FALSE)

    # Back-transform to probability scale
    summary_sens <- exp(ma_sens$estimate) / (1 + exp(ma_sens$estimate))
    summary_spec <- exp(ma_spec$estimate) / (1 + exp(ma_spec$estimate))

    sens_ci_lower <- exp(ma_sens$ci_lower) / (1 + exp(ma_sens$ci_lower))
    sens_ci_upper <- exp(ma_sens$ci_upper) / (1 + exp(ma_sens$ci_upper))

    spec_ci_lower <- exp(ma_spec$ci_lower) / (1 + exp(ma_spec$ci_lower))
    spec_ci_upper <- exp(ma_spec$ci_upper) / (1 + exp(ma_spec$ci_upper))

    # Estimate correlation (simplified)
    correlation <- cor(logit_sens, logit_spec)

  } else if (method == "reitsma") {
    # Reitsma bivariate approach (simplified implementation)
    # This is a simplified version - full implementation would use nlme or similar

    # Weighted means
    w_sens <- 1 / var_logit_sens
    w_spec <- 1 / var_logit_spec

    mean_logit_sens <- sum(w_sens * logit_sens) / sum(w_sens)
    mean_logit_spec <- sum(w_spec * logit_spec) / sum(w_spec)

    # Estimate between-study variance (simplified DL method)
    Q_sens <- sum(w_sens * (logit_sens - mean_logit_sens)^2)
    Q_spec <- sum(w_spec * (logit_spec - mean_logit_spec)^2)

    C_sens <- sum(w_sens) - sum(w_sens^2) / sum(w_sens)
    C_spec <- sum(w_spec) - sum(w_spec^2) / sum(w_spec)

    tau2_sens <- max(0, (Q_sens - (k - 1)) / C_sens)
    tau2_spec <- max(0, (Q_spec - (k - 1)) / C_spec)

    # Random-effects weights
    w_sens_re <- 1 / (var_logit_sens + tau2_sens)
    w_spec_re <- 1 / (var_logit_spec + tau2_spec)

    mean_logit_sens_re <- sum(w_sens_re * logit_sens) / sum(w_sens_re)
    mean_logit_spec_re <- sum(w_spec_re * logit_spec) / sum(w_spec_re)

    se_logit_sens_re <- sqrt(1 / sum(w_sens_re))
    se_logit_spec_re <- sqrt(1 / sum(w_spec_re))

    # Confidence intervals on logit scale
    logit_sens_ci_lower <- mean_logit_sens_re - 1.96 * se_logit_sens_re
    logit_sens_ci_upper <- mean_logit_sens_re + 1.96 * se_logit_sens_re

    logit_spec_ci_lower <- mean_logit_spec_re - 1.96 * se_logit_spec_re
    logit_spec_ci_upper <- mean_logit_spec_re + 1.96 * se_logit_spec_re

    # Back-transform
    summary_sens <- exp(mean_logit_sens_re) / (1 + exp(mean_logit_sens_re))
    summary_spec <- exp(mean_logit_spec_re) / (1 + exp(mean_logit_spec_re))

    sens_ci_lower <- exp(logit_sens_ci_lower) / (1 + exp(logit_sens_ci_lower))
    sens_ci_upper <- exp(logit_sens_ci_upper) / (1 + exp(logit_sens_ci_upper))

    spec_ci_lower <- exp(logit_spec_ci_lower) / (1 + exp(logit_spec_ci_lower))
    spec_ci_upper <- exp(logit_spec_ci_upper) / (1 + exp(logit_spec_ci_upper))

    # Correlation
    correlation <- cor(logit_sens, logit_spec)
  } else {
    stop("method must be 'simple' or 'reitsma'")
  }

  # Study-level results
  study_results <- data.frame(
    study = if ("study" %in% names(data)) data$study else paste0("Study", 1:k),
    sensitivity = sens,
    specificity = spec,
    logit_sens = logit_sens,
    logit_spec = logit_spec,
    stringsAsFactors = FALSE
  )

  # Generate SROC curve points
  # Create a range of specificities
  spec_range <- seq(0.01, 0.99, by = 0.01)
  logit_spec_range <- log(spec_range / (1 - spec_range))

  # Simple linear relationship (can be made more sophisticated)
  # logit(sens) = a + b * logit(spec) + error
  if (k >= 3) {
    fit <- lm(logit_sens ~ logit_spec)
    logit_sens_predicted <- predict(fit, newdata = data.frame(logit_spec = logit_spec_range))
    sens_predicted <- exp(logit_sens_predicted) / (1 + exp(logit_sens_predicted))

    sroc_data <- data.frame(
      specificity = spec_range,
      sensitivity = sens_predicted
    )
  } else {
    sroc_data <- NULL
  }

  result <- list(
    summary_sens = summary_sens,
    summary_spec = summary_spec,
    sens_ci_lower = sens_ci_lower,
    sens_ci_upper = sens_ci_upper,
    spec_ci_lower = spec_ci_lower,
    spec_ci_upper = spec_ci_upper,
    correlation = correlation,
    study_results = study_results,
    sroc = sroc_data,
    k = k,
    method = method,
    note = "Simplified bivariate model. For full implementation, consider mada or metandi packages."
  )

  class(result) <- "dta_bivariate"
  return(result)
}


#' Summary ROC Plot for DTA
#'
#' Creates a summary ROC plot showing individual studies and summary estimates.
#'
#' @param dta_result Object from dta_bivariate() function
#' @param show_ci Logical indicating whether to show confidence regions
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_dta)
#' bivariate_result <- dta_bivariate(example_dta)
#' sroc_plot(bivariate_result)
#' }
sroc_plot <- function(dta_result, show_ci = TRUE,
                      main = "Summary ROC Plot") {
  if (!inherits(dta_result, "dta_bivariate")) {
    stop("Input must be a dta_bivariate object")
  }

  # Get study data
  study_data <- dta_result$study_results

  # Create plot (1-specificity on x-axis, sensitivity on y-axis)
  plot(1 - study_data$specificity, study_data$sensitivity,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "1 - Specificity (False Positive Rate)",
       ylab = "Sensitivity (True Positive Rate)",
       main = main,
       pch = 19,
       col = rgb(0, 0, 1, 0.6),
       cex = 1.5)

  # Add diagonal reference line (no discrimination)
  abline(a = 0, b = 1, lty = 3, col = "gray70")

  # Add SROC curve if available
  if (!is.null(dta_result$sroc)) {
    lines(1 - dta_result$sroc$specificity, dta_result$sroc$sensitivity,
          col = "darkblue", lwd = 2)
  }

  # Add summary point
  points(1 - dta_result$summary_spec, dta_result$summary_sens,
         pch = 18, cex = 3, col = "red")

  # Add confidence region if requested (simplified)
  if (show_ci) {
    # Horizontal CI for sensitivity
    segments(1 - dta_result$summary_spec,
             dta_result$sens_ci_lower,
             1 - dta_result$summary_spec,
             dta_result$sens_ci_upper,
             col = "red", lwd = 2)

    # Vertical CI for specificity
    segments(1 - dta_result$spec_ci_upper,
             dta_result$summary_sens,
             1 - dta_result$spec_ci_lower,
             dta_result$summary_sens,
             col = "red", lwd = 2)
  }

  # Add grid
  grid(col = "gray80", lty = "dotted")

  # Add legend
  legend("bottomright",
         legend = c("Individual studies", "Summary point", "SROC curve"),
         pch = c(19, 18, NA),
         lty = c(NA, NA, 1),
         col = c(rgb(0, 0, 1, 0.6), "red", "darkblue"),
         pt.cex = c(1.5, 3, NA),
         lwd = c(NA, NA, 2),
         bty = "n")

  invisible(NULL)
}


#' Coupled Forest Plot for Sensitivity and Specificity
#'
#' Creates side-by-side forest plots for sensitivity and specificity.
#'
#' @param data DTA data frame
#' @param dta_result Optional dta_bivariate result object
#' @param main Overall plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
dta_forest_plot <- function(data, dta_result = NULL, main = "DTA Forest Plot") {
  # Calculate sensitivity and specificity if not provided
  if (is.null(dta_result)) {
    dta_result <- dta_bivariate(data, method = "simple")
  }

  study_data <- dta_result$study_results
  k <- nrow(study_data)

  # Set up side-by-side plots
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(1, 2), mar = c(5, 8, 4, 2))

  # Sensitivity forest plot
  plot(NULL, xlim = c(0, 1), ylim = c(0, k + 2),
       xlab = "Sensitivity", ylab = "", yaxt = "n",
       main = "Sensitivity", frame.plot = FALSE)

  for (i in 1:k) {
    y_pos <- k - i + 2

    # Calculate CI (Wilson score interval approximation)
    n_pos <- data$tp[i] + data$fn[i]
    sens <- study_data$sensitivity[i]
    se_sens <- sqrt(sens * (1 - sens) / n_pos)
    ci_lower <- max(0, sens - 1.96 * se_sens)
    ci_upper <- min(1, sens + 1.96 * se_sens)

    # Plot
    segments(ci_lower, y_pos, ci_upper, y_pos, lwd = 1.5)
    points(sens, y_pos, pch = 15, cex = 1.5)

    # Label
    axis(2, at = y_pos, labels = study_data$study[i], las = 1, tick = FALSE, cex.axis = 0.8)
  }

  # Add summary
  y_pos <- 1
  segments(dta_result$sens_ci_lower, y_pos, dta_result$sens_ci_upper, y_pos,
           lwd = 3, col = "blue")
  points(dta_result$summary_sens, y_pos, pch = 18, cex = 2.5, col = "blue")
  axis(2, at = y_pos, labels = "Summary", las = 1, tick = FALSE, font = 2)

  abline(h = 1.5, col = "gray50")

  # Specificity forest plot
  plot(NULL, xlim = c(0, 1), ylim = c(0, k + 2),
       xlab = "Specificity", ylab = "", yaxt = "n",
       main = "Specificity", frame.plot = FALSE)

  for (i in 1:k) {
    y_pos <- k - i + 2

    # Calculate CI
    n_neg <- data$tn[i] + data$fp[i]
    spec <- study_data$specificity[i]
    se_spec <- sqrt(spec * (1 - spec) / n_neg)
    ci_lower <- max(0, spec - 1.96 * se_spec)
    ci_upper <- min(1, spec + 1.96 * se_spec)

    # Plot
    segments(ci_lower, y_pos, ci_upper, y_pos, lwd = 1.5)
    points(spec, y_pos, pch = 15, cex = 1.5)
  }

  # Add summary
  y_pos <- 1
  segments(dta_result$spec_ci_lower, y_pos, dta_result$spec_ci_upper, y_pos,
           lwd = 3, col = "blue")
  points(dta_result$summary_spec, y_pos, pch = 18, cex = 2.5, col = "blue")

  abline(h = 1.5, col = "gray50")

  # Add overall title
  mtext(main, side = 3, line = -1.5, outer = TRUE, cex = 1.3, font = 2)

  invisible(NULL)
}


#' Calculate Diagnostic Likelihood Ratios
#'
#' Calculates positive and negative likelihood ratios for diagnostic test accuracy.
#'
#' @param sensitivity Sensitivity (true positive rate)
#' @param specificity Specificity (true negative rate)
#'
#' @return List containing PLR and NLR
#'
#' @export
#' @examples
#' likelihood_ratios(sensitivity = 0.90, specificity = 0.85)
likelihood_ratios <- function(sensitivity, specificity) {
  # Positive likelihood ratio
  plr <- sensitivity / (1 - specificity)

  # Negative likelihood ratio
  nlr <- (1 - sensitivity) / specificity

  return(list(
    PLR = plr,
    NLR = nlr,
    interpretation = list(
      PLR = if (plr > 10) "Large increase in probability" else
            if (plr > 5) "Moderate increase" else
            if (plr > 2) "Small increase" else "Minimal change",
      NLR = if (nlr < 0.1) "Large decrease in probability" else
            if (nlr < 0.2) "Moderate decrease" else
            if (nlr < 0.5) "Small decrease" else "Minimal change"
    )
  ))
}


#' Calculate Diagnostic Odds Ratio
#'
#' Calculates the diagnostic odds ratio from sensitivity and specificity or
#' from a 2x2 table.
#'
#' @param sensitivity Sensitivity (optional if using tp, fp, fn, tn)
#' @param specificity Specificity (optional if using tp, fp, fn, tn)
#' @param tp True positives (optional if using sensitivity/specificity)
#' @param fp False positives (optional)
#' @param fn False negatives (optional)
#' @param tn True negatives (optional)
#'
#' @return Diagnostic odds ratio
#'
#' @export
diagnostic_or <- function(sensitivity = NULL, specificity = NULL,
                          tp = NULL, fp = NULL, fn = NULL, tn = NULL) {
  if (!is.null(sensitivity) && !is.null(specificity)) {
    # Calculate from sens/spec
    dor <- (sensitivity / (1 - sensitivity)) / ((1 - specificity) / specificity)
  } else if (!is.null(tp) && !is.null(fp) && !is.null(fn) && !is.null(tn)) {
    # Calculate from 2x2 table
    dor <- (tp * tn) / (fp * fn)
  } else {
    stop("Must provide either (sensitivity, specificity) or (tp, fp, fn, tn)")
  }

  return(dor)
}
