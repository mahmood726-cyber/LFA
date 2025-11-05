#' Advanced Visualization for Meta-Analysis
#'
#' State-of-the-art plotting functions based on recent methodological literature
#'
#' @name advanced_plots
NULL

#' Baujat Plot
#'
#' Creates a Baujat plot to identify studies contributing to heterogeneity
#' and/or having high influence on the pooled estimate. Studies in the
#' upper-right quadrant warrant particular attention.
#'
#' @param data Data frame with meta-analysis data
#' @param method Method for meta-analysis (default: "DL")
#' @param label_points Logical indicating whether to label points (default: TRUE)
#' @param label_threshold Threshold for labeling (default: NULL, labels all)
#' @param main Plot title
#'
#' @return Invisibly returns the Baujat data
#'
#' @references
#' Baujat B, Mahé C, Pignon JP, Hill C (2002). A graphical method for exploring
#' heterogeneity in meta-analyses. Statistics in Medicine, 21(18):2641-2652.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' baujat_plot(example_meta)
#' }
baujat_plot <- function(data, method = "DL", label_points = TRUE,
                        label_threshold = NULL, main = "Baujat Plot") {
  # Get Baujat data
  baujat_data <- baujat_plot_data(data, method = method)

  # Create plot
  plot(baujat_data$heterogeneity_contribution,
       baujat_data$influence,
       xlab = "Contribution to Overall Heterogeneity",
       ylab = "Influence on Pooled Estimate",
       main = main,
       pch = 19,
       col = rgb(0, 0, 1, 0.6),
       cex = 1.2)

  # Add grid
  grid(col = "gray80", lty = "dotted")

  # Add quadrant lines at medians
  abline(v = median(baujat_data$heterogeneity_contribution),
         lty = 2, col = "gray50")
  abline(h = median(baujat_data$influence),
         lty = 2, col = "gray50")

  # Label points if requested
  if (label_points) {
    if (is.null(label_threshold)) {
      # Label all points
      text(baujat_data$heterogeneity_contribution,
           baujat_data$influence,
           labels = baujat_data$study,
           pos = 3, cex = 0.7, col = "darkblue")
    } else {
      # Label only points exceeding threshold
      label_idx <- (baujat_data$heterogeneity_contribution > quantile(baujat_data$heterogeneity_contribution, label_threshold)) |
                   (baujat_data$influence > quantile(baujat_data$influence, label_threshold))

      text(baujat_data$heterogeneity_contribution[label_idx],
           baujat_data$influence[label_idx],
           labels = baujat_data$study[label_idx],
           pos = 3, cex = 0.7, col = "darkblue")
    }
  }

  # Add legend
  legend("topright",
         legend = "Studies in upper-right warrant investigation",
         bty = "n", cex = 0.8, text.col = "gray30")

  invisible(baujat_data)
}


#' Contour-Enhanced Funnel Plot
#'
#' Creates a contour-enhanced funnel plot showing regions of statistical
#' significance. Helps distinguish publication bias from other causes of
#' asymmetry.
#'
#' @param data Data frame with meta-analysis data
#' @param result Optional cbamm result object
#' @param contours Character vector of contour types: "p_value" (default),
#'   "none"
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @references
#' Peters JL, Sutton AJ, Jones DR, Abrams KR, Rushton L (2008). Contour-enhanced
#' meta-analysis funnel plots help distinguish publication bias from other causes
#' of asymmetry. Journal of Clinical Epidemiology, 61(10):991-996.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' contour_funnel_plot(example_meta)
#' }
contour_funnel_plot <- function(data, result = NULL, contours = "p_value",
                                 main = "Contour-Enhanced Funnel Plot") {
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Get or compute pooled estimate
  if (is.null(result)) {
    result <- cbamm_fast(data, verbose = FALSE)
  }

  pooled_estimate <- result$estimate
  yi <- data$effect
  sei <- data$se

  # Set up plot
  max_se <- max(sei) * 1.1
  xlim <- range(c(yi, pooled_estimate)) + c(-1, 1) * max_se * 3
  ylim <- c(max_se, 0)

  plot(yi, sei,
       xlim = xlim, ylim = ylim,
       xlab = "Effect Size",
       ylab = "Standard Error",
       main = main,
       pch = 19,
       col = rgb(0, 0, 0, 0.6),
       cex = 1.2)

  # Add contours if requested
  if ("p_value" %in% contours) {
    # Create sequence of SE values
    se_seq <- seq(0, max_se, length.out = 100)

    # Significance levels
    # p < 0.01 (2-tailed: z > 2.576)
    lines(pooled_estimate + 2.576 * se_seq, se_seq,
          lty = 1, col = "gray70", lwd = 1.5)
    lines(pooled_estimate - 2.576 * se_seq, se_seq,
          lty = 1, col = "gray70", lwd = 1.5)

    # p < 0.05 (2-tailed: z > 1.96)
    lines(pooled_estimate + 1.96 * se_seq, se_seq,
          lty = 2, col = "gray60", lwd = 1.5)
    lines(pooled_estimate - 1.96 * se_seq, se_seq,
          lty = 2, col = "gray60", lwd = 1.5)

    # p < 0.10 (2-tailed: z > 1.645)
    lines(pooled_estimate + 1.645 * se_seq, se_seq,
          lty = 3, col = "gray50", lwd = 1.5)
    lines(pooled_estimate - 1.645 * se_seq, se_seq,
          lty = 3, col = "gray50", lwd = 1.5)

    # Shade regions
    # Most significant (p < 0.01)
    polygon(c(pooled_estimate + 2.576 * se_seq, rev(pooled_estimate + 3 * max_se)),
            c(se_seq, rev(se_seq)),
            col = rgb(1, 0.9, 0.9, 0.3), border = NA)
    polygon(c(pooled_estimate - 2.576 * se_seq, rev(pooled_estimate - 3 * max_se)),
            c(se_seq, rev(se_seq)),
            col = rgb(1, 0.9, 0.9, 0.3), border = NA)

    # Add legend for significance regions
    legend("top",
           legend = c("p < 0.01", "p < 0.05", "p < 0.10"),
           lty = c(1, 2, 3),
           col = c("gray70", "gray60", "gray50"),
           bty = "n",
           horiz = TRUE,
           cex = 0.8)
  }

  # Add pooled estimate line
  abline(v = pooled_estimate, lty = 2, col = "blue", lwd = 2)

  # Add points on top
  points(yi, sei, pch = 19, col = rgb(0, 0, 0, 0.7), cex = 1.2)

  invisible(NULL)
}


#' Radial (Galbraith) Plot
#'
#' Creates a radial plot for assessing heterogeneity. Studies should scatter
#' around zero on the y-axis in the absence of heterogeneity.
#'
#' @param data Data frame with meta-analysis data
#' @param result Optional cbamm result object
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @references
#' Galbraith RF (1988). A note on graphical presentation of estimated odds
#' ratios from several clinical trials. Statistics in Medicine, 7(8):889-894.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' radial_plot(example_meta)
#' }
radial_plot <- function(data, result = NULL, main = "Radial Plot") {
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Get or compute pooled estimate
  if (is.null(result)) {
    result <- cbamm_fast(data, verbose = FALSE)
  }

  yi <- data$effect
  sei <- data$se

  # X-axis: 1/SE (precision)
  prec <- 1 / sei

  # Y-axis: (effect - pooled) / SE
  z <- (yi - result$estimate) / sei

  # Create plot
  plot(prec, z,
       xlab = "Inverse Standard Error (Precision)",
       ylab = "Standardized Effect",
       main = main,
       pch = 19,
       col = rgb(0, 0, 1, 0.6),
       cex = 1.2)

  # Add reference lines
  abline(h = 0, lty = 1, col = "blue", lwd = 2)
  abline(h = c(-1.96, 1.96), lty = 2, col = "red")

  # Add confidence region
  abline(a = 0, b = result$estimate, lty = 3, col = "gray50")

  # Add grid
  grid(col = "gray80", lty = "dotted")

  # Add legend
  legend("topright",
         legend = c("Expected (no heterogeneity)", "95% limits"),
         lty = c(1, 2),
         col = c("blue", "red"),
         bty = "n",
         cex = 0.8)

  invisible(NULL)
}


#' Leave-One-Out Forest Plot
#'
#' Creates a forest plot showing the pooled estimate when each study is
#' omitted. Useful for visualizing study influence.
#'
#' @param loo_result Object from leave_one_out() function
#' @param main Plot title
#' @param show_full_estimate Logical indicating whether to show full model estimate
#'
#' @return Invisibly returns NULL
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' loo_result <- leave_one_out(example_meta)
#' plot_leave_one_out(loo_result)
#' }
plot_leave_one_out <- function(loo_result, main = "Leave-One-Out Analysis",
                                show_full_estimate = TRUE) {
  if (!inherits(loo_result, "loo_analysis")) {
    stop("Input must be a loo_analysis object")
  }

  results <- loo_result$results
  full <- loo_result$full_model
  k <- nrow(results)

  # Set up plot
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(5, 12, 4, 2))

  # Determine x-axis range
  xlim <- range(c(results$ci_lower, results$ci_upper, full$ci_lower, full$ci_upper))
  xlim <- xlim + c(-0.1, 0.1) * diff(xlim)

  # Create empty plot
  n_rows <- k + if (show_full_estimate) 2 else 0
  plot(NULL, xlim = xlim, ylim = c(0, n_rows + 1),
       xlab = "Effect Size", ylab = "", yaxt = "n", main = main,
       frame.plot = FALSE)

  # Add vertical line at null
  abline(v = 0, lty = 3, col = "gray50")

  # Add full model estimate line
  if (show_full_estimate) {
    abline(v = full$estimate, lty = 2, col = "blue", lwd = 2)
  }

  # Plot leave-one-out results
  for (i in 1:k) {
    y_pos <- n_rows - i + 1

    # CI line
    segments(results$ci_lower[i], y_pos, results$ci_upper[i], y_pos, lwd = 1.5)

    # Point estimate
    points(results$estimate[i], y_pos, pch = 15, cex = 1.5,
           col = if (results$influence[i] > 1.96) "red" else "black")

    # Label
    label <- paste("Omit:", results$excluded_study[i])
    axis(2, at = y_pos, labels = label, las = 1, tick = FALSE, cex.axis = 0.8)
  }

  # Add full model estimate if requested
  if (show_full_estimate) {
    y_pos <- 1
    segments(xlim[1], y_pos + 0.5, xlim[2], y_pos + 0.5, col = "gray50")
    segments(full$ci_lower, y_pos, full$ci_upper, y_pos, lwd = 3, col = "blue")
    points(full$estimate, y_pos, pch = 18, cex = 2.5, col = "blue")
    axis(2, at = y_pos, labels = "Full Model", las = 1, tick = FALSE, font = 2)
  }

  # Add legend
  legend("topright",
         legend = c("Influential (influence > 1.96)", "Non-influential"),
         pch = 15,
         col = c("red", "black"),
         bty = "n",
         cex = 0.8)

  invisible(NULL)
}


#' Influence Diagnostic Plots
#'
#' Creates a multi-panel plot showing various influence diagnostics:
#' standardized residuals, Cook's distance, DFBETAS, and hat values.
#'
#' @param influence_result Object from influence_diagnostics() function
#' @param main Overall plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' influence_result <- influence_diagnostics(example_meta)
#' plot_influence_diagnostics(influence_result)
#' }
plot_influence_diagnostics <- function(influence_result,
                                        main = "Influence Diagnostics") {
  if (!inherits(influence_result, "influence_diagnostics")) {
    stop("Input must be an influence_diagnostics object")
  }

  diag <- influence_result$diagnostics
  k <- nrow(diag)

  # Set up 2x2 plot layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # Panel 1: Standardized residuals
  plot(1:k, diag$std_residual,
       xlab = "Study", ylab = "Standardized Residual",
       main = "Standardized Residuals",
       pch = 19,
       col = ifelse(diag$is_outlier, "red", "black"))
  abline(h = c(-1.96, 0, 1.96), lty = c(2, 1, 2), col = c("red", "gray50", "red"))
  grid(col = "gray80", lty = "dotted")

  # Panel 2: Cook's distance
  barplot(diag$cooks_d,
          names.arg = 1:k,
          xlab = "Study",
          ylab = "Cook's Distance",
          main = "Cook's Distance",
          col = ifelse(diag$cooks_d > influence_result$thresholds$cooks_d, "red", "steelblue"))
  abline(h = influence_result$thresholds$cooks_d, lty = 2, col = "red")

  # Panel 3: DFBETAS
  plot(1:k, diag$dfbetas,
       xlab = "Study", ylab = "DFBETAS",
       main = "DFBETAS",
       pch = 19,
       col = ifelse(abs(diag$dfbetas) > influence_result$thresholds$dfbetas, "red", "black"))
  abline(h = c(-influence_result$thresholds$dfbetas, 0, influence_result$thresholds$dfbetas),
         lty = c(2, 1, 2),
         col = c("red", "gray50", "red"))
  grid(col = "gray80", lty = "dotted")

  # Panel 4: Hat values (leverage)
  plot(1:k, diag$hat,
       xlab = "Study", ylab = "Hat Value (Leverage)",
       main = "Leverage",
       pch = 19,
       col = "steelblue",
       type = "h",
       lwd = 3)
  grid(col = "gray80", lty = "dotted")

  # Add overall title
  mtext(main, side = 3, line = -1.5, outer = TRUE, cex = 1.3, font = 2)

  invisible(NULL)
}


#' L'Abbé Plot for Binary Outcomes
#'
#' Creates an L'Abbé plot for visualizing treatment effects in studies with
#' binary outcomes. Each point represents a study, plotted by control group
#' event rate (x-axis) vs treatment group event rate (y-axis).
#'
#' @param data Data frame with columns: events_treatment, n_treatment,
#'   events_control, n_control
#' @param add_diagonal Logical indicating whether to add diagonal reference line
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
labbe_plot <- function(data, add_diagonal = TRUE,
                       main = "L'Abbé Plot") {
  # Validate columns
  required_cols <- c("events_treatment", "n_treatment", "events_control", "n_control")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Calculate event rates
  rate_treatment <- data$events_treatment / data$n_treatment
  rate_control <- data$events_control / data$n_control

  # Create plot
  plot(rate_control, rate_treatment,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Control Group Event Rate",
       ylab = "Treatment Group Event Rate",
       main = main,
       pch = 19,
       col = rgb(0, 0, 1, 0.6),
       cex = 1.5)

  # Add diagonal (no treatment effect)
  if (add_diagonal) {
    abline(a = 0, b = 1, lty = 2, col = "red", lwd = 2)
  }

  # Add grid
  grid(col = "gray80", lty = "dotted")

  # Add legend
  legend("topleft",
         legend = c("Studies", "No effect (diagonal)"),
         pch = c(19, NA),
         lty = c(NA, 2),
         col = c(rgb(0, 0, 1, 0.6), "red"),
         bty = "n")

  invisible(NULL)
}
