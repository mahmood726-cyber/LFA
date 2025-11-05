#' Visualization Functions for Meta-Analysis
#'
#' @name visualizations
NULL

#' Enhanced Forest Plot
#'
#' Creates an enhanced forest plot for meta-analysis results using base graphics
#'
#' @param data Data frame with meta-analysis data
#' @param result Optional cbamm result object to display pooled estimate
#' @param title Plot title (optional)
#' @param xlab X-axis label (default: "Effect Size")
#' @param order_by Variable to order studies by (optional)
#' @param show_weights Logical indicating whether to show study weights (default: TRUE)
#' @param digits Number of digits for displaying estimates (default: 2)
#'
#' @return Invisibly returns NULL (creates plot as side effect)
#' @export
forest_plot_enhanced <- function(data,
                                   result = NULL,
                                   title = NULL,
                                   xlab = "Effect Size",
                                   order_by = NULL,
                                   show_weights = TRUE,
                                   digits = 2) {
  # Validate data
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Compute confidence intervals
  if (!"ci_lower" %in% names(data)) {
    data$ci_lower <- data$effect - 1.96 * data$se
  }
  if (!"ci_upper" %in% names(data)) {
    data$ci_upper <- data$effect + 1.96 * data$se
  }

  # Order studies if requested
  if (!is.null(order_by) && order_by %in% names(data)) {
    data <- data[order(data[[order_by]]), ]
  }

  k <- nrow(data)

  # Get study labels
  if ("study" %in% names(data)) {
    labels <- as.character(data$study)
  } else {
    labels <- paste0("Study ", 1:k)
  }

  # Set up plot area
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Calculate plot dimensions
  n_rows <- k
  if (!is.null(result)) n_rows <- n_rows + 2  # Add space for pooled estimate

  par(mar = c(5, 10, 4, 2))

  # Determine x-axis range
  all_lower <- c(data$ci_lower, if (!is.null(result)) result$ci_lower else NULL)
  all_upper <- c(data$ci_upper, if (!is.null(result)) result$ci_upper else NULL)
  xlim <- range(c(all_lower, all_upper), na.rm = TRUE)
  xlim <- xlim + c(-0.1, 0.1) * diff(xlim)

  # Create empty plot
  plot(NULL, xlim = xlim, ylim = c(0, n_rows + 1),
       xlab = xlab, ylab = "", yaxt = "n", main = title,
       frame.plot = FALSE)

  # Add vertical line at null effect
  abline(v = 0, lty = 2, col = "gray50")

  # Plot individual studies
  for (i in 1:k) {
    y_pos <- n_rows - i + 1

    # Plot CI line
    segments(data$ci_lower[i], y_pos, data$ci_upper[i], y_pos, lwd = 1.5)

    # Plot point estimate (size proportional to inverse variance)
    point_size <- 1 / data$se[i]
    point_size <- point_size / max(point_size, na.rm = TRUE) * 2 + 0.5
    points(data$effect[i], y_pos, pch = 15, cex = point_size)

    # Add study label
    axis(2, at = y_pos, labels = labels[i], las = 1, tick = FALSE, line = -0.5)

    # Add effect estimate text
    effect_text <- sprintf("%.*f [%.*f, %.*f]",
                           digits, data$effect[i],
                           digits, data$ci_lower[i],
                           digits, data$ci_upper[i])
    text(xlim[2], y_pos, effect_text, pos = 4, xpd = TRUE)

    # Add weight if requested
    if (show_weights && !is.null(result)) {
      weight_pct <- round(result$weights[i] * 100, 1)
      text(xlim[1], y_pos, sprintf("%.*f%%", 1, weight_pct), pos = 2, xpd = TRUE)
    }
  }

  # Add pooled estimate if provided
  if (!is.null(result)) {
    y_pos <- 1

    # Draw horizontal line
    segments(xlim[1], y_pos + 0.5, xlim[2], y_pos + 0.5, col = "gray50")

    # Plot pooled CI
    segments(result$ci_lower, y_pos, result$ci_upper, y_pos, lwd = 3, col = "blue")

    # Plot pooled point estimate
    points(result$estimate, y_pos, pch = 18, cex = 2.5, col = "blue")

    # Add label
    axis(2, at = y_pos, labels = "Pooled", las = 1, tick = FALSE, line = -0.5, font = 2)

    # Add pooled estimate text
    pooled_text <- sprintf("%.*f [%.*f, %.*f]",
                           digits, result$estimate,
                           digits, result$ci_lower,
                           digits, result$ci_upper)
    text(xlim[2], y_pos, pooled_text, pos = 4, xpd = TRUE, font = 2, col = "blue")
  }

  invisible(NULL)
}


#' Create Cumulative Meta-Analysis Dashboard
#'
#' Creates a comprehensive dashboard visualizing cumulative meta-analysis results
#'
#' @param cumulative_result A cbamm_cumulative object
#' @param main Title for the dashboard (optional)
#'
#' @return Invisibly returns NULL (creates plot as side effect)
#' @export
create_cumulative_dashboard <- function(cumulative_result, main = NULL) {
  if (!inherits(cumulative_result, "cbamm_cumulative")) {
    stop("Input must be a cbamm_cumulative object")
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Set up 2x2 plot layout
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  cum_res <- cumulative_result$cumulative_results
  k_values <- cum_res$k

  # Panel 1: Cumulative effect size with confidence intervals
  plot(k_values, cum_res$estimate, type = "n",
       xlab = "Number of Studies (k)", ylab = "Effect Size",
       main = "Cumulative Effect Estimate",
       ylim = range(c(cum_res$ci_lower, cum_res$ci_upper)))

  # Add confidence band
  polygon(c(k_values, rev(k_values)),
          c(cum_res$ci_lower, rev(cum_res$ci_upper)),
          col = rgb(0, 0, 1, 0.2), border = NA)

  # Add estimate line
  lines(k_values, cum_res$estimate, lwd = 2, col = "blue")

  # Add null line
  abline(h = 0, lty = 2, col = "gray50")

  # Mark stability point if present
  if (!is.na(cumulative_result$stability_index)) {
    abline(v = cumulative_result$stability_index + 1, lty = 2, col = "red")
    text(cumulative_result$stability_index + 1, par("usr")[4],
         "Stability", pos = 3, col = "red", xpd = TRUE)
  }

  # Panel 2: I-squared over time
  plot(k_values, cum_res$I2, type = "l", lwd = 2, col = "darkgreen",
       xlab = "Number of Studies (k)", ylab = "I² (%)",
       main = "Heterogeneity Over Time",
       ylim = c(0, max(100, max(cum_res$I2))))
  abline(h = c(25, 50, 75), lty = 3, col = "gray70")
  text(par("usr")[1], c(25, 50, 75), c("Low", "Moderate", "High"),
       pos = 4, cex = 0.8, col = "gray50")

  # Panel 3: Precision over time (1/SE)
  precision <- 1 / cum_res$se
  plot(k_values, precision, type = "l", lwd = 2, col = "purple",
       xlab = "Number of Studies (k)", ylab = "Precision (1/SE)",
       main = "Precision Accumulation")

  # Panel 4: Width of confidence interval
  ci_width <- cum_res$ci_upper - cum_res$ci_lower
  plot(k_values, ci_width, type = "l", lwd = 2, col = "orange",
       xlab = "Number of Studies (k)", ylab = "CI Width",
       main = "Confidence Interval Width")

  # Add overall title if provided
  if (!is.null(main)) {
    mtext(main, side = 3, line = -1.5, outer = TRUE, cex = 1.3, font = 2)
  }

  invisible(NULL)
}


#' Funnel Plot
#'
#' Creates a funnel plot for assessing publication bias
#'
#' @param data Data frame with effect and se columns
#' @param result Optional cbamm result object
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#' @export
funnel_plot <- function(data, result = NULL, main = "Funnel Plot") {
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Calculate precision
  precision <- 1 / data$se

  # Get pooled estimate if provided
  pooled_estimate <- if (!is.null(result)) result$estimate else mean(data$effect)

  # Create plot
  plot(data$effect, precision,
       xlab = "Effect Size", ylab = "Precision (1/SE)",
       main = main, pch = 19, col = rgb(0, 0, 0, 0.5))

  # Add vertical line at pooled estimate
  abline(v = pooled_estimate, lty = 2, col = "blue", lwd = 2)

  # Add pseudo 95% confidence limits
  max_precision <- max(precision)
  se_range <- seq(0, max(data$se), length.out = 100)
  prec_range <- 1 / se_range

  # Upper and lower funnel limits
  upper_limit <- pooled_estimate + 1.96 * se_range
  lower_limit <- pooled_estimate - 1.96 * se_range

  lines(upper_limit, prec_range, lty = 3, col = "gray50")
  lines(lower_limit, prec_range, lty = 3, col = "gray50")

  invisible(NULL)
}
