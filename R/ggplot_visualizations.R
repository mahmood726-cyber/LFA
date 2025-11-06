#' Modern ggplot2-Based Visualizations
#'
#' Publication-quality visualizations using ggplot2 for meta-analysis results.
#' Includes interactive features and modern aesthetics.
#'
#' @name ggplot_visualizations
NULL

#' Modern Forest Plot using ggplot2
#'
#' Creates a publication-quality forest plot with ggplot2, featuring clean
#' design, customizable colors, and optional prediction intervals.
#'
#' @param data Data frame with study, effect, se (and optional: year, quality)
#' @param result Optional meta-analysis result to add pooled estimate
#' @param show_weights Show study weights as point sizes (default: TRUE)
#' @param show_prediction_interval Show prediction interval (default: FALSE)
#' @param color_by Variable to color points by (e.g., "year", "quality")
#' @param title Plot title
#'
#' @return ggplot2 object
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- gg_forest_plot(data, result = ma_result, show_weights = TRUE)
#' print(p)
#' ggsave("forest_plot.pdf", p, width = 10, height = 8)
#' }
gg_forest_plot <- function(data, result = NULL, show_weights = TRUE,
                          show_prediction_interval = FALSE, color_by = NULL,
                          title = "Forest Plot") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Install with: install.packages('ggplot2')")
  }

  # Prepare data
  plot_data <- data
  plot_data$ci_lower <- plot_data$effect - 1.96 * plot_data$se
  plot_data$ci_upper <- plot_data$effect + 1.96 * plot_data$se

  # Calculate weights if showing
  if (show_weights) {
    plot_data$weight <- 1 / plot_data$se^2
    plot_data$weight <- 100 * plot_data$weight / sum(plot_data$weight)
  }

  # Reorder studies by effect size
  plot_data$study <- factor(plot_data$study,
                            levels = plot_data$study[order(plot_data$effect)])

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = effect, y = study))

  # Add confidence intervals
  p <- p + ggplot2::geom_segment(
    ggplot2::aes(x = ci_lower, xend = ci_upper, y = study, yend = study),
    size = 1, color = "gray40"
  )

  # Add point estimates
  if (show_weights) {
    if (!is.null(color_by) && color_by %in% names(plot_data)) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(size = weight, color = .data[[color_by]]),
        alpha = 0.7
      )
    } else {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(size = weight),
        color = "#2166AC", alpha = 0.7
      )
    }
    p <- p + ggplot2::scale_size_continuous(
      range = c(2, 8),
      name = "Weight (%)"
    )
  } else {
    if (!is.null(color_by) && color_by %in% names(plot_data)) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = .data[[color_by]]),
        size = 3, alpha = 0.7
      )
    } else {
      p <- p + ggplot2::geom_point(size = 3, color = "#2166AC", alpha = 0.7)
    }
  }

  # Add pooled estimate if provided
  if (!is.null(result)) {
    pooled_data <- data.frame(
      study = "Pooled",
      effect = result$estimate,
      ci_lower = result$ci_lower,
      ci_upper = result$ci_upper
    )

    p <- p + ggplot2::geom_segment(
      data = pooled_data,
      ggplot2::aes(x = ci_lower, xend = ci_upper, y = study, yend = study),
      size = 2, color = "#D6604D"
    )

    p <- p + ggplot2::geom_point(
      data = pooled_data,
      ggplot2::aes(x = effect, y = study),
      size = 5, shape = 18, color = "#D6604D"
    )

    # Add prediction interval if requested
    if (show_prediction_interval && !is.null(result$pi_lower)) {
      pi_data <- data.frame(
        study = "Prediction Interval",
        ci_lower = result$pi_lower,
        ci_upper = result$pi_upper
      )

      p <- p + ggplot2::geom_segment(
        data = pi_data,
        ggplot2::aes(x = ci_lower, xend = ci_upper, y = study, yend = study),
        size = 1.5, color = "#F4A582", linetype = "dashed"
      )
    }
  }

  # Add null line
  p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                               color = "red", alpha = 0.5)

  # Customize theme
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  # Labels
  p <- p + ggplot2::labs(
    title = title,
    x = "Effect Size (95% CI)",
    y = NULL
  )

  return(p)
}

#' Modern Funnel Plot with Contours
#'
#' Creates an enhanced funnel plot with significance contours and trim-and-fill.
#'
#' @param data Data frame with study, effect, and se columns
#' @param result Optional meta-analysis result
#' @param show_contours Show significance contours (default: TRUE)
#' @param show_trim_fill Show imputed studies from trim-and-fill (default: FALSE)
#' @param title Plot title
#'
#' @return ggplot2 object
#'
#' @export
gg_funnel_plot <- function(data, result = NULL, show_contours = TRUE,
                          show_trim_fill = FALSE, title = "Funnel Plot") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  # Prepare data
  plot_data <- data.frame(
    effect = data$effect,
    se = data$se,
    precision = 1 / data$se,
    type = "Observed"
  )

  # Create base plot (inverted: precision on y-axis)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = effect, y = precision))

  # Add significance contours if requested
  if (show_contours && !is.null(result)) {
    se_range <- seq(min(plot_data$se), max(plot_data$se), length.out = 100)
    precision_range <- 1 / se_range

    # 95% significance contours
    contour_data <- data.frame(
      precision = rep(precision_range, 2),
      effect_lower = result$estimate - 1.96 * se_range,
      effect_upper = result$estimate + 1.96 * se_range
    )

    p <- p + ggplot2::geom_ribbon(
      data = contour_data,
      ggplot2::aes(y = precision, xmin = effect_lower, xmax = effect_upper),
      fill = "#E0E0E0", alpha = 0.5, inherit.aes = FALSE
    )

    # 99% contours
    contour_data_99 <- data.frame(
      precision = rep(precision_range, 2),
      effect_lower = result$estimate - 2.58 * se_range,
      effect_upper = result$estimate + 2.58 * se_range
    )

    p <- p + ggplot2::geom_ribbon(
      data = contour_data_99,
      ggplot2::aes(y = precision, xmin = effect_lower, xmax = effect_upper),
      fill = "#F5F5F5", alpha = 0.5, inherit.aes = FALSE
    )
  }

  # Add pooled estimate line
  if (!is.null(result)) {
    p <- p + ggplot2::geom_vline(
      xintercept = result$estimate,
      linetype = "solid", color = "#2166AC", size = 1
    )
  }

  # Add null line
  p <- p + ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed", color = "red", alpha = 0.5
  )

  # Add observed studies
  p <- p + ggplot2::geom_point(
    ggplot2::aes(color = type),
    size = 3, alpha = 0.7
  )

  # Add trim-and-fill imputed studies if requested
  if (show_trim_fill) {
    # Simple trim-and-fill implementation
    # In practice, would use more sophisticated algorithm
    asymmetry <- sum(plot_data$effect * plot_data$precision) / sum(plot_data$precision)

    if (abs(asymmetry) > 0.1) {
      # Estimate missing studies (simplified)
      n_missing <- max(1, floor(nrow(plot_data) * 0.2))
      imputed_effects <- 2 * result$estimate - plot_data$effect[1:n_missing]
      imputed_data <- data.frame(
        effect = imputed_effects,
        se = plot_data$se[1:n_missing],
        precision = plot_data$precision[1:n_missing],
        type = "Imputed"
      )

      p <- p + ggplot2::geom_point(
        data = imputed_data,
        ggplot2::aes(x = effect, y = precision, color = type),
        size = 3, alpha = 0.7, shape = 17
      )

      plot_data <- rbind(plot_data, imputed_data)
    }
  }

  # Customize colors
  p <- p + ggplot2::scale_color_manual(
    values = c("Observed" = "#2166AC", "Imputed" = "#B2182B"),
    name = "Study Type"
  )

  # Theme
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )

  # Labels
  p <- p + ggplot2::labs(
    title = title,
    x = "Effect Size",
    y = "Precision (1/SE)"
  )

  return(p)
}

#' L'Abbé Plot for Binary Outcomes
#'
#' Creates a L'Abbé plot showing event rates in treatment vs control groups.
#'
#' @param data Data frame with study, events1, n1, events2, n2
#' @param show_identity Add identity line (default: TRUE)
#' @param show_contours Add OR contour lines (default: TRUE)
#'
#' @return ggplot2 object
#'
#' @export
gg_labbe_plot <- function(data, show_identity = TRUE, show_contours = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  # Calculate proportions
  plot_data <- data.frame(
    study = data$study,
    prop_control = data$events2 / data$n2,
    prop_treatment = data$events1 / data$n1,
    size = sqrt(data$n1 + data$n2)
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = prop_control, y = prop_treatment))

  # Add contour lines for OR if requested
  if (show_contours) {
    # OR = 0.5, 1, 2 lines
    x_seq <- seq(0.01, 0.99, length.out = 100)

    for (or in c(0.5, 2)) {
      y_or <- (or * x_seq) / (1 + x_seq * (or - 1))
      contour_data <- data.frame(x = x_seq, y = y_or, or = or)
      p <- p + ggplot2::geom_line(
        data = contour_data,
        ggplot2::aes(x = x, y = y),
        color = "gray70", linetype = "dashed",
        inherit.aes = FALSE
      )
    }
  }

  # Add identity line
  if (show_identity) {
    p <- p + ggplot2::geom_abline(
      intercept = 0, slope = 1,
      color = "red", linetype = "dashed"
    )
  }

  # Add points
  p <- p + ggplot2::geom_point(
    ggplot2::aes(size = size),
    color = "#2166AC", alpha = 0.6
  )

  p <- p + ggplot2::scale_size_continuous(
    range = c(2, 10),
    name = "Sample Size"
  )

  # Theme and labels
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "L'Abbé Plot",
      x = "Event Rate (Control)",
      y = "Event Rate (Treatment)"
    ) +
    ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1))

  return(p)
}

#' Cumulative Meta-Analysis Plot
#'
#' Visualizes how the pooled effect estimate evolves as studies are added.
#'
#' @param cumulative_result Result from cumulative_meta_analysis()
#' @param color_by_significance Color points by statistical significance
#'
#' @return ggplot2 object
#'
#' @export
gg_cumulative_plot <- function(cumulative_result, color_by_significance = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  # Extract data
  if (inherits(cumulative_result, "cumulative_ma")) {
    plot_data <- cumulative_result$cumulative_results
  } else {
    plot_data <- cumulative_result
  }

  # Add significance indicator
  plot_data$significant <- abs(plot_data$estimate) > 1.96 * plot_data$se

  # Create plot
  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = cumulative_n, y = estimate))

  # Add confidence interval ribbon
  p <- p + ggplot2::geom_ribbon(
    ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#92C5DE", alpha = 0.3
  )

  # Add line
  p <- p + ggplot2::geom_line(color = "#2166AC", size = 1)

  # Add points
  if (color_by_significance) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = significant),
      size = 3
    ) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#D6604D", "FALSE" = "#4393C3"),
        labels = c("Not Significant", "Significant"),
        name = "Significance"
      )
  } else {
    p <- p + ggplot2::geom_point(color = "#2166AC", size = 3)
  }

  # Add null line
  p <- p + ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed", color = "red", alpha = 0.5
  )

  # Theme and labels
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "Cumulative Meta-Analysis",
      x = "Cumulative Number of Studies",
      y = "Cumulative Effect Estimate (95% CI)"
    )

  return(p)
}

#' GOSH (Graphical Display of Study Heterogeneity) Plot
#'
#' Creates GOSH plot showing distribution of effect estimates across all
#' possible study combinations.
#'
#' @param data Data frame with study, effect, and se columns
#' @param n_subsets Number of random subsets to sample (default: 1000)
#' @param highlight_outliers Highlight potential outlier studies
#'
#' @return ggplot2 object
#'
#' @export
gg_gosh_plot <- function(data, n_subsets = 1000, highlight_outliers = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  n_studies <- nrow(data)

  if (n_studies > 10) {
    warning("GOSH plot with >10 studies: using random sampling of subsets")
  }

  # Generate random subsets
  subset_results <- matrix(0, n_subsets, 3)
  colnames(subset_results) <- c("estimate", "tau2", "I2")

  for (i in 1:n_subsets) {
    # Sample subset (at least 3 studies)
    subset_size <- sample(3:n_studies, 1)
    subset_idx <- sample(1:n_studies, subset_size)

    yi <- data$effect[subset_idx]
    vi <- data$se[subset_idx]^2

    # Meta-analysis
    wi <- 1 / vi
    Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
    df <- length(yi) - 1
    C <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - df) / C)

    wi_re <- 1 / (vi + tau2)
    estimate <- sum(wi_re * yi) / sum(wi_re)

    I2 <- max(0, 100 * (Q - df) / Q)

    subset_results[i, ] <- c(estimate, tau2, I2)
  }

  plot_data <- as.data.frame(subset_results)

  # Identify outliers if requested
  if (highlight_outliers) {
    # Studies that cause extreme estimates
    plot_data$outlier <- abs(plot_data$estimate - median(plot_data$estimate)) >
                         2 * mad(plot_data$estimate)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = estimate, y = I2))

  if (highlight_outliers) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = outlier),
      alpha = 0.5, size = 2
    ) +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "#4393C3", "TRUE" = "#D6604D"),
        labels = c("Typical", "Outlying"),
        name = "Subset Type"
      )
  } else {
    p <- p + ggplot2::geom_point(color = "#4393C3", alpha = 0.5, size = 2)
  }

  # Theme and labels
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "GOSH Plot - Study Heterogeneity",
      x = "Effect Estimate",
      y = expression(I^2 ~ "(%)")
    )

  return(p)
}

#' Meta-Regression Scatter Plot with Fitted Line
#'
#' Visualizes relationship between study-level moderators and effect sizes.
#'
#' @param data Data frame with effect, se, and moderator variable
#' @param moderator Name of moderator variable
#' @param show_bubble Use bubble plot with sizes proportional to precision
#'
#' @return ggplot2 object
#'
#' @export
gg_metareg_plot <- function(data, moderator, show_bubble = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required")
  }

  if (!moderator %in% names(data)) {
    stop(sprintf("Moderator '%s' not found in data", moderator))
  }

  # Calculate weights
  data$weight <- 1 / data$se^2
  data$weight_scaled <- 100 * data$weight / sum(data$weight)

  # Fit weighted regression
  fit <- lm(effect ~ data[[moderator]], data = data, weights = weight)
  data$fitted <- predict(fit)

  # Create plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[moderator]], y = effect))

  # Add regression line with confidence band
  p <- p + ggplot2::geom_smooth(
    method = "lm", formula = y ~ x,
    color = "#2166AC", fill = "#92C5DE", alpha = 0.2
  )

  # Add points
  if (show_bubble) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(size = weight_scaled),
      color = "#2166AC", alpha = 0.6
    ) +
      ggplot2::scale_size_continuous(
        range = c(2, 10),
        name = "Weight (%)"
      )
  } else {
    p <- p + ggplot2::geom_point(color = "#2166AC", size = 3, alpha = 0.6)
  }

  # Theme and labels
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = sprintf("Meta-Regression: %s", moderator),
      x = moderator,
      y = "Effect Size"
    )

  return(p)
}
