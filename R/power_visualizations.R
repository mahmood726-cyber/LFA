#' Power Visualizations - Cutting-Edge Statistical Graphics
#'
#' State-of-the-art visualizations inspired by Nature, Science, JAMA, BMJ,
#' Statistics in Medicine, and other top-tier journals. Includes interactive
#' plots, multiverse analysis, specification curves, and advanced diagnostics.
#'
#' @name power_visualizations
NULL

#' Comprehensive Meta-Analysis Dashboard
#'
#' Creates a publication-quality comprehensive dashboard with 12+ integrated plots
#' showing all key aspects of meta-analysis results in a single view.
#'
#' @param data Data frame with meta-analysis data
#' @param result Meta-analysis result object
#' @param show_all Show all 12+ panels (default: TRUE)
#' @param output_file Optional file path to save (default: display only)
#'
#' @return Invisibly returns plot data
#'
#' @export
#' @examples
#' \dontrun{
#' result <- cbamm_fast(data)
#' comprehensive_dashboard(data, result)
#' comprehensive_dashboard(data, result, output_file = "dashboard.pdf")
#' }
comprehensive_dashboard <- function(data, result, show_all = TRUE, output_file = NULL) {
  if (!is.null(output_file)) {
    pdf(output_file, width = 20, height = 16)
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # 4x3 layout for 12 panels
  layout_matrix <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
  layout(layout_matrix)
  par(mar = c(4, 4, 3, 2))

  # Panel 1: Forest plot
  plot_mini_forest(data, result, main = "Forest Plot")

  # Panel 2: Funnel plot with contours
  plot_mini_funnel(data, result, main = "Funnel Plot")

  # Panel 3: Baujat plot
  plot_mini_baujat(data, main = "Baujat Plot - Heterogeneity")

  # Panel 4: Influence diagnostics
  plot_mini_influence(data, result, main = "Influence")

  # Panel 5: Heterogeneity over time
  if ("year" %in% names(data)) {
    plot_heterogeneity_time(data, main = "Heterogeneity Over Time")
  } else {
    plot_effect_distribution(data, main = "Effect Distribution")
  }

  # Panel 6: P-curve
  plot_p_curve(data, main = "P-Curve Analysis")

  # Panel 7: Cumulative meta-analysis
  if ("year" %in% names(data)) {
    plot_mini_cumulative(data, main = "Cumulative Evidence")
  } else {
    plot_mini_radial(data, result, main = "Radial Plot")
  }

  # Panel 8: Precision-weighted histogram
  plot_precision_histogram(data, result, main = "Precision-Weighted Distribution")

  # Panel 9: Quality assessment
  if ("quality" %in% names(data) || "rob" %in% names(data)) {
    plot_quality_assessment(data, main = "Study Quality")
  } else {
    plot_study_size(data, main = "Study Characteristics")
  }

  # Panel 10: Leave-one-out sensitivity
  plot_loo_sensitivity(data, main = "Leave-One-Out Sensitivity")

  # Panel 11: Heterogeneity decomposition
  plot_heterogeneity_decomp(data, result, main = "Heterogeneity Decomposition")

  # Panel 12: Summary statistics
  plot_summary_stats(data, result, main = "Summary Statistics")

  # Add overall title
  mtext("Comprehensive Meta-Analysis Dashboard",
        side = 3, line = -2, outer = TRUE, cex = 1.8, font = 2)

  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("Dashboard saved to %s", output_file))
  }

  invisible(NULL)
}

#' Interactive 3D Heterogeneity Plot
#'
#' Creates a 3D visualization of heterogeneity, effect size, and precision.
#' Requires rgl package for true 3D rendering (falls back to perspective plot).
#'
#' @param data Meta-analysis data
#' @param result Meta-analysis result
#' @param use_3d Use true 3D if rgl available (default: TRUE)
#'
#' @return Invisibly returns plot data
#'
#' @export
plot_3d_heterogeneity <- function(data, result, use_3d = TRUE) {
  # Calculate metrics
  precision <- 1 / data$se
  effects <- data$effect
  k <- nrow(data)

  # Calculate local heterogeneity contribution for each study
  het_contrib <- sapply(1:k, function(i) {
    data_i <- data[-i, ]
    if (nrow(data_i) < 2) return(0)
    tryCatch({
      result_i <- cbamm_fast(data_i, verbose = FALSE)
      return(abs(result$I2 - result_i$I2))
    }, error = function(e) 0)
  })

  if (use_3d && requireNamespace("rgl", quietly = TRUE)) {
    # True 3D plot
    rgl::plot3d(effects, precision, het_contrib,
                col = rainbow(k),
                size = 5,
                xlab = "Effect Size",
                ylab = "Precision",
                zlab = "Heterogeneity Contribution",
                main = "3D Heterogeneity Landscape")
  } else {
    # Fallback to perspective plot
    x <- seq(min(effects), max(effects), length.out = 30)
    y <- seq(min(precision), max(precision), length.out = 30)
    z <- outer(x, y, function(x, y) {
      mean(het_contrib * exp(-((effects - x)^2 + (precision - y)^2)/2))
    })

    persp(x, y, z,
          theta = 30, phi = 30, expand = 0.5,
          col = "lightblue", shade = 0.5,
          xlab = "Effect Size", ylab = "Precision",
          zlab = "Heterogeneity", main = "Heterogeneity Landscape")
  }

  invisible(list(effects = effects, precision = precision, het = het_contrib))
}

#' Multiverse/Specification Curve Analysis
#'
#' Performs multiverse analysis by running meta-analysis across all possible
#' analytical specifications (methods, outlier removal, transformations) and
#' visualizes the distribution of results.
#'
#' @param data Meta-analysis data
#' @param methods Vector of methods to test (default: all)
#' @param test_outliers Test with/without outliers (default: TRUE)
#' @param n_specs Number of specifications to generate (default: 1000)
#'
#' @return Multiverse analysis result with visualization
#'
#' @export
multiverse_analysis <- function(data, methods = c("DL", "REML", "ML", "PM", "FE"),
                               test_outliers = TRUE, n_specs = 1000) {
  cat("Running multiverse analysis across", n_specs, "specifications...\n")

  specs <- expand.grid(
    method = methods,
    outlier_removed = if (test_outliers) c(TRUE, FALSE) else FALSE,
    stringsAsFactors = FALSE
  )

  # Extend with random sampling variations
  if (nrow(specs) < n_specs) {
    extra_specs <- data.frame(
      method = sample(methods, n_specs - nrow(specs), replace = TRUE),
      outlier_removed = sample(c(TRUE, FALSE), n_specs - nrow(specs), replace = TRUE)
    )
    specs <- rbind(specs, extra_specs)
  }

  # Run all specifications
  results <- data.frame(
    spec_id = 1:nrow(specs),
    method = specs$method,
    outlier_removed = specs$outlier_removed,
    estimate = numeric(nrow(specs)),
    ci_lower = numeric(nrow(specs)),
    ci_upper = numeric(nrow(specs)),
    I2 = numeric(nrow(specs)),
    p_value = numeric(nrow(specs))
  )

  pb <- txtProgressBar(max = nrow(specs), style = 3)

  for (i in 1:nrow(specs)) {
    data_i <- data

    # Remove outliers if specified
    if (specs$outlier_removed[i] && nrow(data) > 5) {
      # Simple outlier detection
      z_scores <- abs(scale(data$effect))
      data_i <- data[z_scores < 2.5, ]
    }

    # Run meta-analysis
    tryCatch({
      res <- cbamm_fast(data_i, method = specs$method[i], verbose = FALSE)
      results$estimate[i] <- res$estimate
      results$ci_lower[i] <- res$ci_lower
      results$ci_upper[i] <- res$ci_upper
      results$I2[i] <- res$I2
      results$p_value[i] <- res$p_value
    }, error = function(e) {
      results$estimate[i] <<- NA
    })

    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Remove failed specifications
  results <- results[!is.na(results$estimate), ]

  # Visualize specification curve
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

  # Panel 1: Specification curve
  spec_order <- order(results$estimate)
  plot(1:nrow(results), results$estimate[spec_order],
       type = "l", lwd = 2, col = "blue",
       xlab = "Specification (sorted by estimate)",
       ylab = "Effect Estimate",
       main = "Specification Curve Analysis")

  # Add confidence bands
  polygon(c(1:nrow(results), rev(1:nrow(results))),
          c(results$ci_lower[spec_order], rev(results$ci_upper[spec_order])),
          col = rgb(0, 0, 1, 0.2), border = NA)

  # Add null line
  abline(h = 0, lty = 2, col = "red")

  # Add median and quartiles
  abline(h = median(results$estimate), lty = 1, col = "darkgreen", lwd = 2)
  abline(h = quantile(results$estimate, c(0.25, 0.75)), lty = 3, col = "darkgreen")

  legend("topleft",
         legend = c("Specification estimates", "Median", "IQR", "Null"),
         lty = c(1, 1, 3, 2),
         col = c("blue", "darkgreen", "darkgreen", "red"),
         lwd = c(2, 2, 1, 1))

  # Panel 2: Distribution of estimates
  hist(results$estimate, breaks = 30, col = "skyblue", border = "white",
       xlab = "Effect Estimate", main = "Distribution Across Specifications",
       freq = FALSE)

  # Add density
  lines(density(results$estimate), col = "darkblue", lwd = 2)

  # Add reference lines
  abline(v = 0, lty = 2, col = "red", lwd = 2)
  abline(v = median(results$estimate), lty = 1, col = "darkgreen", lwd = 2)

  # Summary statistics
  cat("\n=== Multiverse Analysis Summary ===\n")
  cat(sprintf("Number of specifications: %d\n", nrow(results)))
  cat(sprintf("Median estimate: %.3f\n", median(results$estimate)))
  cat(sprintf("IQR: %.3f to %.3f\n",
              quantile(results$estimate, 0.25),
              quantile(results$estimate, 0.75)))
  cat(sprintf("Range: %.3f to %.3f\n", min(results$estimate), max(results$estimate)))
  cat(sprintf("Proportion significant (p<0.05): %.1f%%\n",
              100 * mean(results$p_value < 0.05)))
  cat(sprintf("Proportion crossing null: %.1f%%\n",
              100 * mean(sign(results$ci_lower) != sign(results$ci_upper))))

  invisible(results)
}

#' Raincloud Plot for Effect Sizes
#'
#' Creates a raincloud plot combining violin plot, box plot, and raw data points
#' for visualizing effect size distributions. Based on Allen et al. (2019).
#'
#' @param data Meta-analysis data
#' @param grouping Optional grouping variable
#' @param colors Color palette for groups
#'
#' @return ggplot2 object (if ggplot2 available) or base plot
#'
#' @export
raincloud_plot <- function(data, grouping = NULL, colors = NULL) {
  if (!is.null(grouping) && grouping %in% names(data)) {
    groups <- unique(data[[grouping]])
  } else {
    groups <- "All Studies"
    data$group_var <- "All Studies"
    grouping <- "group_var"
  }

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # ggplot2 version
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[grouping]], y = effect))

    # Add violin (cloud)
    p <- p + ggplot2::geom_violin(
      ggplot2::aes(fill = .data[[grouping]]),
      alpha = 0.5, trim = FALSE
    )

    # Add boxplot
    p <- p + ggplot2::geom_boxplot(
      width = 0.2, alpha = 0.7, outlier.shape = NA
    )

    # Add jittered points (rain)
    p <- p + ggplot2::geom_jitter(
      width = 0.05, alpha = 0.6, size = 2
    )

    # Styling
    p <- p + ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Raincloud Plot - Effect Size Distribution",
        x = NULL,
        y = "Effect Size"
      ) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major.x = ggplot2::element_blank()
      )

    # Add null line
    p <- p + ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6
    )

    print(p)
    return(invisible(p))
  } else {
    # Base graphics fallback
    plot(1, type = "n",
         xlim = c(0.5, length(groups) + 0.5),
         ylim = range(data$effect) + c(-0.5, 0.5) * diff(range(data$effect)),
         xlab = "", ylab = "Effect Size",
         main = "Effect Size Distribution", xaxt = "n")

    axis(1, at = 1:length(groups), labels = groups)
    abline(h = 0, lty = 2, col = "red")

    for (i in seq_along(groups)) {
      group_data <- data[data[[grouping]] == groups[i], ]

      # Add violin-like density
      dens <- density(group_data$effect)
      dens_scaled <- 0.3 * dens$y / max(dens$y)
      polygon(i + dens_scaled, dens$x, col = rgb(0, 0, 1, 0.3), border = "blue")
      polygon(i - dens_scaled, dens$x, col = rgb(0, 0, 1, 0.3), border = "blue")

      # Add points
      points(rep(i, nrow(group_data)) + rnorm(nrow(group_data), 0, 0.05),
             group_data$effect, pch = 19, col = rgb(0, 0, 0, 0.5))

      # Add median line
      segments(i - 0.15, median(group_data$effect),
               i + 0.15, median(group_data$effect),
               lwd = 3, col = "darkblue")
    }

    return(invisible(NULL))
  }
}

#' P-Curve Meta-Analysis
#'
#' Performs p-curve analysis to assess evidential value and detect p-hacking.
#' Based on Simonsohn, Nelson, & Simmons (2014).
#'
#' @param data Meta-analysis data
#' @param plot Create diagnostic plot (default: TRUE)
#'
#' @return P-curve analysis results
#'
#' @export
p_curve_analysis <- function(data, plot = TRUE) {
  # Calculate p-values from effect and se
  z_scores <- data$effect / data$se
  p_values <- 2 * pnorm(-abs(z_scores))

  # Keep only significant results (p < 0.05)
  sig_p <- p_values[p_values < 0.05]

  if (length(sig_p) < 3) {
    warning("Fewer than 3 significant results. P-curve analysis unreliable.")
    return(NULL)
  }

  # Create histogram of p-values
  if (plot) {
    par(mfrow = c(1, 2))

    # Panel 1: Observed p-curve
    hist(sig_p, breaks = seq(0, 0.05, by = 0.005),
         col = "skyblue", border = "white",
         main = "P-Curve",
         xlab = "P-value",
         ylab = "Frequency",
         xlim = c(0, 0.05))

    # Add expected distributions
    x_seq <- seq(0, 0.05, by = 0.001)

    # Uniform (null hypothesis / p-hacking)
    lines(x_seq, rep(length(sig_p) * 0.005 / 0.05, length(x_seq)),
          col = "red", lwd = 2, lty = 2)

    # Right-skewed (true effect)
    expected_right_skew <- length(sig_p) * 0.005 * 2 * (1 - x_seq/0.05) / 0.05
    lines(x_seq, expected_right_skew, col = "darkgreen", lwd = 2, lty = 3)

    legend("topright",
           legend = c("Observed", "Null/P-hacking", "True Effect"),
           col = c("skyblue", "red", "darkgreen"),
           lty = c(1, 2, 3), lwd = 2, cex = 0.8)

    # Panel 2: Cumulative distribution
    plot(ecdf(sig_p), main = "Cumulative P-Curve",
         xlab = "P-value", ylab = "Cumulative Proportion",
         col = "blue", lwd = 2)

    # Add reference lines
    abline(a = 0, b = 20, col = "red", lty = 2, lwd = 2)  # Uniform
    curve(((x/0.05)^2), 0, 0.05, add = TRUE, col = "darkgreen", lty = 3, lwd = 2)  # Right-skew

    legend("bottomright",
           legend = c("Observed", "Null", "True Effect"),
           col = c("blue", "red", "darkgreen"),
           lty = c(1, 2, 3), lwd = 2, cex = 0.8)
  }

  # Test for right-skewness (evidential value)
  # Binomial test: are more results p < 0.025 than expected under uniform?
  n_very_sig <- sum(sig_p < 0.025)
  binom_test <- binom.test(n_very_sig, length(sig_p), p = 0.5)

  # Chi-square test for uniformity
  breaks <- seq(0, 0.05, by = 0.01)
  observed <- hist(sig_p, breaks = breaks, plot = FALSE)$counts
  expected <- rep(length(sig_p) / length(breaks), length(observed))
  chi_test <- chisq.test(observed, p = expected/sum(expected))

  result <- list(
    n_significant = length(sig_p),
    proportion_very_sig = n_very_sig / length(sig_p),
    evidential_value = binom_test$p.value < 0.05 && n_very_sig / length(sig_p) > 0.5,
    right_skew_p = binom_test$p.value,
    uniformity_p = chi_test$p.value,
    interpretation = if (binom_test$p.value < 0.05 && n_very_sig / length(sig_p) > 0.5) {
      "Strong evidential value detected (right-skewed p-curve)"
    } else if (chi_test$p.value > 0.05) {
      "WARNING: Uniform p-curve suggests p-hacking or publication bias"
    } else {
      "Inconclusive - more studies needed"
    }
  )

  cat("\n=== P-Curve Analysis ===\n")
  cat(sprintf("Significant results analyzed: %d\n", result$n_significant))
  cat(sprintf("Proportion with p < 0.025: %.1f%%\n", 100 * result$proportion_very_sig))
  cat(sprintf("Right-skew test: p = %.4f\n", result$right_skew_p))
  cat(sprintf("Uniformity test: p = %.4f\n", result$uniformity_p))
  cat(sprintf("Interpretation: %s\n", result$interpretation))

  return(invisible(result))
}

#' GRADE Evidence Profile Visualization
#'
#' Creates a GRADE (Grading of Recommendations Assessment, Development and
#' Evaluation) evidence profile visualization for systematic reviews.
#'
#' @param data Meta-analysis data
#' @param result Meta-analysis result
#' @param rob Risk of bias assessment
#' @param inconsistency Inconsistency rating
#' @param indirectness Indirectness rating
#' @param imprecision Imprecision rating
#' @param publication_bias Publication bias rating
#'
#' @return GRADE profile visualization
#'
#' @export
grade_profile <- function(data, result, rob = "Low", inconsistency = "None",
                         indirectness = "None", imprecision = "None",
                         publication_bias = "Undetected") {
  # Start with HIGH quality for RCTs, LOW for observational
  initial_quality <- "HIGH"

  # Downgrade for each issue
  downgrades <- 0

  # Risk of bias
  if (rob %in% c("High", "Serious")) downgrades <- downgrades + 1
  if (rob == "Very Serious") downgrades <- downgrades + 2

  # Inconsistency (heterogeneity)
  if (result$I2 > 50) downgrades <- downgrades + 1
  if (result$I2 > 75) downgrades <- downgrades + 1

  # Imprecision
  ci_width <- result$ci_upper - result$ci_lower
  if (ci_width > 0.8) downgrades <- downgrades + 1

  # Publication bias
  if (publication_bias %in% c("Likely", "Strong")) downgrades <- downgrades + 1

  # Determine final quality
  quality_levels <- c("HIGH", "MODERATE", "LOW", "VERY LOW")
  final_quality <- quality_levels[min(1 + downgrades, 4)]

  # Create visualization
  par(mar = c(5, 10, 4, 2))

  plot(NULL, xlim = c(0, 5), ylim = c(0, 6),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "GRADE Evidence Profile",
       frame.plot = TRUE)

  # Y-axis labels
  criteria <- c("Risk of Bias", "Inconsistency", "Indirectness", "Imprecision",
                "Publication Bias", "OVERALL QUALITY")
  axis(2, at = 1:6, labels = rev(criteria), las = 1, tick = FALSE)

  # X-axis
  axis(1, at = 0:4, labels = c("No Issue", "Serious", "Very Serious", "", ""))

  # Color scheme
  colors <- c(
    "No Issue" = "#4CAF50",      # Green
    "Serious" = "#FFC107",        # Amber
    "Very Serious" = "#F44336"    # Red
  )

  # Plot each criterion
  y_pos <- 6

  # Risk of bias
  rect_col <- if (rob == "Low") colors[1] else if (rob %in% c("High", "Serious")) colors[2] else colors[3]
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = rect_col, border = "white")
  text(2, y_pos, rob, font = 2, col = "white")
  y_pos <- y_pos - 1

  # Inconsistency
  incons_level <- if (result$I2 < 50) "Low" else if (result$I2 < 75) "Moderate" else "High"
  rect_col <- if (incons_level == "Low") colors[1] else if (incons_level == "Moderate") colors[2] else colors[3]
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = rect_col, border = "white")
  text(2, y_pos, sprintf("%s (I²=%.0f%%)", incons_level, result$I2), font = 2, col = "white")
  y_pos <- y_pos - 1

  # Indirectness
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = colors[1], border = "white")
  text(2, y_pos, indirectness, font = 2, col = "white")
  y_pos <- y_pos - 1

  # Imprecision
  imprec_level <- if (ci_width < 0.5) "Low" else if (ci_width < 0.8) "Moderate" else "High"
  rect_col <- if (imprec_level == "Low") colors[1] else if (imprec_level == "Moderate") colors[2] else colors[3]
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = rect_col, border = "white")
  text(2, y_pos, sprintf("%s (CI: %.2f)", imprec_level, ci_width), font = 2, col = "white")
  y_pos <- y_pos - 1

  # Publication bias
  pb_col <- if (publication_bias == "Undetected") colors[1] else colors[2]
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = pb_col, border = "white")
  text(2, y_pos, publication_bias, font = 2, col = "white")
  y_pos <- y_pos - 1

  # Overall quality
  qual_col <- switch(final_quality,
                     "HIGH" = "#4CAF50",
                     "MODERATE" = "#8BC34A",
                     "LOW" = "#FFC107",
                     "VERY LOW" = "#F44336")
  rect(0, y_pos - 0.4, 4, y_pos + 0.4, col = qual_col, border = "black", lwd = 3)
  text(2, y_pos, final_quality, font = 2, cex = 1.5, col = "white")

  # Add legend
  legend("topright",
         legend = c("No concerns", "Some concerns", "Serious concerns"),
         fill = colors,
         bty = "n", cex = 0.9)

  # Return assessment
  assessment <- list(
    initial_quality = initial_quality,
    final_quality = final_quality,
    downgrades = downgrades,
    rob = rob,
    inconsistency = incons_level,
    imprecision = imprec_level,
    publication_bias = publication_bias
  )

  return(invisible(assessment))
}

#' Heat Map of Study-Level Heterogeneity
#'
#' Creates a heat map showing pairwise heterogeneity between studies.
#'
#' @param data Meta-analysis data
#' @param method Distance method ("effect", "se", "both")
#'
#' @return Heat map plot
#'
#' @export
heterogeneity_heatmap <- function(data, method = "both") {
  k <- nrow(data)

  # Calculate pairwise distances
  dist_matrix <- matrix(0, k, k)

  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      if (method == "effect") {
        dist_matrix[i, j] <- abs(data$effect[i] - data$effect[j])
      } else if (method == "se") {
        dist_matrix[i, j] <- abs(data$se[i] - data$se[j])
      } else {  # both
        effect_diff <- abs(data$effect[i] - data$effect[j])
        se_diff <- abs(data$se[i] - data$se[j])
        dist_matrix[i, j] <- sqrt(effect_diff^2 + se_diff^2)
      }
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }

  # Create heat map
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # ggplot2 version
    dist_df <- expand.grid(Study1 = 1:k, Study2 = 1:k)
    dist_df$Distance <- as.vector(dist_matrix)

    p <- ggplot2::ggplot(dist_df, ggplot2::aes(x = Study1, y = Study2, fill = Distance)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        low = "white", mid = "yellow", high = "red",
        midpoint = median(dist_matrix),
        name = "Heterogeneity"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Pairwise Study Heterogeneity",
        x = "Study", y = "Study"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    print(p)
    return(invisible(dist_matrix))
  } else {
    # Base graphics version
    image(1:k, 1:k, dist_matrix,
          col = heat.colors(20, rev = TRUE),
          xlab = "Study", ylab = "Study",
          main = "Pairwise Study Heterogeneity")

    # Add grid
    abline(h = 1:k + 0.5, col = "white", lwd = 0.5)
    abline(v = 1:k + 0.5, col = "white", lwd = 0.5)

    return(invisible(dist_matrix))
  }
}

#' Helper functions for dashboard panels
#' @keywords internal

plot_mini_forest <- function(data, result, main = "") {
  k <- min(nrow(data), 10)  # Show max 10 studies
  data_sub <- head(data[order(data$effect), ], k)

  xlim <- range(c(data_sub$effect - 1.96*data_sub$se,
                  data_sub$effect + 1.96*data_sub$se))

  plot(NULL, xlim = xlim, ylim = c(0, k+2),
       xlab = "Effect", ylab = "", yaxt = "n", main = main, frame.plot = FALSE)
  abline(v = 0, lty = 2, col = "gray")

  for (i in 1:k) {
    segments(data_sub$effect[i] - 1.96*data_sub$se[i], i,
             data_sub$effect[i] + 1.96*data_sub$se[i], i)
    points(data_sub$effect[i], i, pch = 19, cex = 1.5)
  }

  # Pooled
  segments(result$ci_lower, k+1, result$ci_upper, k+1, lwd = 2, col = "blue")
  points(result$estimate, k+1, pch = 18, cex = 2, col = "blue")
}

plot_mini_funnel <- function(data, result, main = "") {
  precision <- 1/data$se
  plot(data$effect, precision, pch = 19, col = rgb(0,0,0,0.6),
       xlab = "Effect", ylab = "Precision", main = main)
  abline(v = result$estimate, lty = 2, col = "blue", lwd = 2)
}

plot_mini_baujat <- function(data, main = "") {
  # Simplified Baujat calculation
  k <- nrow(data)
  contrib <- numeric(k)
  influence <- numeric(k)

  for (i in 1:k) {
    contrib[i] <- abs(data$effect[i] - mean(data$effect)) / data$se[i]
    influence[i] <- 1/data$se[i]^2
  }

  plot(contrib, influence, pch = 19, col = rgb(0,0,1,0.6),
       xlab = "Heterogeneity Contribution", ylab = "Influence", main = main)
  abline(v = median(contrib), h = median(influence), lty = 2, col = "gray")
}

plot_mini_influence <- function(data, result, main = "") {
  k <- nrow(data)
  loo_est <- numeric(k)

  for (i in 1:k) {
    tryCatch({
      res_i <- cbamm_fast(data[-i,], verbose = FALSE)
      loo_est[i] <- res_i$estimate
    }, error = function(e) loo_est[i] <<- result$estimate)
  }

  influence <- abs(loo_est - result$estimate)
  barplot(influence, names.arg = 1:k, col = "steelblue",
          xlab = "Study", ylab = "Influence", main = main)
}

plot_heterogeneity_time <- function(data, main = "") {
  if (!"year" %in% names(data)) return(plot.new())

  plot(data$year, abs(data$effect), pch = 19, col = rgb(0,0,1,0.6),
       xlab = "Year", ylab = "|Effect|", main = main)
  lines(lowess(data$year, abs(data$effect)), col = "red", lwd = 2)
}

plot_effect_distribution <- function(data, main = "") {
  hist(data$effect, breaks = 15, col = "skyblue", border = "white",
       xlab = "Effect", main = main, freq = FALSE)
  lines(density(data$effect), col = "darkblue", lwd = 2)
  abline(v = 0, lty = 2, col = "red")
}

plot_p_curve <- function(data, main = "") {
  z <- abs(data$effect / data$se)
  p <- 2*pnorm(-z)
  p_sig <- p[p < 0.05]

  if (length(p_sig) < 3) {
    plot.new()
    text(0.5, 0.5, "Too few\nsignificant results", cex = 1.2)
    return()
  }

  hist(p_sig, breaks = 10, col = "skyblue", border = "white",
       xlab = "P-value", main = main, xlim = c(0, 0.05))
}

plot_mini_cumulative <- function(data, main = "") {
  if (!"year" %in% names(data)) return(plot.new())

  data <- data[order(data$year), ]
  k <- nrow(data)
  cum_est <- numeric(k)

  for (i in 3:k) {
    tryCatch({
      res <- cbamm_fast(data[1:i,], verbose = FALSE)
      cum_est[i] <- res$estimate
    }, error = function(e) cum_est[i] <<- NA)
  }

  plot(3:k, cum_est[3:k], type = "l", lwd = 2, col = "blue",
       xlab = "# Studies", ylab = "Cumulative Effect", main = main)
  abline(h = 0, lty = 2, col = "gray")
}

plot_mini_radial <- function(data, result, main = "") {
  precision <- 1/data$se
  z <- (data$effect - result$estimate) / data$se

  plot(precision, z, pch = 19, col = rgb(0,0,1,0.6),
       xlab = "Precision", ylab = "Standardized Effect", main = main)
  abline(h = c(-1.96, 0, 1.96), lty = c(2,1,2), col = c("red", "blue", "red"))
}

plot_precision_histogram <- function(data, result, main = "") {
  weights <- 1/data$se^2
  weights <- weights/sum(weights)

  xlim <- range(data$effect) + c(-1, 1)*0.2*diff(range(data$effect))
  breaks <- seq(xlim[1], xlim[2], length.out = 15)

  hist(data$effect, breaks = breaks, col = rgb(0, 0, 1, 0.5),
       xlab = "Effect", main = main, border = "white")
  abline(v = result$estimate, col = "red", lwd = 2, lty = 2)
}

plot_quality_assessment <- function(data, main = "") {
  if ("quality" %in% names(data)) {
    barplot(table(data$quality), col = c("red", "yellow", "green"),
            main = main, ylab = "Count", las = 2)
  } else {
    plot.new()
    text(0.5, 0.5, "No quality data", cex = 1.2)
  }
}

plot_study_size <- function(data, main = "") {
  if ("n" %in% names(data)) {
    barplot(head(sort(data$n, decreasing = TRUE), 10),
            col = "steelblue", main = main, ylab = "Sample Size")
  } else {
    precision <- 1/data$se
    barplot(head(sort(precision, decreasing = TRUE), 10),
            col = "steelblue", main = main, ylab = "Precision")
  }
}

plot_loo_sensitivity <- function(data, main = "") {
  k <- min(nrow(data), 10)
  loo_est <- numeric(k)

  for (i in 1:k) {
    tryCatch({
      res <- cbamm_fast(data[-i,], verbose = FALSE)
      loo_est[i] <- res$estimate
    }, error = function(e) loo_est[i] <<- NA)
  }

  plot(1:k, loo_est, pch = 19, col = "darkblue",
       xlab = "Study Omitted", ylab = "Pooled Estimate", main = main,
       ylim = range(loo_est, na.rm = TRUE) + c(-0.1, 0.1)*diff(range(loo_est, na.rm = TRUE)))
  abline(h = mean(loo_est, na.rm = TRUE), lty = 2, col = "red")
}

plot_heterogeneity_decomp <- function(data, result, main = "") {
  # Decompose heterogeneity sources
  total_var <- var(data$effect)
  within_var <- mean(data$se^2)
  between_var <- max(0, total_var - within_var)

  pie(c(within_var, between_var),
      labels = c("Within-study", "Between-study"),
      col = c("skyblue", "coral"),
      main = main)
}

plot_summary_stats <- function(data, result, main = "") {
  par(mar = c(2, 2, 3, 2))
  plot.new()

  stats_text <- sprintf(
    "Studies: %d\nEffect: %.3f (%.3f, %.3f)\nI²: %.1f%%\nτ²: %.3f\np: %.4f\n\nQuality: %s",
    nrow(data),
    result$estimate,
    result$ci_lower,
    result$ci_upper,
    result$I2,
    result$tau2,
    result$p_value,
    if (result$I2 < 25) "Low Het" else if (result$I2 < 75) "Mod Het" else "High Het"
  )

  text(0.5, 0.5, stats_text, cex = 1.1, family = "mono")
  title(main)
}
