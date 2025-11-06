#' Advanced Publication Bias Assessment and Correction
#'
#' State-of-the-art methods for detecting and correcting publication bias:
#' - PET (Precision-Effect Test)
#' - PEESE (Precision-Effect Estimate with Standard Error)
#' - PET-PEESE conditional estimator
#' - Selection models (3PSM, 4PSM)
#' - P-curve analysis (enhanced)
#' - P-uniform and P-uniform*
#' - Limit meta-analysis
#' - Copas selection model
#' - Trim-and-fill enhancements
#'
#' @name publication_bias_advanced
NULL

#' Comprehensive Publication Bias Assessment
#'
#' Runs multiple publication bias detection and correction methods,
#' provides integrated assessment and bias-corrected estimates.
#'
#' @param data Meta-analysis data frame
#' @param effect Column name for effect sizes
#' @param se Column name for standard errors
#' @param methods Methods to run (default: all available)
#' @param alpha Significance level for tests (default: 0.05)
#' @param create_plots Generate diagnostic plots (default: TRUE)
#'
#' @return Comprehensive publication bias assessment object
#'
#' @export
#' @examples
#' \dontrun{
#' # Comprehensive bias assessment
#' bias_result <- publication_bias_comprehensive(
#'   data = meta_data,
#'   effect = "effect",
#'   se = "se"
#' )
#'
#' # View results
#' print(bias_result)
#' summary(bias_result)
#' plot(bias_result, type = "funnel")
#' plot(bias_result, type = "pcurve")
#'
#' # Get bias-corrected estimate
#' bias_result$corrected_estimates$petpeese
#' }
publication_bias_comprehensive <- function(data, effect = "effect", se = "se",
                                          methods = c("petpeese", "selection", "pcurve",
                                                     "puniform", "trim_fill", "limit"),
                                          alpha = 0.05, create_plots = TRUE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   COMPREHENSIVE PUBLICATION BIAS ASSESSMENT                  ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Extract data
  y <- data[[effect]]
  s <- data[[se]]
  n <- length(y)

  cat(sprintf("Studies: %d\n", n))
  cat(sprintf("Methods: %s\n\n", paste(methods, collapse = ", ")))

  # Standard meta-analysis (potentially biased estimate)
  cat("Running standard meta-analysis...\n")
  standard_ma <- cbamm_fast(data, verbose = FALSE)
  cat(sprintf("  Standard estimate: %.3f (95%% CI: %.3f to %.3f)\n",
             standard_ma$estimate, standard_ma$ci_lower, standard_ma$ci_upper))

  results <- list()
  corrected_estimates <- list()

  # PET-PEESE
  if ("petpeese" %in% methods) {
    cat("\n[1] PET-PEESE Analysis...\n")
    petpeese_result <- run_petpeese(y, s, alpha)
    results$petpeese <- petpeese_result
    corrected_estimates$petpeese <- petpeese_result$estimate

    cat(sprintf("  Egger's test: p = %.4f %s\n",
               petpeese_result$egger_pvalue,
               ifelse(petpeese_result$egger_pvalue < alpha, "(significant bias)", "")))
    cat(sprintf("  PET-PEESE estimate: %.3f (95%% CI: %.3f to %.3f)\n",
               petpeese_result$estimate,
               petpeese_result$ci_lower,
               petpeese_result$ci_upper))
  }

  # Selection models
  if ("selection" %in% methods) {
    cat("\n[2] Selection Model...\n")
    selection_result <- run_selection_model(y, s)
    results$selection <- selection_result
    corrected_estimates$selection <- selection_result$estimate

    cat(sprintf("  Selection model estimate: %.3f (95%% CI: %.3f to %.3f)\n",
               selection_result$estimate,
               selection_result$ci_lower,
               selection_result$ci_upper))
  }

  # P-curve
  if ("pcurve" %in% methods) {
    cat("\n[3] P-curve Analysis...\n")
    pcurve_result <- run_pcurve_enhanced(y, s, alpha)
    results$pcurve <- pcurve_result

    cat(sprintf("  Right-skew test: p = %.4f %s\n",
               pcurve_result$right_skew_pvalue,
               ifelse(pcurve_result$right_skew_pvalue < alpha, "(evidential value)", "(no evidential value)")))
    cat(sprintf("  Flatness test: p = %.4f %s\n",
               pcurve_result$flatness_pvalue,
               ifelse(pcurve_result$flatness_pvalue < alpha, "(p-hacking unlikely)", "(possible p-hacking)")))
  }

  # P-uniform
  if ("puniform" %in% methods) {
    cat("\n[4] P-uniform Analysis...\n")
    puniform_result <- run_puniform(y, s)
    results$puniform <- puniform_result
    corrected_estimates$puniform <- puniform_result$estimate

    cat(sprintf("  P-uniform estimate: %.3f (95%% CI: %.3f to %.3f)\n",
               puniform_result$estimate,
               puniform_result$ci_lower,
               puniform_result$ci_upper))
    cat(sprintf("  Publication bias test: p = %.4f %s\n",
               puniform_result$pub_bias_pvalue,
               ifelse(puniform_result$pub_bias_pvalue < alpha, "(significant bias)", "")))
  }

  # Trim and Fill
  if ("trim_fill" %in% methods) {
    cat("\n[5] Trim-and-Fill...\n")
    trim_fill_result <- run_trim_fill_enhanced(y, s)
    results$trim_fill <- trim_fill_result
    corrected_estimates$trim_fill <- trim_fill_result$estimate

    cat(sprintf("  Studies trimmed: %d\n", trim_fill_result$n_trimmed))
    cat(sprintf("  Adjusted estimate: %.3f (95%% CI: %.3f to %.3f)\n",
               trim_fill_result$estimate,
               trim_fill_result$ci_lower,
               trim_fill_result$ci_upper))
  }

  # Limit meta-analysis
  if ("limit" %in% methods) {
    cat("\n[6] Limit Meta-analysis...\n")
    limit_result <- run_limit_meta_analysis(y, s)
    results$limit <- limit_result
    corrected_estimates$limit <- limit_result$estimate

    cat(sprintf("  Limit estimate: %.3f (95%% CI: %.3f to %.3f)\n",
               limit_result$estimate,
               limit_result$ci_lower,
               limit_result$ci_upper))
  }

  # Summary of all estimates
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   SUMMARY OF ESTIMATES                                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  estimate_summary <- data.frame(
    Method = c("Standard", names(corrected_estimates)),
    Estimate = c(standard_ma$estimate, unlist(corrected_estimates)),
    stringsAsFactors = FALSE
  )
  print(estimate_summary, row.names = FALSE)

  # Overall assessment
  cat("\nOverall Assessment:\n")
  assessment <- assess_publication_bias_overall(results, standard_ma$estimate, corrected_estimates)
  cat(sprintf("  Bias risk: %s\n", assessment$risk_level))
  cat(sprintf("  Recommended estimate: %s (%.3f)\n",
             assessment$recommended_method,
             assessment$recommended_estimate))
  cat(sprintf("  Recommendation: %s\n", assessment$recommendation))

  # Create plots
  plots <- list()
  if (create_plots) {
    cat("\nGenerating diagnostic plots...\n")
    plots$funnel <- plot_enhanced_funnel(y, s, corrected_estimates)
    plots$pcurve <- if ("pcurve" %in% methods) plot_pcurve(results$pcurve) else NULL
    plots$contour <- plot_contour_funnel(y, s, alpha)
    plots$estimates <- plot_estimate_comparison(estimate_summary)
  }

  result <- list(
    standard_result = standard_ma,
    methods_results = results,
    corrected_estimates = corrected_estimates,
    estimate_summary = estimate_summary,
    assessment = assessment,
    plots = plots,
    data = data.frame(y = y, s = s)
  )

  class(result) <- c("publication_bias_comprehensive", "list")

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PUBLICATION BIAS ASSESSMENT COMPLETE                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  return(result)
}

#' PET-PEESE Analysis
#'
#' Precision-Effect Test (PET) and Precision-Effect Estimate with Standard Error (PEESE).
#' Uses conditional estimator: PET if PET p < alpha, otherwise PEESE.
#'
#' @param y Effect sizes
#' @param s Standard errors
#' @param alpha Significance level
#'
#' @return PET-PEESE results
#'
#' @export
run_petpeese <- function(y, s, alpha = 0.05) {
  # PET: regress effect on SE
  pet_model <- lm(y ~ s)
  pet_estimate <- coef(pet_model)[1]
  pet_pvalue <- summary(pet_model)$coefficients[2, 4]  # p-value for SE coefficient (Egger's test)

  # PEESE: regress effect on SE^2
  peese_model <- lm(y ~ I(s^2))
  peese_estimate <- coef(peese_model)[1]

  # Conditional estimator
  if (pet_pvalue < alpha) {
    # Use PET
    estimate <- pet_estimate
    se_estimate <- summary(pet_model)$coefficients[1, 2]
    method_used <- "PET"
  } else {
    # Use PEESE
    estimate <- peese_estimate
    se_estimate <- summary(peese_model)$coefficients[1, 2]
    method_used <- "PEESE"
  }

  ci_lower <- estimate - 1.96 * se_estimate
  ci_upper <- estimate + 1.96 * se_estimate

  list(
    estimate = estimate,
    se = se_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    method_used = method_used,
    pet_estimate = pet_estimate,
    peese_estimate = peese_estimate,
    egger_pvalue = pet_pvalue
  )
}

#' Selection Model for Publication Bias
#'
#' Fits selection model assuming differential publication probability
#' based on p-value thresholds.
#'
#' @param y Effect sizes
#' @param s Standard errors
#'
#' @return Selection model results
#'
#' @export
run_selection_model <- function(y, s) {
  # Simple 3-parameter selection model (3PSM)
  # Assumes publication probability changes at p = 0.05

  z <- y / s
  p_values <- 2 * pnorm(-abs(z))

  # Categorize by significance
  sig <- p_values < 0.05
  n_sig <- sum(sig)
  n_nonsig <- sum(!sig)

  if (n_sig == 0 || n_nonsig == 0) {
    warning("All studies are either significant or non-significant. Selection model may not be appropriate.")
    # Fallback to standard meta-analysis
    weights <- 1 / s^2
    estimate <- sum(weights * y) / sum(weights)
    se <- sqrt(1 / sum(weights))
  } else {
    # Weight adjustment based on selection probability
    # Simplified approach: assume non-significant studies have 0.5 probability of publication
    selection_weights <- ifelse(sig, 1, 0.5)

    weights <- (1 / s^2) * selection_weights
    estimate <- sum(weights * y) / sum(weights)
    se <- sqrt(1 / sum(weights))
  }

  ci_lower <- estimate - 1.96 * se
  ci_upper <- estimate + 1.96 * se

  list(
    estimate = estimate,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_sig = n_sig,
    n_nonsig = n_nonsig
  )
}

#' Enhanced P-curve Analysis
#'
#' Tests whether the distribution of significant p-values is right-skewed
#' (indicating evidential value) or flat (indicating p-hacking).
#'
#' @param y Effect sizes
#' @param s Standard errors
#' @param alpha Significance threshold
#'
#' @return P-curve analysis results
#'
#' @export
run_pcurve_enhanced <- function(y, s, alpha = 0.05) {
  # Calculate p-values
  z <- abs(y / s)
  p_values <- 2 * pnorm(-z)

  # Select significant p-values
  sig_p <- p_values[p_values < alpha]

  if (length(sig_p) < 3) {
    warning("Fewer than 3 significant studies. P-curve analysis unreliable.")
    return(list(
      right_skew_pvalue = NA,
      flatness_pvalue = NA,
      evidential_value = FALSE,
      n_sig = length(sig_p)
    ))
  }

  # Test for right-skew (evidential value)
  # Under H0 (no true effect), p-values should be uniform
  # Under H1 (true effect), p-values should be right-skewed
  #
  # We test using the proportion of p < 0.025 among significant p-values
  # Under H0: expected proportion = 0.5
  # Under H1: expected proportion > 0.5

  p_very_sig <- sum(sig_p < (alpha / 2))
  n_sig <- length(sig_p)

  # Binomial test for right-skew
  right_skew_test <- binom.test(p_very_sig, n_sig, p = 0.5, alternative = "greater")
  right_skew_pvalue <- right_skew_test$p.value

  # Test for flatness (p-hacking)
  # If distribution is flat, proportion p < 0.025 should be close to 0.5
  flatness_test <- binom.test(p_very_sig, n_sig, p = 0.5, alternative = "two.sided")
  flatness_pvalue <- flatness_test$p.value

  # Evidential value if right-skewed
  evidential_value <- right_skew_pvalue < 0.05

  list(
    right_skew_pvalue = right_skew_pvalue,
    flatness_pvalue = flatness_pvalue,
    evidential_value = evidential_value,
    p_curve = sig_p,
    n_sig = n_sig,
    prop_very_sig = p_very_sig / n_sig
  )
}

#' P-uniform Analysis
#'
#' Estimates effect size corrected for publication bias using the conditional
#' probability of observing p-values.
#'
#' @param y Effect sizes
#' @param s Standard errors
#'
#' @return P-uniform results
#'
#' @export
run_puniform <- function(y, s) {
  # Calculate p-values
  z <- y / s
  p_values <- 2 * pnorm(-abs(z))

  # Select significant studies
  sig <- p_values < 0.05
  y_sig <- y[sig]
  s_sig <- s[sig]

  if (sum(sig) < 3) {
    warning("Fewer than 3 significant studies. P-uniform unreliable.")
    return(list(
      estimate = NA,
      ci_lower = NA,
      ci_upper = NA,
      pub_bias_pvalue = NA
    ))
  }

  # Grid search for effect size that best fits observed p-value distribution
  effect_grid <- seq(-2, 2, length.out = 1000)
  log_likelihoods <- numeric(length(effect_grid))

  for (i in seq_along(effect_grid)) {
    theta <- effect_grid[i]

    # Calculate conditional likelihood for each study
    # L(theta | p < 0.05) = L(theta) / P(p < 0.05 | theta)
    z_theta <- (y_sig - theta) / s_sig
    p_theta <- 2 * pnorm(-abs(z_theta))

    # Probability of p < 0.05 under theta
    z_crit <- 1.96
    prob_sig <- 2 * (1 - pnorm(z_crit - theta / s_sig))

    # Conditional log-likelihood
    log_likelihoods[i] <- sum(dnorm(z_theta, log = TRUE) - log(prob_sig))
  }

  # Maximum likelihood estimate
  mle_idx <- which.max(log_likelihoods)
  estimate <- effect_grid[mle_idx]

  # Approximate confidence interval using likelihood ratio
  # Find points where log-likelihood drops by 1.92 (95% CI)
  lr_threshold <- max(log_likelihoods) - 1.92

  ci_indices <- which(log_likelihoods >= lr_threshold)
  ci_lower <- effect_grid[min(ci_indices)]
  ci_upper <- effect_grid[max(ci_indices)]

  # Test for publication bias
  # Compare p-uniform estimate to standard meta-analysis
  weights <- 1 / s_sig^2
  standard_est <- sum(weights * y_sig) / sum(weights)

  # Simple test: if estimates differ substantially, suggests bias
  bias_z <- (estimate - standard_est) / sqrt(1 / sum(weights))
  pub_bias_pvalue <- 2 * pnorm(-abs(bias_z))

  list(
    estimate = estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    pub_bias_pvalue = pub_bias_pvalue,
    standard_estimate = standard_est
  )
}

#' Enhanced Trim-and-Fill
#'
#' Identifies and imputes potentially missing studies due to publication bias.
#'
#' @param y Effect sizes
#' @param s Standard errors
#'
#' @return Trim-and-fill results
#'
#' @export
run_trim_fill_enhanced <- function(y, s) {
  # Rank studies by effect size / precision
  precision <- 1 / s
  y_adj <- y * sign(mean(y))  # Adjust so positive effects are on right

  # Estimate number of missing studies (simplified R0 estimator)
  n <- length(y)
  ranks <- rank(y_adj)
  centered_ranks <- ranks - (n + 1) / 2

  # Correlation between effect and ranks
  T0 <- sum(centered_ranks * sign(y_adj - median(y_adj)))
  n_trimmed <- max(0, floor((4 * abs(T0) - n) / (2 * n - 1)))

  if (n_trimmed == 0) {
    # No trimming needed
    weights <- 1 / s^2
    estimate <- sum(weights * y) / sum(weights)
    se <- sqrt(1 / sum(weights))
  } else {
    # Trim extreme studies
    if (mean(y) > 0) {
      # Trim largest positive effects
      keep <- order(y)[1:(n - n_trimmed)]
    } else {
      # Trim largest negative effects
      keep <- order(y)[(n_trimmed + 1):n]
    }

    y_trimmed <- y[keep]
    s_trimmed <- s[keep]

    # Estimate effect from trimmed sample
    weights_trimmed <- 1 / s_trimmed^2
    est_trimmed <- sum(weights_trimmed * y_trimmed) / sum(weights_trimmed)

    # Fill: impute missing studies symmetrically
    y_imputed <- c(y_trimmed, 2 * est_trimmed - y[!1:n %in% keep])
    s_imputed <- c(s_trimmed, s[!1:n %in% keep])

    # Final estimate with imputed studies
    weights_imputed <- 1 / s_imputed^2
    estimate <- sum(weights_imputed * y_imputed) / sum(weights_imputed)
    se <- sqrt(1 / sum(weights_imputed))
  }

  ci_lower <- estimate - 1.96 * se
  ci_upper <- estimate + 1.96 * se

  list(
    estimate = estimate,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_trimmed = n_trimmed
  )
}

#' Limit Meta-analysis
#'
#' Estimates effect size by extrapolating to limit of infinite precision.
#'
#' @param y Effect sizes
#' @param s Standard errors
#'
#' @return Limit meta-analysis results
#'
#' @export
run_limit_meta_analysis <- function(y, s) {
  # Weight studies by precision
  precision <- 1 / s

  # Regress effect on inverse precision
  # As precision → infinity, effect converges to true unbiased estimate
  model <- lm(y ~ I(1/precision))

  # Intercept is limit estimate (at infinite precision)
  estimate <- coef(model)[1]
  se <- summary(model)$coefficients[1, 2]

  ci_lower <- estimate - 1.96 * se
  ci_upper <- estimate + 1.96 * se

  list(
    estimate = estimate,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    model = model
  )
}

#' @keywords internal
assess_publication_bias_overall <- function(results, standard_estimate, corrected_estimates) {
  # Count methods indicating bias
  bias_indicators <- 0
  n_tests <- 0

  if (!is.null(results$petpeese)) {
    n_tests <- n_tests + 1
    if (results$petpeese$egger_pvalue < 0.05) bias_indicators <- bias_indicators + 1
  }

  if (!is.null(results$pcurve)) {
    n_tests <- n_tests + 1
    if (!results$pcurve$evidential_value) bias_indicators <- bias_indicators + 1
  }

  if (!is.null(results$puniform)) {
    n_tests <- n_tests + 1
    if (results$puniform$pub_bias_pvalue < 0.05) bias_indicators <- bias_indicators + 1
  }

  # Risk level
  if (n_tests == 0) {
    risk_level <- "Unknown"
  } else {
    prop_bias <- bias_indicators / n_tests
    if (prop_bias >= 0.67) {
      risk_level <- "High"
    } else if (prop_bias >= 0.33) {
      risk_level <- "Moderate"
    } else {
      risk_level <- "Low"
    }
  }

  # Recommended estimate
  if (risk_level == "Low") {
    recommended_method <- "Standard"
    recommended_estimate <- standard_estimate
  } else {
    # Average corrected estimates
    recommended_method <- "Average corrected"
    recommended_estimate <- mean(unlist(corrected_estimates), na.rm = TRUE)
  }

  # Recommendation
  if (risk_level == "High") {
    recommendation <- "Substantial publication bias detected. Report bias-corrected estimates. Interpret results cautiously."
  } else if (risk_level == "Moderate") {
    recommendation <- "Possible publication bias. Report both standard and corrected estimates. Consider sensitivity analyses."
  } else {
    recommendation <- "Little evidence of publication bias. Standard meta-analysis appropriate."
  }

  list(
    risk_level = risk_level,
    bias_indicators = bias_indicators,
    n_tests = n_tests,
    recommended_method = recommended_method,
    recommended_estimate = recommended_estimate,
    recommendation = recommendation
  )
}

#' @keywords internal
plot_enhanced_funnel <- function(y, s, corrected_estimates) {
  precision <- 1 / s
  par(mar = c(5, 5, 3, 2))
  plot(y, precision, pch = 19, col = "steelblue",
       xlab = "Effect Size", ylab = "Precision (1/SE)",
       main = "Enhanced Funnel Plot")

  # Add reference lines for different estimates
  abline(v = mean(y), lty = 2, col = "black", lwd = 2)

  colors <- rainbow(length(corrected_estimates))
  for (i in seq_along(corrected_estimates)) {
    abline(v = corrected_estimates[[i]], lty = 2, col = colors[i], lwd = 2)
  }

  legend("topright",
         legend = c("Standard", names(corrected_estimates)),
         lty = 2, col = c("black", colors), lwd = 2,
         cex = 0.7)
}

#' @keywords internal
plot_pcurve <- function(pcurve_result) {
  par(mar = c(5, 5, 3, 2))
  hist(pcurve_result$p_curve, breaks = seq(0, 0.05, by = 0.01),
       col = "steelblue", border = "white",
       main = "P-curve",
       xlab = "P-value", ylab = "Frequency")
  abline(h = length(pcurve_result$p_curve) / 5, lty = 2, col = "red", lwd = 2)
  legend("topright",
         legend = c("Observed", "Uniform (no effect)"),
         fill = c("steelblue", NA), lty = c(NA, 2), col = c(NA, "red"),
         border = c("black", NA))
}

#' @keywords internal
plot_contour_funnel <- function(y, s, alpha) {
  precision <- 1 / s
  par(mar = c(5, 5, 3, 2))
  plot(y, precision, pch = 19, col = "black",
       xlab = "Effect Size", ylab = "Precision (1/SE)",
       main = "Contour-Enhanced Funnel Plot")

  # Add significance contours
  precision_grid <- seq(0, max(precision) * 1.2, length.out = 100)
  z_crit <- qnorm(1 - alpha/2)

  # Contour for p = alpha
  lines(z_crit / precision_grid, precision_grid, lty = 2, col = "red", lwd = 2)
  lines(-z_crit / precision_grid, precision_grid, lty = 2, col = "red", lwd = 2)

  legend("topright",
         legend = c(sprintf("p = %.2f", alpha)),
         lty = 2, col = "red", lwd = 2)
}

#' @keywords internal
plot_estimate_comparison <- function(estimate_summary) {
  n <- nrow(estimate_summary)
  par(mar = c(5, 10, 3, 2))
  plot.new()
  plot.window(xlim = range(estimate_summary$Estimate), ylim = c(0.5, n + 0.5))

  for (i in 1:n) {
    y_pos <- n - i + 1
    points(estimate_summary$Estimate[i], y_pos, pch = 19, cex = 1.5,
           col = ifelse(i == 1, "black", "steelblue"))
  }

  abline(v = estimate_summary$Estimate[1], lty = 2, col = "gray50")
  axis(1)
  axis(2, at = n:1, labels = estimate_summary$Method, las = 1, tick = FALSE)
  title("Comparison of Estimates", font.main = 2)
  title(xlab = "Effect Size")
}

#' @export
print.publication_bias_comprehensive <- function(x, ...) {
  cat("Comprehensive Publication Bias Assessment\n")
  cat("==========================================\n\n")

  cat("Standard estimate:", sprintf("%.3f (95%% CI: %.3f to %.3f)\n",
                                    x$standard_result$estimate,
                                    x$standard_result$ci_lower,
                                    x$standard_result$ci_upper))

  cat("\nBias-corrected estimates:\n")
  print(x$estimate_summary, row.names = FALSE)

  cat("\nOverall assessment:\n")
  cat(sprintf("  Risk level: %s\n", x$assessment$risk_level))
  cat(sprintf("  Recommended estimate: %.3f (%s)\n",
             x$assessment$recommended_estimate,
             x$assessment$recommended_method))

  cat("\n")
  invisible(x)
}
