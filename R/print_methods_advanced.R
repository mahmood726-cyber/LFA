#' Additional S3 Print Methods for Advanced Classes
#'
#' @name print_methods_advanced
NULL

#' Print Method for Egger Test
#' @param x An egger_test object
#' @param ... Additional arguments
#' @export
print.egger_test <- function(x, ...) {
  cat("\n", x$test, "\n")
  cat(sprintf("Intercept: %.4f (SE = %.4f)\n", x$intercept, x$se))
  cat(sprintf("t = %.3f, df = %d, p = %.4f\n", x$t_stat, x$df, x$p_value))
  cat(sprintf("95%% CI: (%.4f, %.4f)\n\n", x$ci_lower, x$ci_upper))
  cat("Interpretation:", x$interpretation, "\n")
  invisible(x)
}

#' Print Method for PET-PEESE
#' @param x A pet_peese object
#' @param ... Additional arguments
#' @export
print.pet_peese <- function(x, ...) {
  cat("\nPET-PEESE Publication Bias Correction\n")
  cat(sprintf("Method used: %s\n", x$method_used))
  cat(sprintf("Reasoning: %s\n\n", x$reasoning))

  cat("Bias-Corrected Estimate:\n")
  cat(sprintf("  Effect: %.3f\n", x$estimate))
  cat(sprintf("  95%% CI: (%.3f, %.3f)\n", x$ci_lower, x$ci_upper))
  cat(sprintf("  SE: %.3f\n", x$se))
  cat(sprintf("  p-value: %.4f\n\n", x$p_value))

  cat(sprintf("Note: %s\n", x$note))
  invisible(x)
}

#' Print Method for Trim and Fill
#' @param x A trim_fill object
#' @param ... Additional arguments
#' @export
print.trim_fill <- function(x, ...) {
  cat("\nTrim and Fill Method\n")
  cat(sprintf("Estimated missing studies: %d (on %s side)\n\n", x$k0, x$side))

  cat("Original Estimate:\n")
  cat(sprintf("  Effect: %.3f (95%% CI: %.3f, %.3f)\n",
              x$original_estimate, x$original_ci[1], x$original_ci[2]))

  cat("\nAdjusted Estimate (after filling):\n")
  cat(sprintf("  Effect: %.3f (95%% CI: %.3f, %.3f)\n\n",
              x$filled_estimate, x$filled_ci_lower, x$filled_ci_upper))

  cat(sprintf("Note: %s\n", x$note))
  invisible(x)
}

#' Print Method for Bias Assessment
#' @param x A bias_assessment object
#' @param ... Additional arguments
#' @export
print.bias_assessment <- function(x, ...) {
  cat("\n=== Comprehensive Publication Bias Assessment ===\n\n")

  cat("Original Meta-Analysis:\n")
  cat(sprintf("  Effect: %.3f (95%% CI: %.3f, %.3f), p = %.4f\n\n",
              x$original$estimate, x$original$ci_lower,
              x$original$ci_upper, x$original$p_value))

  if (!is.null(x$egger) && is.null(x$egger$error)) {
    cat("Egger's Test:\n")
    cat(sprintf("  Intercept: %.4f, p = %.4f\n", x$egger$intercept, x$egger$p_value))
    cat(sprintf("  %s\n\n", x$egger$interpretation))
  }

  if (!is.null(x$pet_peese) && is.null(x$pet_peese$error)) {
    cat("PET-PEESE Correction:\n")
    cat(sprintf("  %s estimate: %.3f (95%% CI: %.3f, %.3f)\n\n",
                x$pet_peese$method_used, x$pet_peese$estimate,
                x$pet_peese$ci_lower, x$pet_peese$ci_upper))
  }

  if (!is.null(x$trim_fill) && is.null(x$trim_fill$error)) {
    cat("Trim and Fill:\n")
    cat(sprintf("  Estimated %d missing studies\n", x$trim_fill$k0))
    cat(sprintf("  Adjusted estimate: %.3f (95%% CI: %.3f, %.3f)\n\n",
                x$trim_fill$filled_estimate,
                x$trim_fill$filled_ci_lower,
                x$trim_fill$filled_ci_upper))
  }

  cat(sprintf("Note: %s\n", x$summary$note))
  invisible(x)
}

#' Print Method for Leave-One-Out Analysis
#' @param x A loo_analysis object
#' @param ... Additional arguments
#' @export
print.loo_analysis <- function(x, ...) {
  cat("\nLeave-One-Out Sensitivity Analysis\n")
  cat(sprintf("Number of studies: %d\n\n", x$k))

  cat("Full Model Estimate:\n")
  cat(sprintf("  Effect: %.3f (95%% CI: %.3f, %.3f)\n\n",
              x$full_model$estimate, x$full_model$ci_lower, x$full_model$ci_upper))

  cat("Most Influential Study:\n")
  cat(sprintf("  Excluded: %s\n", x$max_influence$excluded_study))
  cat(sprintf("  Influence: %.3f\n", x$max_influence$influence))
  cat(sprintf("  Effect without this study: %.3f (95%% CI: %.3f, %.3f)\n\n",
              x$max_influence$estimate, x$max_influence$ci_lower, x$max_influence$ci_upper))

  if (nrow(x$influential_studies) > 0) {
    cat(sprintf("Number of influential studies (influence > 1.96): %d\n",
                nrow(x$influential_studies)))
    print(x$influential_studies[, c("excluded_study", "estimate", "influence")])
  } else {
    cat("No studies exceed the influence threshold\n")
  }

  invisible(x)
}

#' Print Method for Influence Diagnostics
#' @param x An influence_diagnostics object
#' @param ... Additional arguments
#' @export
print.influence_diagnostics <- function(x, ...) {
  cat("\nInfluence Diagnostics\n")
  cat(sprintf("Number of studies: %d\n\n", x$k))

  if (nrow(x$outliers) > 0) {
    cat(sprintf("Outliers detected (%d studies):\n", nrow(x$outliers)))
    print(x$outliers[, c("study", "effect", "std_residual")])
    cat("\n")
  } else {
    cat("No outliers detected\n\n")
  }

  if (nrow(x$influential) > 0) {
    cat(sprintf("Influential studies detected (%d studies):\n", nrow(x$influential)))
    print(x$influential[, c("study", "cooks_d", "dfbetas")])
    cat("\n")
  } else {
    cat("No influential studies detected\n\n")
  }

  cat("Thresholds used:\n")
  cat(sprintf("  Outlier (|std residual|): %.2f\n", x$thresholds$outlier_resid))
  cat(sprintf("  Cook's D: %.4f\n", x$thresholds$cooks_d))
  cat(sprintf("  DFBETAS: %.4f\n", x$thresholds$dfbetas))

  invisible(x)
}

#' Print Method for Bootstrap MA
#' @param x A bootstrap_ma object
#' @param ... Additional arguments
#' @export
print.bootstrap_ma <- function(x, ...) {
  cat("\nBootstrap Meta-Analysis\n")
  cat(sprintf("Number of bootstrap samples: %d\n\n", x$n_bootstrap))

  cat("Original Estimate:\n")
  cat(sprintf("  Effect: %.3f (SE = %.3f)\n\n", x$original_estimate, x$original_se))

  cat(sprintf("Bootstrap %d%% Confidence Interval:\n", x$ci_level * 100))
  cat(sprintf("  (%.3f, %.3f)\n\n", x$bootstrap_ci[1], x$bootstrap_ci[2]))

  cat(sprintf("Bootstrap SE: %.3f\n", x$bootstrap_se))

  invisible(x)
}

#' Print Method for DTA Bivariate
#' @param x A dta_bivariate object
#' @param ... Additional arguments
#' @export
print.dta_bivariate <- function(x, ...) {
  cat("\nBivariate Meta-Analysis of Diagnostic Test Accuracy\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Number of studies: %d\n\n", x$k))

  cat("Summary Estimates:\n")
  cat(sprintf("  Sensitivity: %.3f (95%% CI: %.3f, %.3f)\n",
              x$summary_sens, x$sens_ci_lower, x$sens_ci_upper))
  cat(sprintf("  Specificity: %.3f (95%% CI: %.3f, %.3f)\n\n",
              x$summary_spec, x$spec_ci_lower, x$spec_ci_upper))

  cat(sprintf("Correlation between logit(sens) and logit(spec): %.3f\n\n", x$correlation))

  # Calculate summary likelihood ratios
  lr <- likelihood_ratios(x$summary_sens, x$summary_spec)
  cat("Summary Likelihood Ratios:\n")
  cat(sprintf("  Positive LR: %.2f (%s)\n", lr$PLR, lr$interpretation$PLR))
  cat(sprintf("  Negative LR: %.2f (%s)\n\n", lr$NLR, lr$interpretation$NLR))

  # Calculate DOR
  dor_val <- diagnostic_or(x$summary_sens, x$summary_spec)
  cat(sprintf("Diagnostic Odds Ratio: %.2f\n\n", dor_val))

  cat(sprintf("Note: %s\n", x$note))

  invisible(x)
}
