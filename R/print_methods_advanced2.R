#' Print Methods for Advanced Classes (Part 2)
#'
#' @name print_methods_advanced2
NULL

#' Print Method for Network Meta-Analysis
#' @param x An nma object
#' @param ... Additional arguments
#' @export
print.nma <- function(x, ...) {
  cat("\nNetwork Meta-Analysis\n")
  cat(sprintf("Number of treatments: %d\n", x$n_treatments))
  cat(sprintf("Reference treatment: %s\n", x$reference))
  cat(sprintf("Number of studies: %d\n", length(unique(x$network_structure$n_studies))))
  cat("\nTreatment Effects (vs Reference):\n")
  print(x$treatment_effects[, c("treatment", "estimate", "ci_lower", "ci_upper")])
  cat("\nTreatment Ranking:\n")
  print(x$ranking[1:min(5, nrow(x$ranking)), c("rank", "treatment", "estimate")])
  cat(sprintf("\nNote: %s\n", x$note))
  invisible(x)
}

#' Print Method for Bayesian Meta-Analysis
#' @param x A bayesian_ma object
#' @param ... Additional arguments
#' @export
print.bayesian_ma <- function(x, ...) {
  cat("\nBayesian Random-Effects Meta-Analysis\n")
  cat(sprintf("MCMC iterations: %d (burn-in: %d, thinning: %d)\n",
              x$settings$n_iter, x$settings$n_burn, x$settings$n_thin))
  cat(sprintf("Posterior samples: %d\n\n", x$settings$n_samples))

  cat("Posterior Summary:\n")
  print(x$summary, row.names = FALSE)

  cat("\nMCMC Diagnostics:\n")
  cat(sprintf("Acceptance rate (theta): %.3f\n", x$diagnostics$acceptance_rate_theta))
  cat(sprintf("Acceptance rate (tau): %.3f\n", x$diagnostics$acceptance_rate_tau))
  cat(sprintf("%s\n", x$diagnostics$note))

  invisible(x)
}

#' Print Method for IPD One-Stage
#' @param x An ipd_onestage object
#' @param ... Additional arguments
#' @export
print.ipd_onestage <- function(x, ...) {
  cat("\nOne-Stage IPD Meta-Analysis\n")
  cat(sprintf("Number of studies: %d\n", x$n_studies))
  cat(sprintf("Total participants: %d\n", x$n_participants))
  cat(sprintf("Model family: %s\n\n", x$family))

  cat("Overall Treatment Effect:\n")
  cat(sprintf("  Estimate: %.3f (95%% CI: %.3f, %.3f)\n",
              x$overall_effect$estimate,
              x$overall_effect$ci_lower,
              x$overall_effect$ci_upper))
  cat(sprintf("  p-value: %.4f\n\n", x$overall_effect$p_value))

  if (!is.null(x$study_effects)) {
    cat(sprintf("Between-study heterogeneity: τ² = %.4f\n", x$heterogeneity$tau2))
  }

  if (!is.null(x$covariate_effects)) {
    cat("\nCovariate Effects:\n")
    print(x$covariate_effects, row.names = FALSE)
  }

  cat(sprintf("\nNote: %s\n", x$note))
  invisible(x)
}

#' Print Method for IPD Two-Stage
#' @param x An ipd_twostage object
#' @param ... Additional arguments
#' @export
print.ipd_twostage <- function(x, ...) {
  cat("\nTwo-Stage IPD Meta-Analysis\n")
  cat(sprintf("Number of studies: %d\n", x$n_studies))
  cat(sprintf("Total participants: %d\n\n", x$total_participants))

  cat("Pooled Treatment Effect:\n")
  cat(sprintf("  Estimate: %.3f (95%% CI: %.3f, %.3f)\n",
              x$pooled_result$estimate,
              x$pooled_result$ci_lower,
              x$pooled_result$ci_upper))
  cat(sprintf("  p-value: %.4f\n\n", x$pooled_result$p_value))

  cat(sprintf("Heterogeneity: I² = %.1f%%, τ² = %.4f\n",
              x$pooled_result$I2, x$pooled_result$tau2))

  invisible(x)
}

#' Print Method for Dose-Response MA
#' @param x A dose_response_ma object
#' @param ... Additional arguments
#' @export
print.dose_response_ma <- function(x, ...) {
  cat("\nDose-Response Meta-Analysis\n")
  cat(sprintf("Model type: %s\n", x$model_type))
  cat(sprintf("Reference dose: %.2f\n\n", x$reference_dose))

  cat("Model Coefficients:\n")
  print(x$coefficients)

  cat("\nHeterogeneity:\n")
  cat(sprintf("  I² = %.1f%%\n", x$heterogeneity$I2))
  cat(sprintf("  Q = %.2f (df = %d, p = %.4f)\n",
              x$heterogeneity$Q, x$heterogeneity$df, x$heterogeneity$p_value))

  cat("\nDose Range:\n")
  cat(sprintf("  Min: %.2f, Max: %.2f\n",
              min(x$data$dose), max(x$data$dose)))

  invisible(x)
}

#' Print Method for Bayesian Meta-Regression
#' @param x A bayesian_metareg object
#' @param ... Additional arguments
#' @export
print.bayesian_metareg <- function(x, ...) {
  cat("\nBayesian Meta-Regression\n\n")
  cat("Posterior Summary:\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' Print Method for IPD Interaction
#' @param x An ipd_interaction object
#' @param ... Additional arguments
#' @export
print.ipd_interaction <- function(x, ...) {
  cat("\nIPD Treatment-Covariate Interaction Analysis\n")
  cat(sprintf("Moderator: %s\n\n", x$moderator))

  if (!is.null(x$interaction$error)) {
    cat("Error:", x$interaction$error, "\n")
  } else {
    cat("Interaction Effect:\n")
    cat(sprintf("  Estimate: %.3f (SE = %.3f)\n",
                x$interaction$estimate, x$interaction$se))
    cat(sprintf("  t-statistic: %.3f\n", x$interaction$t_stat))
    cat(sprintf("  p-value: %.4f\n\n", x$interaction$p_value))
    cat(sprintf("Interpretation: %s\n", x$interaction$interpretation))
  }

  invisible(x)
}

#' Plot Method for Bayesian Meta-Analysis
#' @param x A bayesian_ma object
#' @param ... Additional arguments
#' @export
plot.bayesian_ma <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2))

  # Trace plot for theta
  plot(x$posterior_samples[, "theta"], type = "l",
       main = "Trace Plot: Effect Size (theta)",
       xlab = "Iteration", ylab = "theta")

  # Density plot for theta
  plot(density(x$posterior_samples[, "theta"]),
       main = "Posterior: Effect Size",
       xlab = "theta", ylab = "Density")
  abline(v = mean(x$posterior_samples[, "theta"]), col = "red", lty = 2)

  # Trace plot for tau
  plot(x$posterior_samples[, "tau"], type = "l",
       main = "Trace Plot: Heterogeneity (tau)",
       xlab = "Iteration", ylab = "tau")

  # Density plot for tau
  plot(density(x$posterior_samples[, "tau"]),
       main = "Posterior: Heterogeneity",
       xlab = "tau", ylab = "Density")
  abline(v = mean(x$posterior_samples[, "tau"]), col = "red", lty = 2)

  invisible(NULL)
}

#' Plot Method for Dose-Response
#' @param x A dose_response_ma object
#' @param ... Additional arguments
#' @export
plot.dose_response_ma <- function(x, ...) {
  plot_dose_response(x, ...)
}
