#' Advanced Heterogeneity Estimators Module
#'
#' Comprehensive collection of tau-squared estimators for random-effects meta-analysis.
#' Goes beyond standard DerSimonian-Laird and REML methods.
#'
#' Estimators included:
#' - Paule-Mandel (PM) - iterative, moment-based
#' - Sidik-Jonkman (SJ) - robust, model-error variance
#' - Empirical Bayes (EB) - maximum likelihood
#' - Hunter-Schmidt (HS) - variance-component
#' - Hedges (HE) - ANOVA-type
#' - Maximum Likelihood (ML)
#' - Restricted Maximum Likelihood (REML) - improved
#' - DerSimonian-Laird (DL) - classic
#' - Hartung-Knapp-Sidik-Jonkman (HKSJ) small-sample adjustment
#'
#' Based on methodological reviews: Veroniki et al. (2016), Langan et al. (2019),
#' IntHout et al. (2014).
#'
#' @name advanced_heterogeneity_estimators
NULL


#' Meta-Analysis with Advanced Heterogeneity Estimators
#'
#' Performs random-effects meta-analysis using state-of-the-art heterogeneity estimators.
#' Provides more accurate inference, especially for small sample sizes.
#'
#' @param data Data frame with meta-analysis data
#' @param method Heterogeneity estimator: "PM" (Paule-Mandel), "SJ" (Sidik-Jonkman),
#'   "EB" (Empirical Bayes), "ML", "REML", "DL", "HS", "HE" (default: "PM")
#' @param test Test for pooled effect: "z" or "knha" (Knapp-Hartung, default: "knha")
#' @param prediction_interval Calculate 95% prediction interval (default: TRUE)
#' @param compare_methods Compare multiple estimators (default: FALSE)
#'
#' @return Meta-analysis result with advanced heterogeneity estimation
#'
#' @references
#' Veroniki AA et al. (2016). Methods to estimate the between-study variance and
#' its uncertainty in meta-analysis. Res Synth Methods, 7(1):55-79.
#'
#' Langan D et al. (2019). A comparison of heterogeneity variance estimators in
#' simulated random-effects meta-analyses. Res Synth Methods, 10(1):83-98.
#'
#' IntHout J et al. (2014). The Hartung-Knapp-Sidik-Jonkman method for random
#' effects meta-analysis is straightforward and considerably outperforms the
#' standard DerSimonian-Laird method. BMC Med Res Methodol, 14:25.
#'
#' @export
#' @examples
#' \dontrun{
#' # Paule-Mandel with Knapp-Hartung adjustment
#' result <- meta_analysis_advanced(
#'   data = ma_data,
#'   method = "PM",
#'   test = "knha"
#' )
#'
#' # Compare multiple estimators
#' comparison <- meta_analysis_advanced(
#'   data = ma_data,
#'   compare_methods = TRUE
#' )
#' }
meta_analysis_advanced <- function(data, method = "PM", test = "knha",
                                   prediction_interval = TRUE,
                                   compare_methods = FALSE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   META-ANALYSIS: ADVANCED HETEROGENEITY ESTIMATION           ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Remove missing values
  complete_idx <- complete.cases(data[, c("effect", "se")])
  if (!all(complete_idx)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_idx)))
    data <- data[complete_idx, ]
  }

  y <- data$effect
  v <- data$se^2
  k <- length(y)

  cat(sprintf("Studies: %d\n", k))

  if (compare_methods) {
    cat("\nComparing heterogeneity estimators...\n")

    methods <- c("DL", "PM", "REML", "ML", "EB", "SJ", "HS", "HE")
    results_list <- list()

    for (m in methods) {
      result <- estimate_heterogeneity(y, v, m)
      results_list[[m]] <- result
      cat(sprintf("  %s: τ² = %.4f\n", m, result$tau2))
    }

    # Compare estimates
    comparison_df <- data.frame(
      Method = methods,
      tau2 = sapply(results_list, function(x) x$tau2),
      tau = sapply(results_list, function(x) x$tau),
      I2 = sapply(results_list, function(x) x$I2),
      estimate = sapply(results_list, function(x) x$estimate),
      se = sapply(results_list, function(x) x$se)
    )

    cat("\nComparison Table:\n")
    print(comparison_df)

    return(list(
      comparison = comparison_df,
      results = results_list,
      data = data
    ))
  }

  # Single method analysis
  cat(sprintf("Method: %s\n", method))
  cat(sprintf("Test: %s\n", test))

  result <- estimate_heterogeneity(y, v, method)

  cat(sprintf("\nHeterogeneity: τ² = %.4f, I² = %.1f%%\n",
              result$tau2, result$I2))
  cat(sprintf("Pooled estimate: %.3f (95%% CI: %.3f, %.3f)\n",
              result$estimate, result$ci_lower, result$ci_upper))

  # Apply Hartung-Knapp-Sidik-Jonkman adjustment if requested
  if (test == "knha" || test == "hksj") {
    cat("\nApplying Hartung-Knapp-Sidik-Jonkman adjustment...\n")
    result <- apply_hksj_adjustment(result, y, v)

    cat(sprintf("Adjusted 95%% CI: (%.3f, %.3f)\n",
                result$ci_lower_hksj, result$ci_upper_hksj))
    cat(sprintf("Adjusted p-value: %.4f\n", result$p_value_hksj))
  }

  # Prediction interval
  if (prediction_interval) {
    cat("\nCalculating prediction interval...\n")
    result$prediction_interval <- calculate_prediction_interval(
      result$estimate, result$se, result$tau, k
    )

    cat(sprintf("95%% Prediction interval: (%.3f, %.3f)\n",
                result$prediction_interval$pi_lower,
                result$prediction_interval$pi_upper))
  }

  result$method <- method
  result$test <- test
  result$data <- data
  result$k <- k

  class(result) <- c("meta_advanced", "list")

  cat("\n✓ Meta-analysis completed\n")
  return(result)
}


#' Estimate Heterogeneity (Multiple Methods)
#'
#' @keywords internal
estimate_heterogeneity <- function(y, v, method) {

  k <- length(y)

  # Fixed-effect estimate for Q statistic
  wi_fe <- 1 / v
  theta_fe <- sum(wi_fe * y) / sum(wi_fe)
  Q <- sum(wi_fe * (y - theta_fe)^2)
  df <- k - 1

  # Estimate tau-squared using specified method
  if (method == "DL") {
    tau2 <- estimate_tau2_dl(y, v, Q, df)
  } else if (method == "PM") {
    tau2 <- estimate_tau2_pm(y, v)
  } else if (method == "REML") {
    tau2 <- estimate_tau2_reml(y, v)
  } else if (method == "ML") {
    tau2 <- estimate_tau2_ml(y, v)
  } else if (method == "EB") {
    tau2 <- estimate_tau2_eb(y, v)
  } else if (method == "SJ") {
    tau2 <- estimate_tau2_sj(y, v)
  } else if (method == "HS") {
    tau2 <- estimate_tau2_hs(y, v)
  } else if (method == "HE") {
    tau2 <- estimate_tau2_he(y, v)
  } else {
    stop("Unknown method. Use: DL, PM, REML, ML, EB, SJ, HS, HE")
  }

  # Random-effects estimate
  wi <- 1 / (v + tau2)
  theta <- sum(wi * y) / sum(wi)
  se_theta <- sqrt(1 / sum(wi))

  ci_lower <- theta - qnorm(0.975) * se_theta
  ci_upper <- theta + qnorm(0.975) * se_theta
  p_value <- 2 * pnorm(-abs(theta / se_theta))

  # I-squared
  I2 <- max(0, 100 * (Q - df) / Q)

  # H-squared
  H2 <- Q / df

  return(list(
    estimate = theta,
    se = se_theta,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    tau2 = tau2,
    tau = sqrt(tau2),
    I2 = I2,
    H2 = H2,
    Q = Q,
    Q_df = df,
    Q_pval = pchisq(Q, df, lower.tail = FALSE),
    weights = wi / sum(wi)
  ))
}


#' DerSimonian-Laird Estimator
#'
#' @keywords internal
estimate_tau2_dl <- function(y, v, Q, df) {
  k <- length(y)
  wi <- 1 / v
  C <- sum(wi) - sum(wi^2) / sum(wi)
  tau2 <- max(0, (Q - df) / C)
  return(tau2)
}


#' Paule-Mandel Estimator
#'
#' Iterative moment-based estimator. Recommended by many recent reviews.
#'
#' @keywords internal
estimate_tau2_pm <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)

  # Initialize with DL estimate
  wi_fe <- 1 / v
  theta_fe <- sum(wi_fe * y) / sum(wi_fe)
  Q <- sum(wi_fe * (y - theta_fe)^2)
  df <- k - 1

  tau2 <- estimate_tau2_dl(y, v, Q, df)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    # Update weights and estimate
    wi <- 1 / (v + tau2)
    theta <- sum(wi * y) / sum(wi)

    # Update Q
    Q_new <- sum(wi * (y - theta)^2)

    # Solve for tau2: Q = k - 1
    # Q = sum(wi * (y - theta)^2) should equal k - 1
    # Iteratively adjust tau2

    if (abs(Q_new - df) < tol) {
      break
    }

    # Newton-Raphson update
    # Derivative of Q w.r.t. tau2
    dQ <- -sum((y - theta)^2 / (v + tau2)^2)

    tau2 <- max(0, tau2 - (Q_new - df) / dQ)

    if (abs(tau2 - tau2_old) < tol) {
      break
    }
  }

  return(tau2)
}


#' REML Estimator (Improved)
#'
#' @keywords internal
estimate_tau2_reml <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)

  # Initialize
  tau2 <- 0.5 * var(y)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    wi <- 1 / (v + tau2)
    sum_wi <- sum(wi)
    theta <- sum(wi * y) / sum_wi

    # REML score equation
    # d(REML log-lik) / d(tau2) = 0

    resid <- y - theta
    score <- -0.5 * sum(wi) + 0.5 * sum(wi^2) / sum_wi +
             0.5 * sum(wi^2 * resid^2)

    # Second derivative (Fisher information)
    info <- 0.5 * sum(wi^2) - sum(wi^2)^2 / sum_wi -
            sum(wi^3 * resid^2)

    # Newton-Raphson update
    tau2 <- max(0, tau2 - score / info)

    if (abs(tau2 - tau2_old) < tol) {
      break
    }
  }

  return(tau2)
}


#' ML Estimator
#'
#' @keywords internal
estimate_tau2_ml <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)
  tau2 <- 0.5 * var(y)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    wi <- 1 / (v + tau2)
    theta <- sum(wi * y) / sum(wi)

    resid <- y - theta

    # ML score equation
    score <- -0.5 * sum(wi) + 0.5 * sum(wi^2 * resid^2)

    # Second derivative
    info <- 0.5 * sum(wi^2) - sum(wi^3 * resid^2)

    tau2 <- max(0, tau2 - score / info)

    if (abs(tau2 - tau2_old) < tol) {
      break
    }
  }

  return(tau2)
}


#' Empirical Bayes Estimator
#'
#' @keywords internal
estimate_tau2_eb <- function(y, v) {
  # Similar to ML but with Bayesian interpretation
  return(estimate_tau2_ml(y, v))
}


#' Sidik-Jonkman Estimator
#'
#' Robust estimator based on model-error variance.
#'
#' @keywords internal
estimate_tau2_sj <- function(y, v) {

  k <- length(y)

  # Weighted mean
  wi <- 1 / v
  theta_w <- sum(wi * y) / sum(wi)

  # Typical variance
  # tau2 = sum((y - theta_w)^2 / (k - 1)) - mean(v)

  s2 <- sum((y - theta_w)^2) / (k - 1)

  tau2 <- max(0, s2 - mean(v))

  return(tau2)
}


#' Hunter-Schmidt Estimator
#'
#' @keywords internal
estimate_tau2_hs <- function(y, v) {

  k <- length(y)
  wi <- 1 / v
  theta <- sum(wi * y) / sum(wi)

  # Variance component method
  var_obs <- sum(wi * (y - theta)^2) / sum(wi)
  var_exp <- sum(wi * v) / sum(wi)

  tau2 <- max(0, var_obs - var_exp)

  return(tau2)
}


#' Hedges Estimator (ANOVA-type)
#'
#' @keywords internal
estimate_tau2_he <- function(y, v) {

  k <- length(y)

  # Simple ANOVA-type estimator
  # Between-study variance from ANOVA

  grand_mean <- mean(y)
  ss_between <- sum((y - grand_mean)^2)
  ms_between <- ss_between / (k - 1)

  # Within-study variance
  ms_within <- mean(v)

  tau2 <- max(0, (ms_between - ms_within) / k)

  return(tau2)
}


#' Apply Hartung-Knapp-Sidik-Jonkman Adjustment
#'
#' Small-sample adjustment for more accurate confidence intervals.
#' Uses t-distribution and adjusted variance.
#'
#' @keywords internal
apply_hksj_adjustment <- function(result, y, v) {

  k <- length(y)
  tau2 <- result$tau2

  # Recalculate with adjusted variance
  wi <- 1 / (v + tau2)
  theta <- sum(wi * y) / sum(wi)

  # Adjusted variance estimator
  resid <- y - theta
  s2 <- sum(wi * resid^2) / (k - 1)

  se_adj <- sqrt(s2 / sum(wi))

  # Use t-distribution with k-1 df
  t_crit <- qt(0.975, k - 1)

  ci_lower_hksj <- theta - t_crit * se_adj
  ci_upper_hksj <- theta + t_crit * se_adj
  p_value_hksj <- 2 * pt(-abs(theta / se_adj), k - 1)

  result$se_hksj <- se_adj
  result$ci_lower_hksj <- ci_lower_hksj
  result$ci_upper_hksj <- ci_upper_hksj
  result$p_value_hksj <- p_value_hksj
  result$df_hksj <- k - 1

  return(result)
}


#' Calculate Prediction Interval
#'
#' 95% prediction interval for the effect in a new study.
#' Accounts for both estimation uncertainty and heterogeneity.
#'
#' @keywords internal
calculate_prediction_interval <- function(estimate, se, tau, k) {

  # Prediction interval variance
  # Var(new study) = se^2 + tau^2

  se_pi <- sqrt(se^2 + tau^2)

  # Use t-distribution for small k
  if (k <= 30) {
    t_crit <- qt(0.975, k - 2)
  } else {
    t_crit <- qnorm(0.975)
  }

  pi_lower <- estimate - t_crit * se_pi
  pi_upper <- estimate + t_crit * se_pi

  return(list(
    pi_lower = pi_lower,
    pi_upper = pi_upper,
    se_pi = se_pi
  ))
}


#' Print Method for Advanced Meta-Analysis
#'
#' @export
print.meta_advanced <- function(x, ...) {
  cat("Random-Effects Meta-Analysis\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Test: %s\n", x$test))
  cat(sprintf("Studies: %d\n\n", x$k))

  cat("Pooled Estimate:\n")
  cat(sprintf("  Effect: %.3f\n", x$estimate))
  cat(sprintf("  SE: %.4f\n", x$se))

  if (!is.null(x$ci_lower_hksj)) {
    cat(sprintf("  95%% CI (HKSJ): (%.3f, %.3f)\n", x$ci_lower_hksj, x$ci_upper_hksj))
    cat(sprintf("  p-value (HKSJ): %.4f\n", x$p_value_hksj))
  } else {
    cat(sprintf("  95%% CI: (%.3f, %.3f)\n", x$ci_lower, x$ci_upper))
    cat(sprintf("  p-value: %.4f\n", x$p_value))
  }

  cat("\nHeterogeneity:\n")
  cat(sprintf("  τ² = %.4f, τ = %.4f\n", x$tau2, x$tau))
  cat(sprintf("  I² = %.1f%%, H² = %.2f\n", x$I2, x$H2))
  cat(sprintf("  Q = %.2f (df = %d, p = %.4f)\n", x$Q, x$Q_df, x$Q_pval))

  if (!is.null(x$prediction_interval)) {
    cat("\nPrediction Interval (95%):\n")
    cat(sprintf("  (%.3f, %.3f)\n", x$prediction_interval$pi_lower,
                x$prediction_interval$pi_upper))
  }

  invisible(x)
}
