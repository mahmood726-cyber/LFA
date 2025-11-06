#' Core Statistical Functions Module
#'
#' Consolidated statistical functions used across multiple meta-analysis methods.
#' This module reduces code duplication and improves maintainability.
#'
#' @name core_statistics
NULL


#' Calculate Pooled Effect Size (Random Effects)
#'
#' Core function for random-effects meta-analysis pooling. Used by multiple modules.
#'
#' @param y Vector of effect sizes
#' @param v Vector of variances
#' @param method Method for tau-squared estimation: "DL", "REML", "PM", "ML"
#' @param conf_level Confidence level (default: 0.95)
#'
#' @return List with pooled estimate, SE, CI, tau2, weights
#'
#' @export
#' @examples
#' \dontrun{
#' y <- c(0.5, 0.3, 0.7, 0.4)
#' v <- c(0.01, 0.02, 0.015, 0.018)
#' result <- pool_effects_random(y, v, method = "DL")
#' }
pool_effects_random <- function(y, v, method = "DL", conf_level = 0.95) {

  k <- length(y)

  if (k < 2) {
    stop("Need at least 2 studies for pooling")
  }

  if (length(v) != k) {
    stop("Length of y and v must match")
  }

  # Fixed-effect estimate for Q
  wi_fe <- 1 / v
  theta_fe <- sum(wi_fe * y) / sum(wi_fe)
  Q <- sum(wi_fe * (y - theta_fe)^2)
  df <- k - 1

  # Estimate tau-squared
  tau2 <- estimate_tau2(y, v, method, Q, df)

  # Random-effects pooling
  wi <- 1 / (v + tau2)
  theta <- sum(wi * y) / sum(wi)
  se_theta <- sqrt(1 / sum(wi))

  # Confidence interval
  z_crit <- qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- theta - z_crit * se_theta
  ci_upper <- theta + z_crit * se_theta

  # P-value
  p_value <- 2 * pnorm(-abs(theta / se_theta))

  # I-squared
  I2 <- max(0, 100 * (Q - df) / Q)

  return(list(
    estimate = theta,
    se = se_theta,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    tau2 = tau2,
    tau = sqrt(tau2),
    I2 = I2,
    Q = Q,
    Q_df = df,
    Q_pval = pchisq(Q, df, lower.tail = FALSE),
    weights = wi / sum(wi),
    method = method,
    k = k
  ))
}


#' Estimate Between-Study Variance (Tau-Squared)
#'
#' Unified interface for multiple tau-squared estimators.
#'
#' @param y Effect sizes
#' @param v Variances
#' @param method Estimation method
#' @param Q Q statistic (pre-computed, optional)
#' @param df Degrees of freedom (optional)
#'
#' @return Tau-squared estimate
#'
#' @keywords internal
estimate_tau2 <- function(y, v, method, Q = NULL, df = NULL) {

  k <- length(y)

  # Compute Q if not provided
  if (is.null(Q)) {
    wi_fe <- 1 / v
    theta_fe <- sum(wi_fe * y) / sum(wi_fe)
    Q <- sum(wi_fe * (y - theta_fe)^2)
    df <- k - 1
  }

  if (method == "DL") {
    # DerSimonian-Laird
    wi <- 1 / v
    C <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - df) / C)

  } else if (method == "REML") {
    # REML (iterative)
    tau2 <- estimate_tau2_reml_iter(y, v)

  } else if (method == "PM") {
    # Paule-Mandel (iterative)
    tau2 <- estimate_tau2_pm_iter(y, v)

  } else if (method == "ML") {
    # Maximum Likelihood
    tau2 <- estimate_tau2_ml_iter(y, v)

  } else {
    stop("Unknown method: ", method)
  }

  return(tau2)
}


#' REML Tau-Squared (Iterative)
#'
#' @keywords internal
estimate_tau2_reml_iter <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)
  tau2 <- 0.5 * var(y)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    wi <- 1 / (v + tau2)
    sum_wi <- sum(wi)
    theta <- sum(wi * y) / sum_wi
    resid <- y - theta

    # REML score equation
    score <- -0.5 * sum(wi) + 0.5 * sum(wi^2) / sum_wi + 0.5 * sum(wi^2 * resid^2)

    # Fisher information
    info <- 0.5 * sum(wi^2) - sum(wi^2)^2 / sum_wi - sum(wi^3 * resid^2)

    # Newton-Raphson
    tau2 <- max(0, tau2 - score / info)

    if (abs(tau2 - tau2_old) < tol) break
  }

  return(tau2)
}


#' Paule-Mandel Tau-Squared (Iterative)
#'
#' @keywords internal
estimate_tau2_pm_iter <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)
  df <- k - 1

  # Initialize with DL
  wi_fe <- 1 / v
  theta_fe <- sum(wi_fe * y) / sum(wi_fe)
  Q <- sum(wi_fe * (y - theta_fe)^2)
  C <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
  tau2 <- max(0, (Q - df) / C)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    wi <- 1 / (v + tau2)
    theta <- sum(wi * y) / sum(wi)
    Q_new <- sum(wi * (y - theta)^2)

    if (abs(Q_new - df) < tol) break

    # Newton-Raphson update
    dQ <- -sum((y - theta)^2 / (v + tau2)^2)
    tau2 <- max(0, tau2 - (Q_new - df) / dQ)

    if (abs(tau2 - tau2_old) < tol) break
  }

  return(tau2)
}


#' ML Tau-Squared (Iterative)
#'
#' @keywords internal
estimate_tau2_ml_iter <- function(y, v, max_iter = 100, tol = 1e-6) {

  k <- length(y)
  tau2 <- 0.5 * var(y)

  for (iter in 1:max_iter) {
    tau2_old <- tau2

    wi <- 1 / (v + tau2)
    theta <- sum(wi * y) / sum(wi)
    resid <- y - theta

    # ML score
    score <- -0.5 * sum(wi) + 0.5 * sum(wi^2 * resid^2)

    # Fisher information
    info <- 0.5 * sum(wi^2) - sum(wi^3 * resid^2)

    tau2 <- max(0, tau2 - score / info)

    if (abs(tau2 - tau2_old) < tol) break
  }

  return(tau2)
}


#' Calculate Effect Size from Summary Statistics
#'
#' Common effect size calculations used across modules.
#'
#' @param n1 Sample size group 1
#' @param n2 Sample size group 2
#' @param mean1 Mean group 1
#' @param mean2 Mean group 2
#' @param sd1 SD group 1
#' @param sd2 SD group 2
#' @param type Effect size type: "SMD" (Cohen's d), "MD", "OR", "RR"
#'
#' @return List with effect size and variance
#'
#' @export
calculate_effect_size <- function(n1, n2, mean1, mean2, sd1, sd2, type = "SMD") {

  if (type == "SMD" || type == "Cohen's d") {
    # Standardized mean difference (Hedges' g)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    d <- (mean1 - mean2) / pooled_sd

    # Small sample bias correction (Hedges' g)
    J <- 1 - 3 / (4 * (n1 + n2 - 2) - 1)
    g <- J * d

    # Variance
    var_g <- (n1 + n2) / (n1 * n2) + g^2 / (2 * (n1 + n2))

    return(list(effect = g, variance = var_g, se = sqrt(var_g)))

  } else if (type == "MD") {
    # Mean difference
    md <- mean1 - mean2

    # Variance
    var_md <- sd1^2 / n1 + sd2^2 / n2

    return(list(effect = md, variance = var_md, se = sqrt(var_md)))

  } else {
    stop("Unsupported effect size type: ", type)
  }
}


#' Calculate Confidence Interval
#'
#' Generic CI calculation with multiple methods.
#'
#' @param estimate Point estimate
#' @param se Standard error
#' @param conf_level Confidence level (default: 0.95)
#' @param method "normal" or "t"
#' @param df Degrees of freedom (for t method)
#'
#' @return List with ci_lower and ci_upper
#'
#' @export
calculate_ci <- function(estimate, se, conf_level = 0.95, method = "normal", df = NULL) {

  alpha <- 1 - conf_level

  if (method == "normal") {
    z_crit <- qnorm(1 - alpha / 2)
    ci_lower <- estimate - z_crit * se
    ci_upper <- estimate + z_crit * se

  } else if (method == "t") {
    if (is.null(df)) stop("df required for t method")
    t_crit <- qt(1 - alpha / 2, df)
    ci_lower <- estimate - t_crit * se
    ci_upper <- estimate + t_crit * se

  } else {
    stop("Unknown method: ", method)
  }

  return(list(ci_lower = ci_lower, ci_upper = ci_upper))
}


#' Calculate Prediction Interval
#'
#' Prediction interval for effect in a new study.
#'
#' @param estimate Pooled effect
#' @param se Standard error of pooled effect
#' @param tau Between-study SD
#' @param k Number of studies
#' @param conf_level Confidence level
#'
#' @return List with pi_lower and pi_upper
#'
#' @export
calculate_prediction_interval <- function(estimate, se, tau, k, conf_level = 0.95) {

  # Prediction variance
  var_pred <- se^2 + tau^2
  se_pred <- sqrt(var_pred)

  # Use t-distribution for small k
  if (k <= 30) {
    df <- k - 1
    t_crit <- qt(1 - (1 - conf_level) / 2, df)
    pi_lower <- estimate - t_crit * se_pred
    pi_upper <- estimate + t_crit * se_pred
  } else {
    z_crit <- qnorm(1 - (1 - conf_level) / 2)
    pi_lower <- estimate - z_crit * se_pred
    pi_upper <- estimate + z_crit * se_pred
  }

  return(list(
    pi_lower = pi_lower,
    pi_upper = pi_upper,
    se_pred = se_pred
  ))
}


#' Calculate Heterogeneity Statistics
#'
#' Q, I², H², tau for heterogeneity assessment.
#'
#' @param y Effect sizes
#' @param v Variances
#' @param tau2 Tau-squared estimate (optional, will be estimated if NULL)
#'
#' @return List with Q, I2, H2, tau2, tau
#'
#' @export
calculate_heterogeneity_stats <- function(y, v, tau2 = NULL) {

  k <- length(y)

  # Fixed-effect estimate
  wi_fe <- 1 / v
  theta_fe <- sum(wi_fe * y) / sum(wi_fe)

  # Q statistic
  Q <- sum(wi_fe * (y - theta_fe)^2)
  df <- k - 1
  Q_pval <- pchisq(Q, df, lower.tail = FALSE)

  # I-squared
  I2 <- max(0, 100 * (Q - df) / Q)

  # H-squared
  H2 <- Q / df

  # Tau-squared (if not provided)
  if (is.null(tau2)) {
    C <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
    tau2 <- max(0, (Q - df) / C)
  }

  return(list(
    Q = Q,
    Q_df = df,
    Q_pval = Q_pval,
    I2 = I2,
    H2 = H2,
    tau2 = tau2,
    tau = sqrt(tau2)
  ))
}


#' Validate Meta-Analysis Data
#'
#' Common validation checks for meta-analysis datasets.
#'
#' @param data Data frame
#' @param required_cols Character vector of required column names
#' @param numeric_cols Character vector of columns that must be numeric
#' @param positive_cols Character vector of columns that must be positive
#'
#' @return TRUE if valid, stops with error otherwise
#'
#' @export
validate_meta_data <- function(data, required_cols, numeric_cols = NULL, positive_cols = NULL) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Check required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check numeric columns
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (!is.numeric(data[[col]])) {
        stop("Column '", col, "' must be numeric")
      }
    }
  }

  # Check positive columns
  if (!is.null(positive_cols)) {
    for (col in positive_cols) {
      if (any(data[[col]] <= 0, na.rm = TRUE)) {
        stop("Column '", col, "' must contain only positive values")
      }
    }
  }

  # Check for missing values
  complete_cases <- complete.cases(data[, required_cols])
  if (!all(complete_cases)) {
    warning(sprintf("Data contains %d rows with missing values in required columns",
                    sum(!complete_cases)))
  }

  return(TRUE)
}


#' Remove Outliers
#'
#' Common outlier detection and removal.
#'
#' @param y Effect sizes
#' @param method Method: "iqr", "zscore", "cook"
#' @param threshold Threshold for outlier detection
#'
#' @return Logical vector indicating which observations are outliers
#'
#' @export
detect_outliers <- function(y, method = "iqr", threshold = 1.5) {

  if (method == "iqr") {
    q1 <- quantile(y, 0.25)
    q3 <- quantile(y, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - threshold * iqr
    upper_bound <- q3 + threshold * iqr
    outliers <- y < lower_bound | y > upper_bound

  } else if (method == "zscore") {
    z <- (y - mean(y)) / sd(y)
    outliers <- abs(z) > threshold

  } else {
    stop("Unknown outlier detection method: ", method)
  }

  return(outliers)
}
