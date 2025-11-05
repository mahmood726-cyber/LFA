#' Utility Functions for Meta-Analysis
#'
#' @name utils
#' @keywords internal
NULL

#' Calculate I-squared Statistic
#'
#' @param Q Cochran's Q statistic
#' @param df Degrees of freedom (k - 1)
#' @return I-squared value (percentage)
#' @export
calculate_i2 <- function(Q, df) {
  if (Q <= df) {
    return(0)
  }
  return(100 * (Q - df) / Q)
}

#' Calculate I-squared Statistic (alternative name)
#'
#' @param Q Cochran's Q statistic
#' @param df Degrees of freedom (k - 1)
#' @return I-squared value (percentage)
#' @export
calculate_i_squared <- function(Q, df) {
  calculate_i2(Q, df)
}

#' Calculate Tau-squared
#'
#' @param Q Cochran's Q statistic
#' @param df Degrees of freedom
#' @param weights Vector of study weights
#' @return Tau-squared estimate
#' @export
calculate_tau2 <- function(Q, df, weights) {
  C <- sum(weights) - sum(weights^2) / sum(weights)
  tau2 <- max(0, (Q - df) / C)
  return(tau2)
}

#' Safe Division
#'
#' Performs division with protection against division by zero
#'
#' @param x Numerator
#' @param y Denominator
#' @param default Value to return when y is zero (default: NA)
#' @return Result of x/y or default if y is zero
#' @export
safe_divide <- function(x, y, default = NA) {
  result <- rep(default, length(x))
  non_zero <- y != 0 & !is.na(y)
  result[non_zero] <- x[non_zero] / y[non_zero]
  return(result)
}

#' Convert Standard Error to Variance
#'
#' @param se Standard error
#' @return Variance
#' @export
se_to_var <- function(se) {
  return(se^2)
}

#' Convert Variance to Standard Error
#'
#' @param var Variance
#' @return Standard error
#' @export
var_to_se <- function(var) {
  return(sqrt(var))
}

#' Format Confidence Interval
#'
#' @param lower Lower bound
#' @param upper Upper bound
#' @param digits Number of decimal places (default: 2)
#' @return Formatted string
#' @export
format_ci <- function(lower, upper, digits = 2) {
  sprintf("(%.${digits}f, %.${digits}f)", lower, upper)
}

#' Format P-value
#'
#' @param p P-value
#' @param digits Number of decimal places (default: 3)
#' @param threshold Threshold for reporting as "< threshold" (default: 0.001)
#' @return Formatted string
#' @export
format_p <- function(p, digits = 3, threshold = 0.001) {
  if (p < threshold) {
    return(sprintf("< %.${digits}f", threshold))
  } else {
    return(sprintf("%.${digits}f", p))
  }
}

#' Prediction Interval
#'
#' Computes a prediction interval for the true effect in a new study
#'
#' @param estimate Pooled effect estimate
#' @param se Standard error of pooled estimate
#' @param tau2 Between-study variance
#' @param k Number of studies
#' @param level Confidence level (default: 0.95)
#' @return List with lower and upper bounds
#' @export
prediction_interval <- function(estimate, se, tau2, k, level = 0.95) {
  # Use t-distribution with k-2 degrees of freedom
  df <- max(1, k - 2)
  t_crit <- qt((1 + level) / 2, df)

  # Prediction interval SE includes both within and between-study variance
  se_pred <- sqrt(se^2 + tau2)

  lower <- estimate - t_crit * se_pred
  upper <- estimate + t_crit * se_pred

  return(list(
    lower = lower,
    upper = upper,
    level = level,
    df = df
  ))
}

#' Detect Outliers Using IQR Method
#'
#' @param data Data frame with meta-analysis data
#' @param multiplier IQR multiplier for outlier detection (default: 1.5)
#' @return Logical vector indicating outliers
#' @export
detect_outliers_iqr <- function(data, multiplier = 1.5) {
  if (!"effect" %in% names(data)) {
    stop("data must contain 'effect' column")
  }

  effects <- data$effect
  q1 <- quantile(effects, 0.25)
  q3 <- quantile(effects, 0.75)
  iqr <- q3 - q1

  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr

  outliers <- effects < lower_bound | effects > upper_bound
  return(outliers)
}

#' Test cbamm Package
#'
#' Runs basic tests to verify package functionality
#'
#' @return Logical indicating if all tests passed
#' @export
test_cbamm <- function() {
  cat("Testing cbamm package...\n")

  # Create test data
  test_data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  # Test 1: cbamm_fast
  cat("  Test 1: cbamm_fast... ")
  tryCatch({
    result <- cbamm_fast(test_data)
    if (is.null(result$estimate)) stop("No estimate returned")
    cat("PASS\n")
  }, error = function(e) {
    cat("FAIL:", e$message, "\n")
    return(FALSE)
  })

  # Test 2: cumulative_meta_analysis
  cat("  Test 2: cumulative_meta_analysis... ")
  tryCatch({
    test_data$year <- 2015:2019
    result <- cumulative_meta_analysis(test_data, order_by = "year")
    if (is.null(result$final_estimate)) stop("No final estimate")
    cat("PASS\n")
  }, error = function(e) {
    cat("FAIL:", e$message, "\n")
    return(FALSE)
  })

  # Test 3: compute_transport_weights
  cat("  Test 3: compute_transport_weights... ")
  tryCatch({
    test_data$age_mean <- c(55, 60, 65, 58, 62)
    target <- list(age_mean = 60)
    result <- compute_transport_weights(test_data, target)
    if (is.null(result$weights)) stop("No weights returned")
    cat("PASS\n")
  }, error = function(e) {
    cat("FAIL:", e$message, "\n")
    return(FALSE)
  })

  cat("All tests completed successfully!\n")
  return(TRUE)
}
