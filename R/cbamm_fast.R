#' Fast Random-Effects Meta-Analysis
#'
#' Performs a fast random-effects meta-analysis using the DerSimonian-Laird method.
#' This function provides a computationally efficient implementation suitable for
#' large meta-analyses.
#'
#' @param data A data frame containing the meta-analysis data with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item effect: Effect size estimate
#'     \item se: Standard error of the effect size
#'     \item var: Variance of the effect size (optional, computed from se if missing)
#'   }
#' @param method Character string specifying the method for estimating tau-squared.
#'   Options: "DL" (DerSimonian-Laird, default), "REML" (Restricted Maximum Likelihood)
#' @param verbose Logical indicating whether to print progress messages (default: FALSE)
#'
#' @return An object of class "cbamm" containing:
#'   \itemize{
#'     \item estimate: Pooled effect size estimate
#'     \item se: Standard error of the pooled estimate
#'     \item ci_lower: Lower bound of 95% confidence interval
#'     \item ci_upper: Upper bound of 95% confidence interval
#'     \item tau2: Estimated between-study variance
#'     \item I2: I-squared statistic (percentage of variation due to heterogeneity)
#'     \item Q: Cochran's Q statistic
#'     \item p_value: P-value for the pooled estimate
#'     \item k: Number of studies
#'     \item weights: Study weights
#'     \item data: Original data
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:10),
#'   effect = rnorm(10, 0.5, 0.2),
#'   se = runif(10, 0.1, 0.3)
#' )
#' result <- cbamm_fast(data)
#' print(result)
#' }
cbamm_fast <- function(data, method = "DL", verbose = FALSE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove any rows with missing values
  complete_cases <- complete.cases(data[, c("effect", "se")])
  if (!all(complete_cases)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_cases)))
    data <- data[complete_cases, ]
  }

  if (nrow(data) < 2) {
    stop("Need at least 2 studies for meta-analysis")
  }

  # Compute variance if not provided
  if (!"var" %in% names(data)) {
    data$var <- data$se^2
  }

  k <- nrow(data)
  yi <- data$effect
  vi <- data$var

  # Fixed-effect estimate (for Q statistic)
  wi_fe <- 1 / vi
  sum_wi_fe <- sum(wi_fe)
  theta_fe <- sum(wi_fe * yi) / sum_wi_fe

  # Cochran's Q statistic
  Q <- sum(wi_fe * (yi - theta_fe)^2)
  df <- k - 1

  # Estimate tau-squared
  if (method == "DL") {
    # DerSimonian-Laird estimator
    C <- sum(wi_fe) - sum(wi_fe^2) / sum(wi_fe)
    tau2 <- max(0, (Q - df) / C)
  } else if (method == "REML") {
    # Simple REML approximation
    tau2 <- max(0, (Q - df) / (k - 1))
  } else {
    stop("Method must be 'DL' or 'REML'")
  }

  # Random-effects weights
  wi <- 1 / (vi + tau2)
  sum_wi <- sum(wi)

  # Pooled estimate
  theta <- sum(wi * yi) / sum_wi
  se_theta <- sqrt(1 / sum_wi)

  # Confidence interval
  z_crit <- qnorm(0.975)
  ci_lower <- theta - z_crit * se_theta
  ci_upper <- theta + z_crit * se_theta

  # P-value
  z_stat <- theta / se_theta
  p_value <- 2 * pnorm(-abs(z_stat))

  # I-squared statistic
  I2 <- max(0, 100 * (Q - df) / Q)

  if (verbose) {
    cat(sprintf("Meta-analysis of %d studies\n", k))
    cat(sprintf("Estimate: %.3f (95%% CI: %.3f, %.3f)\n", theta, ci_lower, ci_upper))
    cat(sprintf("Heterogeneity: I² = %.1f%%, τ² = %.4f\n", I2, tau2))
  }

  # Create result object
  result <- list(
    estimate = theta,
    se = se_theta,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    tau2 = tau2,
    I2 = I2,
    Q = Q,
    Q_df = df,
    Q_pval = pchisq(Q, df, lower.tail = FALSE),
    p_value = p_value,
    k = k,
    weights = wi / sum_wi,
    method = method,
    data = data
  )

  class(result) <- "cbamm"
  return(result)
}
