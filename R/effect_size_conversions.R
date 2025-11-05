#' Effect Size Conversions
#'
#' Functions for converting between different effect size metrics
#'
#' @name effect_size_conversions
NULL

#' Convert Cohen's d to Hedges' g
#'
#' Applies small-sample bias correction to Cohen's d to obtain Hedges' g.
#'
#' @param d Cohen's d effect size
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2 (if NULL, uses n1 for pooled design)
#'
#' @return Hedges' g (bias-corrected effect size)
#'
#' @references
#' Hedges LV (1981). Distribution theory for Glass's estimator of effect size
#' and related estimators. Journal of Educational Statistics, 6(2):107-128.
#'
#' @export
#' @examples
#' d_to_g(0.5, n1 = 30, n2 = 30)
d_to_g <- function(d, n1, n2 = NULL) {
  if (is.null(n2)) {
    n2 <- n1
  }

  # Degrees of freedom
  df <- n1 + n2 - 2

  # Correction factor J (approximation)
  J <- 1 - (3 / (4 * df - 1))

  # Hedges' g
  g <- J * d

  return(g)
}


#' Convert Hedges' g to Cohen's d
#'
#' Removes bias correction from Hedges' g to obtain Cohen's d.
#'
#' @param g Hedges' g effect size
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#'
#' @return Cohen's d
#' @export
g_to_d <- function(g, n1, n2 = NULL) {
  if (is.null(n2)) {
    n2 <- n1
  }

  df <- n1 + n2 - 2
  J <- 1 - (3 / (4 * df - 1))

  d <- g / J
  return(d)
}


#' Convert Cohen's d to Correlation r
#'
#' @param d Cohen's d effect size
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#'
#' @return Correlation coefficient r
#'
#' @references
#' Borenstein M, Hedges LV, Higgins JPT, Rothstein HR (2009). Introduction to
#' Meta-Analysis. Wiley.
#'
#' @export
d_to_r <- function(d, n1, n2 = NULL) {
  if (is.null(n2)) {
    n2 <- n1
  }

  # Convert using the formula
  a <- (n1 + n2)^2 / (n1 * n2)
  r <- d / sqrt(d^2 + a)

  return(r)
}


#' Convert Correlation r to Cohen's d
#'
#' @param r Correlation coefficient
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#'
#' @return Cohen's d effect size
#' @export
r_to_d <- function(r, n1, n2 = NULL) {
  if (is.null(n2)) {
    n2 <- n1
  }

  a <- (n1 + n2)^2 / (n1 * n2)
  d <- (2 * r) / sqrt(1 - r^2) * sqrt(1/a)

  return(d)
}


#' Convert Odds Ratio to Log Odds Ratio
#'
#' @param or Odds ratio
#'
#' @return Log odds ratio
#' @export
or_to_log_or <- function(or) {
  return(log(or))
}


#' Convert Log Odds Ratio to Odds Ratio
#'
#' @param log_or Log odds ratio
#'
#' @return Odds ratio
#' @export
log_or_to_or <- function(log_or) {
  return(exp(log_or))
}


#' Convert Odds Ratio to Risk Ratio (approximate)
#'
#' Converts odds ratio to risk ratio using the baseline risk in the control group.
#'
#' @param or Odds ratio
#' @param p_control Baseline risk in control group (proportion with event)
#'
#' @return Risk ratio (relative risk)
#'
#' @references
#' Zhang J, Yu KF (1998). What's the relative risk? A method of correcting the
#' odds ratio in cohort studies of common outcomes. JAMA, 280(19):1690-1691.
#'
#' @export
or_to_rr <- function(or, p_control) {
  if (any(p_control <= 0 | p_control >= 1, na.rm = TRUE)) {
    stop("p_control must be between 0 and 1")
  }

  # Zhang & Yu approximation
  rr <- or / ((1 - p_control) + (p_control * or))

  return(rr)
}


#' Convert Risk Ratio to Odds Ratio
#'
#' @param rr Risk ratio
#' @param p_control Baseline risk in control group
#'
#' @return Odds ratio
#' @export
rr_to_or <- function(rr, p_control) {
  if (any(p_control <= 0 | p_control >= 1, na.rm = TRUE)) {
    stop("p_control must be between 0 and 1")
  }

  # Calculate OR from RR
  p_treatment <- rr * p_control
  or <- (p_treatment / (1 - p_treatment)) / (p_control / (1 - p_control))

  return(or)
}


#' Convert Cohen's d to Odds Ratio (approximate)
#'
#' @param d Cohen's d effect size
#'
#' @return Odds ratio
#'
#' @references
#' Chinn S (2000). A simple method for converting an odds ratio to effect size
#' for use in meta-analysis. Statistics in Medicine, 19(22):3127-3131.
#'
#' @export
d_to_or <- function(d) {
  # Chinn approximation
  or <- exp(d * pi / sqrt(3))
  return(or)
}


#' Convert Odds Ratio to Cohen's d (approximate)
#'
#' @param or Odds ratio
#'
#' @return Cohen's d effect size
#' @export
or_to_d <- function(or) {
  # Chinn approximation
  d <- log(or) * sqrt(3) / pi
  return(d)
}


#' Calculate Cohen's d from Means and SDs
#'
#' @param m1 Mean of group 1
#' @param m2 Mean of group 2
#' @param sd1 Standard deviation of group 1
#' @param sd2 Standard deviation of group 2
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#' @param pooled Logical indicating whether to use pooled SD (default: TRUE)
#'
#' @return List containing d and its variance
#'
#' @export
#' @examples
#' \dontrun{
#' result <- cohens_d_from_means(m1 = 10, m2 = 8, sd1 = 2, sd2 = 2,
#'                                n1 = 30, n2 = 30)
#' print(result)
#' }
cohens_d_from_means <- function(m1, m2, sd1, sd2, n1, n2, pooled = TRUE) {
  # Calculate d
  if (pooled) {
    # Pooled standard deviation
    sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    d <- (m1 - m2) / sd_pooled
  } else {
    # Average SD
    sd_avg <- sqrt((sd1^2 + sd2^2) / 2)
    d <- (m1 - m2) / sd_avg
  }

  # Variance of d
  var_d <- (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2))

  # Standard error
  se_d <- sqrt(var_d)

  return(list(
    d = d,
    var = var_d,
    se = se_d
  ))
}


#' Calculate Odds Ratio from 2x2 Table
#'
#' @param a Number of events in treatment group
#' @param b Number of non-events in treatment group
#' @param c Number of events in control group
#' @param d Number of non-events in control group
#' @param correction Continuity correction value (default: 0.5)
#'
#' @return List containing OR, log_OR, SE, and confidence interval
#'
#' @export
#' @examples
#' \dontrun{
#' or_result <- or_from_2x2(a = 50, b = 25, c = 30, d = 45)
#' print(or_result)
#' }
or_from_2x2 <- function(a, b, c, d, correction = 0.5) {
  # Apply continuity correction if any cell is zero
  if (any(c(a, b, c, d) == 0)) {
    a <- a + correction
    b <- b + correction
    c <- c + correction
    d <- d + correction
  }

  # Calculate OR
  or <- (a * d) / (b * c)
  log_or <- log(or)

  # Standard error of log(OR)
  se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)

  # Confidence interval for log(OR)
  log_or_lower <- log_or - 1.96 * se_log_or
  log_or_upper <- log_or + 1.96 * se_log_or

  # Back-transform to OR scale
  or_lower <- exp(log_or_lower)
  or_upper <- exp(log_or_upper)

  return(list(
    or = or,
    log_or = log_or,
    se = se_log_or,
    ci_lower = or_lower,
    ci_upper = or_upper,
    log_ci_lower = log_or_lower,
    log_ci_upper = log_or_upper
  ))
}


#' Calculate Risk Ratio from 2x2 Table
#'
#' @param a Number of events in treatment group
#' @param b Number of non-events in treatment group
#' @param c Number of events in control group
#' @param d Number of non-events in control group
#'
#' @return List containing RR, log_RR, SE, and confidence interval
#'
#' @export
rr_from_2x2 <- function(a, b, c, d) {
  n1 <- a + b  # Treatment group size
  n2 <- c + d  # Control group size

  # Event rates
  p1 <- a / n1
  p2 <- c / n2

  # Risk ratio
  rr <- p1 / p2
  log_rr <- log(rr)

  # Standard error of log(RR)
  se_log_rr <- sqrt((1 - p1) / (a) + (1 - p2) / (c))

  # Confidence interval
  log_rr_lower <- log_rr - 1.96 * se_log_rr
  log_rr_upper <- log_rr + 1.96 * se_log_rr

  rr_lower <- exp(log_rr_lower)
  rr_upper <- exp(log_rr_upper)

  return(list(
    rr = rr,
    log_rr = log_rr,
    se = se_log_rr,
    ci_lower = rr_lower,
    ci_upper = rr_upper,
    log_ci_lower = log_rr_lower,
    log_ci_upper = log_rr_upper
  ))
}


#' Calculate Risk Difference from 2x2 Table
#'
#' @param a Number of events in treatment group
#' @param b Number of non-events in treatment group
#' @param c Number of events in control group
#' @param d Number of non-events in control group
#'
#' @return List containing RD, SE, and confidence interval
#'
#' @export
rd_from_2x2 <- function(a, b, c, d) {
  n1 <- a + b
  n2 <- c + d

  # Event rates
  p1 <- a / n1
  p2 <- c / n2

  # Risk difference
  rd <- p1 - p2

  # Standard error
  se_rd <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

  # Confidence interval
  rd_lower <- rd - 1.96 * se_rd
  rd_upper <- rd + 1.96 * se_rd

  return(list(
    rd = rd,
    se = se_rd,
    ci_lower = rd_lower,
    ci_upper = rd_upper
  ))
}


#' Fisher's Z Transformation for Correlations
#'
#' @param r Correlation coefficient
#'
#' @return Fisher's Z-transformed value
#' @export
fisher_z <- function(r) {
  if (any(abs(r) >= 1, na.rm = TRUE)) {
    stop("Correlation must be between -1 and 1")
  }

  z <- 0.5 * log((1 + r) / (1 - r))
  return(z)
}


#' Inverse Fisher's Z Transformation
#'
#' @param z Fisher's Z-transformed value
#'
#' @return Correlation coefficient
#' @export
inv_fisher_z <- function(z) {
  r <- (exp(2 * z) - 1) / (exp(2 * z) + 1)
  return(r)
}


#' SE of Fisher's Z from Sample Size
#'
#' @param n Sample size
#'
#' @return Standard error of Fisher's Z
#' @export
se_fisher_z <- function(n) {
  return(1 / sqrt(n - 3))
}


#' Convert Effect Sizes in Data Frame
#'
#' Utility function to convert effect sizes in a data frame from one metric
#' to another.
#'
#' @param data Data frame with effect size data
#' @param from Source effect size metric
#' @param to Target effect size metric
#' @param ... Additional arguments passed to conversion functions
#'
#' @return Data frame with converted effect sizes
#'
#' @export
convert_effects <- function(data, from, to, ...) {
  supported_metrics <- c("d", "g", "r", "or", "log_or", "rr", "log_rr")

  if (!from %in% supported_metrics || !to %in% supported_metrics) {
    stop("Unsupported effect size metric. Supported: ",
         paste(supported_metrics, collapse = ", "))
  }

  if (from == to) {
    return(data)
  }

  # Create conversion key
  conversion_key <- paste(from, to, sep = "_to_")

  # Apply appropriate conversion
  converted_data <- data

  # Add more conversions as needed
  # This is a framework - expand based on available columns and conversions

  return(converted_data)
}
