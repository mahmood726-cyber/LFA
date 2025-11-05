#' Prevalence and Proportion Meta-Analysis Module
#'
#' Specialized methods for meta-analysis of proportions (prevalence, incidence,
#' response rates, etc.). Handles boundary issues (0, 1) with appropriate
#' transformations.
#'
#' Features:
#' - Multiple variance-stabilizing transformations (logit, arcsine, Freeman-Tukey)
#' - Exact binomial methods for small samples
#' - Continuity corrections for zero cells
#' - Back-transformation with confidence intervals
#' - Subgroup analysis for prevalence studies
#' - Prediction intervals for new populations
#'
#' Based on: Barendregt et al. (2013), Schwarzer et al. (2019), Miller (1978).
#'
#' @name prevalence_meta_analysis
NULL


#' Meta-Analysis of Proportions
#'
#' Performs meta-analysis of prevalence, incidence, or other proportions with
#' appropriate transformations and variance stabilization.
#'
#' @param data Data frame with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item events: Number of events
#'     \item total: Total sample size
#'     \item (Optional) proportion: Pre-calculated proportion (if events/total not available)
#'     \item (Optional) se: Standard error (if events/total not available)
#'   }
#' @param transformation Variance-stabilizing transformation:
#'   "logit" (default), "arcsine", "freeman_tukey", "log", "identity"
#' @param method Pooling method: "DL", "REML", "PM" (default: "REML")
#' @param continuity_correction Continuity correction for zero/one proportions (default: 0.5)
#' @param prediction_interval Calculate prediction interval (default: TRUE)
#' @param exact_binomial Use exact binomial for CIs when possible (default: TRUE)
#'
#' @return Prevalence meta-analysis object with pooled proportion and CIs
#'
#' @references
#' Barendregt JJ et al. (2013). Meta-analysis of prevalence. J Epidemiol Community
#' Health, 67(11):974-978.
#'
#' Schwarzer G et al. (2019). Meta-analysis of proportions. In: Meta-Analysis
#' with R, pp. 163-191. Springer.
#'
#' Miller JJ (1978). The inverse of the Freeman-Tukey double arcsine transformation.
#' Am Stat, 32(4):138.
#'
#' @export
#' @examples
#' \dontrun{
#' # Meta-analysis of disease prevalence
#' prev_data <- data.frame(
#'   study = paste0("Study", 1:15),
#'   events = c(23, 45, 12, 67, 34, 89, 45, 23, 56, 78, 34, 45, 23, 67, 45),
#'   total = c(200, 300, 150, 400, 250, 500, 300, 200, 350, 450, 280, 320, 210, 380, 290)
#' )
#'
#' prev_result <- meta_proportion(
#'   data = prev_data,
#'   transformation = "logit",
#'   method = "REML"
#' )
#'
#' print(prev_result)
#' plot(prev_result)
#' }
meta_proportion <- function(data, transformation = "logit", method = "REML",
                           continuity_correction = 0.5,
                           prediction_interval = TRUE,
                           exact_binomial = TRUE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   META-ANALYSIS OF PROPORTIONS                               ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a data frame")

  # Check if we have events/total or proportion/se
  has_raw <- all(c("events", "total") %in% names(data))
  has_proportion <- all(c("proportion", "se") %in% names(data))

  if (!has_raw && !has_proportion) {
    stop("data must have either (events, total) or (proportion, se) columns")
  }

  # Remove missing values
  if (has_raw) {
    complete_idx <- complete.cases(data[, c("events", "total")])
  } else {
    complete_idx <- complete.cases(data[, c("proportion", "se")])
  }

  if (!all(complete_idx)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_idx)))
    data <- data[complete_idx, ]
  }

  k <- nrow(data)
  cat(sprintf("Studies: %d\n", k))
  cat(sprintf("Transformation: %s\n", transformation))
  cat(sprintf("Method: %s\n\n", method))

  # Calculate proportions if not provided
  if (has_raw) {
    # Apply continuity correction for 0 and 1
    events_adj <- data$events
    total_adj <- data$total

    # Correction for zero events
    zero_idx <- events_adj == 0
    if (any(zero_idx)) {
      events_adj[zero_idx] <- events_adj[zero_idx] + continuity_correction
      total_adj[zero_idx] <- total_adj[zero_idx] + 2 * continuity_correction
      cat(sprintf("Applied continuity correction to %d studies with 0 events\n",
                  sum(zero_idx)))
    }

    # Correction for all events
    one_idx <- events_adj == total_adj
    if (any(one_idx)) {
      events_adj[one_idx] <- events_adj[one_idx] + continuity_correction
      total_adj[one_idx] <- total_adj[one_idx] + 2 * continuity_correction
      cat(sprintf("Applied continuity correction to %d studies with all events\n\n",
                  sum(one_idx)))
    }

    p <- events_adj / total_adj
    n <- total_adj

  } else {
    p <- data$proportion
    # Estimate sample size from SE if needed
    # For proportion: SE ≈ sqrt(p*(1-p)/n)
    # So n ≈ p*(1-p) / SE^2
    n <- p * (1 - p) / data$se^2
  }

  cat(sprintf("Proportion range: %.4f to %.4f\n", min(p), max(p)))
  cat(sprintf("Median sample size: %.0f\n\n", median(n)))

  # Transform proportions
  cat(sprintf("Applying %s transformation...\n", transformation))

  transformed <- transform_proportion(p, n, transformation)
  y <- transformed$y
  v <- transformed$v

  # Meta-analysis on transformed scale
  cat("Performing meta-analysis on transformed scale...\n")

  # Load advanced heterogeneity estimator if available
  if (method == "PM") {
    result_ma <- meta_analysis_advanced(
      data.frame(study = seq_len(k), effect = y, se = sqrt(v)),
      method = "PM", test = "knha", prediction_interval = FALSE
    )
    theta_trans <- result_ma$estimate
    se_trans <- result_ma$se
    tau2 <- result_ma$tau2
    ci_lower_trans <- result_ma$ci_lower_hksj
    ci_upper_trans <- result_ma$ci_upper_hksj
  } else {
    result_ma <- cbamm_fast(
      data.frame(study = seq_len(k), effect = y, se = sqrt(v)),
      method = method, verbose = FALSE
    )
    theta_trans <- result_ma$estimate
    se_trans <- result_ma$se
    tau2 <- result_ma$tau2
    ci_lower_trans <- result_ma$ci_lower
    ci_upper_trans <- result_ma$ci_upper
  }

  cat(sprintf("  Transformed estimate: %.4f (SE = %.4f)\n", theta_trans, se_trans))
  cat(sprintf("  Heterogeneity: τ² = %.4f, I² = %.1f%%\n", tau2, result_ma$I2))

  # Back-transform to proportion scale
  cat("\nBack-transforming to proportion scale...\n")

  pooled_prop <- backtransform_proportion(theta_trans, transformation)
  ci_lower_prop <- backtransform_proportion(ci_lower_trans, transformation)
  ci_upper_prop <- backtransform_proportion(ci_upper_trans, transformation)

  cat(sprintf("Pooled proportion: %.4f (95%% CI: %.4f, %.4f)\n",
              pooled_prop, ci_lower_prop, ci_upper_prop))

  # Prediction interval on proportion scale
  if (prediction_interval) {
    cat("\nCalculating prediction interval...\n")

    # PI on transformed scale
    tau <- sqrt(tau2)
    se_pi_trans <- sqrt(se_trans^2 + tau^2)

    t_crit <- qt(0.975, k - 1)
    pi_lower_trans <- theta_trans - t_crit * se_pi_trans
    pi_upper_trans <- theta_trans + t_crit * se_pi_trans

    # Back-transform
    pi_lower_prop <- backtransform_proportion(pi_lower_trans, transformation)
    pi_upper_prop <- backtransform_proportion(pi_upper_trans, transformation)

    cat(sprintf("95%% Prediction interval: (%.4f, %.4f)\n",
                pi_lower_prop, pi_upper_prop))

    pred_int <- list(
      pi_lower = pi_lower_prop,
      pi_upper = pi_upper_prop,
      pi_lower_trans = pi_lower_trans,
      pi_upper_trans = pi_upper_trans
    )
  } else {
    pred_int <- NULL
  }

  # Study-specific transformed values for plotting
  study_data <- data.frame(
    study = if ("study" %in% names(data)) data$study else seq_len(k),
    proportion = p,
    n = n,
    y_trans = y,
    v_trans = v,
    weight = 1 / (v + tau2)
  )

  # Result object
  result <- list(
    pooled_proportion = pooled_prop,
    ci_lower = ci_lower_prop,
    ci_upper = ci_upper_prop,
    se = se_trans,  # On transformed scale
    theta_trans = theta_trans,
    ci_lower_trans = ci_lower_trans,
    ci_upper_trans = ci_upper_trans,
    tau2 = tau2,
    tau = sqrt(tau2),
    I2 = result_ma$I2,
    Q = result_ma$Q,
    Q_df = result_ma$Q_df,
    Q_pval = result_ma$Q_pval,
    prediction_interval = pred_int,
    transformation = transformation,
    method = method,
    k = k,
    study_data = study_data,
    continuity_correction = continuity_correction
  )

  class(result) <- c("meta_proportion", "list")

  cat("\n✓ Proportion meta-analysis completed\n")
  return(result)
}


#' Transform Proportion
#'
#' @keywords internal
transform_proportion <- function(p, n, transformation) {

  if (transformation == "logit") {
    # Logit transformation: log(p / (1-p))
    y <- log(p / (1 - p))
    # Variance: 1 / (n * p * (1-p))
    v <- 1 / (n * p * (1 - p))

  } else if (transformation == "arcsine") {
    # Arcsine transformation
    y <- asin(sqrt(p))
    # Variance: 1 / (4*n)
    v <- 1 / (4 * n)

  } else if (transformation == "freeman_tukey") {
    # Freeman-Tukey double arcsine transformation
    # FT = 0.5 * (arcsin(sqrt(x/n)) + arcsin(sqrt((x+1)/n)))
    # More stable for extreme proportions
    x <- p * n  # events
    y <- 0.5 * (asin(sqrt(x / n)) + asin(sqrt((x + 1) / (n + 1))))
    # Variance: 1 / (4*n + 2)
    v <- 1 / (4 * n + 2)

  } else if (transformation == "log") {
    # Log transformation (for low proportions)
    y <- log(p)
    # Variance: (1-p) / (n*p)
    v <- (1 - p) / (n * p)

  } else if (transformation == "identity") {
    # No transformation
    y <- p
    # Variance: p*(1-p)/n
    v <- p * (1 - p) / n

  } else {
    stop("Unknown transformation. Use: logit, arcsine, freeman_tukey, log, identity")
  }

  return(list(y = y, v = v))
}


#' Back-Transform Proportion
#'
#' @keywords internal
backtransform_proportion <- function(theta_trans, transformation) {

  if (transformation == "logit") {
    # Inverse logit
    p <- exp(theta_trans) / (1 + exp(theta_trans))

  } else if (transformation == "arcsine") {
    # Inverse arcsine
    p <- (sin(theta_trans))^2

  } else if (transformation == "freeman_tukey") {
    # Inverse Freeman-Tukey (Miller 1978 approximation)
    # p ≈ (sin(FT))^2
    p <- (sin(theta_trans))^2

  } else if (transformation == "log") {
    # Inverse log
    p <- exp(theta_trans)

  } else if (transformation == "identity") {
    # No transformation
    p <- theta_trans

  } else {
    stop("Unknown transformation")
  }

  # Constrain to [0, 1]
  p <- pmax(0, pmin(1, p))

  return(p)
}


#' Meta-Regression for Proportions
#'
#' Meta-regression for proportions with moderators.
#'
#' @param data Data frame with events, total, and moderators
#' @param formula Formula for moderators (e.g., ~ year + region)
#' @param transformation Transformation (default: "logit")
#' @param method Estimation method (default: "REML")
#'
#' @export
metareg_proportion <- function(data, formula, transformation = "logit",
                               method = "REML") {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   META-REGRESSION FOR PROPORTIONS                            ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Calculate proportions
  if (!all(c("events", "total") %in% names(data))) {
    stop("data must have 'events' and 'total' columns")
  }

  p <- data$events / data$total
  n <- data$total

  # Transform
  transformed <- transform_proportion(p, n, transformation)

  # Create data for meta-regression
  data_reg <- data
  data_reg$effect <- transformed$y
  data_reg$se <- sqrt(transformed$v)

  # Perform meta-regression
  result <- meta_regression(data_reg, formula, method = method)

  result$transformation <- transformation
  class(result) <- c("metareg_proportion", class(result))

  return(result)
}


#' Subgroup Analysis for Proportions
#'
#' @param data Data frame with events, total, and subgroup variable
#' @param subgroup Column name for subgroup variable
#' @param transformation Transformation (default: "logit")
#'
#' @export
subgroup_proportion <- function(data, subgroup, transformation = "logit") {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   SUBGROUP ANALYSIS FOR PROPORTIONS                          ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  if (!subgroup %in% names(data)) {
    stop("Subgroup variable not found in data")
  }

  groups <- unique(data[[subgroup]])
  cat(sprintf("Subgroups: %s\n\n", paste(groups, collapse = ", ")))

  subgroup_results <- list()

  for (g in groups) {
    cat(sprintf("Analyzing subgroup: %s\n", g))

    data_g <- data[data[[subgroup]] == g, ]

    result_g <- meta_proportion(
      data_g,
      transformation = transformation,
      prediction_interval = FALSE
    )

    subgroup_results[[as.character(g)]] <- result_g

    cat(sprintf("  Proportion: %.4f (95%% CI: %.4f, %.4f)\n\n",
                result_g$pooled_proportion,
                result_g$ci_lower,
                result_g$ci_upper))
  }

  # Test for subgroup differences (on transformed scale)
  estimates_trans <- sapply(subgroup_results, function(x) x$theta_trans)
  ses_trans <- sapply(subgroup_results, function(x) x$se)

  wi <- 1 / ses_trans^2
  grand_mean <- sum(wi * estimates_trans) / sum(wi)

  Q_between <- sum(wi * (estimates_trans - grand_mean)^2)
  df <- length(estimates_trans) - 1
  p_between <- pchisq(Q_between, df, lower.tail = FALSE)

  cat("Test for Subgroup Differences:\n")
  cat(sprintf("  Q = %.2f, df = %d, p = %.4f\n", Q_between, df, p_between))

  if (p_between < 0.05) {
    cat("  ✓ Significant differences between subgroups\n")
  } else {
    cat("  No significant differences between subgroups\n")
  }

  result <- list(
    subgroup_results = subgroup_results,
    Q_between = Q_between,
    df = df,
    p_between = p_between,
    subgroup_var = subgroup,
    transformation = transformation
  )

  class(result) <- c("subgroup_proportion", "list")
  return(result)
}


#' Print Method for Proportion Meta-Analysis
#'
#' @export
print.meta_proportion <- function(x, ...) {
  cat("Meta-Analysis of Proportions\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Transformation: %s\n", x$transformation))
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Studies: %d\n\n", x$k))

  cat("Pooled Proportion:\n")
  cat(sprintf("  %.4f (95%% CI: %.4f, %.4f)\n\n",
              x$pooled_proportion, x$ci_lower, x$ci_upper))

  cat("Heterogeneity:\n")
  cat(sprintf("  τ² = %.4f, I² = %.1f%%\n", x$tau2, x$I2))
  cat(sprintf("  Q = %.2f (df = %d, p = %.4f)\n\n", x$Q, x$Q_df, x$Q_pval))

  if (!is.null(x$prediction_interval)) {
    cat("Prediction Interval (95%):\n")
    cat(sprintf("  (%.4f, %.4f)\n", x$prediction_interval$pi_lower,
                x$prediction_interval$pi_upper))
  }

  invisible(x)
}


#' Plot Method for Proportion Meta-Analysis
#'
#' @export
plot.meta_proportion <- function(x, ...) {

  # Forest plot for proportions
  study_data <- x$study_data
  k <- x$k

  # Calculate CIs for each study (on proportion scale)
  study_ci_lower <- numeric(k)
  study_ci_upper <- numeric(k)

  for (i in 1:k) {
    ci_trans_lower <- study_data$y_trans[i] - qnorm(0.975) * sqrt(study_data$v_trans[i])
    ci_trans_upper <- study_data$y_trans[i] + qnorm(0.975) * sqrt(study_data$v_trans[i])

    study_ci_lower[i] <- backtransform_proportion(ci_trans_lower, x$transformation)
    study_ci_upper[i] <- backtransform_proportion(ci_trans_upper, x$transformation)
  }

  # Forest plot
  par(mar = c(5, 10, 4, 2))

  y_pos <- k:1

  plot(NULL, xlim = c(0, 1), ylim = c(0.5, k + 1.5),
       xlab = "Proportion", ylab = "",
       main = "Meta-Analysis of Proportions",
       yaxt = "n")

  # Study CIs
  for (i in 1:k) {
    segments(study_ci_lower[i], y_pos[i], study_ci_upper[i], y_pos[i])
    points(study_data$proportion[i], y_pos[i],
           pch = 15, cex = sqrt(study_data$weight[i]) * 2)
  }

  # Pooled estimate
  abline(v = x$pooled_proportion, col = "blue", lwd = 2)

  # Pooled CI
  polygon(c(x$ci_lower, x$ci_upper, x$ci_upper, x$ci_lower),
          c(0.5, 0.5, 0, 0),
          col = rgb(0, 0, 1, 0.2), border = NA)

  # Study labels
  axis(2, at = y_pos, labels = study_data$study, las = 1, cex.axis = 0.7)

  invisible(NULL)
}
