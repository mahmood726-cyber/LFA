#' Sensitivity and Influence Analysis
#'
#' Advanced methods for assessing study influence and conducting sensitivity analyses
#'
#' @name sensitivity_analysis
NULL

#' Leave-One-Out Sensitivity Analysis
#'
#' Conducts leave-one-out meta-analysis to assess the influence of each study
#' on the pooled estimate. Essential for sensitivity analysis.
#'
#' @param data Data frame with meta-analysis data
#' @param method Method for meta-analysis (default: "DL")
#' @param sort_by Variable to sort results by: "none" (default), "estimate",
#'   "I2", or "influence"
#'
#' @return Object of class "loo_analysis" containing:
#'   \itemize{
#'     \item results: Data frame with leave-one-out results for each study
#'     \item full_model: Results from full meta-analysis
#'     \item max_influence: Study with maximum influence
#'     \item influential_studies: Studies exceeding influence threshold
#'   }
#'
#' @references
#' Viechtbauer W, Cheung MWL (2010). Outlier and influence diagnostics for
#' meta-analysis. Research Synthesis Methods, 1(2):112-125.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' loo_result <- leave_one_out(example_meta)
#' print(loo_result)
#' plot(loo_result)
#' }
leave_one_out <- function(data, method = "DL", sort_by = "none") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for leave-one-out analysis")
  }

  # Full model
  full_model <- cbamm_fast(data, method = method, verbose = FALSE)

  # Initialize results storage
  loo_results <- data.frame(
    excluded_study = character(k),
    estimate = numeric(k),
    se = numeric(k),
    ci_lower = numeric(k),
    ci_upper = numeric(k),
    tau2 = numeric(k),
    I2 = numeric(k),
    Q = numeric(k),
    p_value = numeric(k),
    influence = numeric(k),
    stringsAsFactors = FALSE
  )

  # Get study names
  study_names <- if ("study" %in% names(data)) {
    as.character(data$study)
  } else {
    paste0("Study", 1:k)
  }

  # Leave-one-out analysis
  for (i in 1:k) {
    # Exclude study i
    data_loo <- data[-i, ]

    # Run meta-analysis
    ma_loo <- cbamm_fast(data_loo, method = method, verbose = FALSE)

    # Calculate influence (standardized difference in estimate)
    influence <- abs(full_model$estimate - ma_loo$estimate) / ma_loo$se

    # Store results
    loo_results[i, "excluded_study"] <- study_names[i]
    loo_results[i, "estimate"] <- ma_loo$estimate
    loo_results[i, "se"] <- ma_loo$se
    loo_results[i, "ci_lower"] <- ma_loo$ci_lower
    loo_results[i, "ci_upper"] <- ma_loo$ci_upper
    loo_results[i, "tau2"] <- ma_loo$tau2
    loo_results[i, "I2"] <- ma_loo$I2
    loo_results[i, "Q"] <- ma_loo$Q
    loo_results[i, "p_value"] <- ma_loo$p_value
    loo_results[i, "influence"] <- influence
  }

  # Sort if requested
  if (sort_by != "none") {
    if (sort_by == "influence") {
      loo_results <- loo_results[order(-loo_results$influence), ]
    } else if (sort_by %in% names(loo_results)) {
      loo_results <- loo_results[order(-abs(loo_results[[sort_by]] - full_model$estimate)), ]
    }
  }

  # Identify influential studies (influence > 1.96)
  influential_threshold <- 1.96
  influential_studies <- loo_results[loo_results$influence > influential_threshold, ]

  # Study with maximum influence
  max_influence_idx <- which.max(loo_results$influence)

  result <- list(
    results = loo_results,
    full_model = full_model,
    max_influence = loo_results[max_influence_idx, ],
    influential_studies = influential_studies,
    k = k,
    method = method
  )

  class(result) <- "loo_analysis"
  return(result)
}


#' Influence Diagnostics
#'
#' Computes comprehensive influence diagnostics including Cook's distance,
#' hat values, and DFBETAS for meta-analysis.
#'
#' @param data Data frame with meta-analysis data
#' @param method Method for meta-analysis (default: "DL")
#'
#' @return Object of class "influence_diagnostics" containing:
#'   \itemize{
#'     \item diagnostics: Data frame with diagnostic statistics for each study
#'     \item outliers: Studies identified as outliers
#'     \item influential: Studies identified as influential
#'     \item thresholds: Threshold values used for identification
#'   }
#'
#' @references
#' Viechtbauer W, Cheung MWL (2010). Outlier and influence diagnostics for
#' meta-analysis. Research Synthesis Methods, 1(2):112-125.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' influence_result <- influence_diagnostics(example_meta)
#' print(influence_result)
#' }
influence_diagnostics <- function(data, method = "DL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for influence diagnostics")
  }

  # Run full meta-analysis
  ma_full <- cbamm_fast(data, method = method, verbose = FALSE)

  # Get study names
  study_names <- if ("study" %in% names(data)) {
    as.character(data$study)
  } else {
    paste0("Study", 1:k)
  }

  yi <- data$effect
  sei <- data$se
  wi <- ma_full$weights  # Study weights from full model

  # Calculate diagnostics
  diagnostics <- data.frame(
    study = study_names,
    effect = yi,
    se = sei,
    weight = wi,
    stringsAsFactors = FALSE
  )

  # Standardized residuals
  resid <- yi - ma_full$estimate
  std_resid <- resid / sqrt(sei^2 + ma_full$tau2)
  diagnostics$std_residual <- std_resid

  # Hat values (leverage)
  # For meta-analysis: h_i = w_i / sum(w_i)
  diagnostics$hat <- wi

  # Cook's distance approximation
  # D_i = (std_resid^2 * hat) / (1 - hat)
  diagnostics$cooks_d <- (std_resid^2 * wi) / (1 - wi + 0.001)  # Add small constant

  # DFBETAS (change in estimate when study removed)
  dfbetas <- numeric(k)
  for (i in 1:k) {
    ma_loo <- cbamm_fast(data[-i, ], method = method, verbose = FALSE)
    dfbetas[i] <- (ma_full$estimate - ma_loo$estimate) / ma_loo$se
  }
  diagnostics$dfbetas <- dfbetas

  # Identify outliers and influential studies
  # Outliers: |standardized residual| > 1.96
  outlier_threshold <- 1.96
  diagnostics$is_outlier <- abs(diagnostics$std_residual) > outlier_threshold

  # Influential: Cook's D > 4/k or |DFBETAS| > 2/sqrt(k)
  cooks_threshold <- 4 / k
  dfbetas_threshold <- 2 / sqrt(k)
  diagnostics$is_influential <- (diagnostics$cooks_d > cooks_threshold) |
                                 (abs(diagnostics$dfbetas) > dfbetas_threshold)

  # Extract outliers and influential studies
  outliers <- diagnostics[diagnostics$is_outlier, ]
  influential <- diagnostics[diagnostics$is_influential, ]

  result <- list(
    diagnostics = diagnostics,
    outliers = outliers,
    influential = influential,
    thresholds = list(
      outlier_resid = outlier_threshold,
      cooks_d = cooks_threshold,
      dfbetas = dfbetas_threshold
    ),
    full_model = ma_full,
    k = k
  )

  class(result) <- "influence_diagnostics"
  return(result)
}


#' Baujat Plot Data
#'
#' Computes data for Baujat plot, which displays each study's contribution
#' to overall heterogeneity (x-axis) against its influence on pooled estimate
#' (y-axis). Studies in the upper-right quadrant warrant investigation.
#'
#' @param data Data frame with meta-analysis data
#' @param method Method for meta-analysis (default: "DL")
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item heterogeneity_contribution: Contribution to Q statistic
#'     \item influence: Influence on pooled estimate
#'   }
#'
#' @references
#' Baujat B, Mahé C, Pignon JP, Hill C (2002). A graphical method for exploring
#' heterogeneity in meta-analyses: application to a meta-analysis of 65 trials.
#' Statistics in Medicine, 21(18):2641-2652.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' baujat_data <- baujat_plot_data(example_meta)
#' print(baujat_data)
#' }
baujat_plot_data <- function(data, method = "DL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for Baujat plot")
  }

  # Full meta-analysis
  ma_full <- cbamm_fast(data, method = method, verbose = FALSE)

  # Get study names
  study_names <- if ("study" %in% names(data)) {
    as.character(data$study)
  } else {
    paste0("Study", 1:k)
  }

  yi <- data$effect
  sei <- data$se
  wi <- ma_full$weights

  # Calculate heterogeneity contribution for each study
  # Q_i = w_i * (y_i - theta)^2
  resid <- yi - ma_full$estimate
  Q_contrib <- wi * (resid^2) * sum(wi)

  # Calculate influence (squared difference in estimates)
  influence <- numeric(k)
  for (i in 1:k) {
    ma_loo <- cbamm_fast(data[-i, ], method = method, verbose = FALSE)
    # Standardized squared difference
    influence[i] <- ((ma_full$estimate - ma_loo$estimate) / ma_loo$se)^2
  }

  result <- data.frame(
    study = study_names,
    heterogeneity_contribution = Q_contrib,
    influence = influence,
    stringsAsFactors = FALSE
  )

  class(result) <- "baujat_data"
  return(result)
}


#' Bootstrap Meta-Analysis
#'
#' Performs bootstrap resampling to estimate confidence intervals and
#' assess stability of meta-analytic estimates.
#'
#' @param data Data frame with meta-analysis data
#' @param n_bootstrap Number of bootstrap samples (default: 1000)
#' @param method Method for meta-analysis (default: "DL")
#' @param ci_level Confidence level (default: 0.95)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return List containing:
#'   \itemize{
#'     \item original_estimate: Estimate from original data
#'     \item bootstrap_estimates: Vector of bootstrap estimates
#'     \item bootstrap_ci: Bootstrap confidence interval
#'     \item bootstrap_se: Bootstrap standard error
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' boot_result <- bootstrap_ma(example_meta, n_bootstrap = 500)
#' hist(boot_result$bootstrap_estimates)
#' }
bootstrap_ma <- function(data, n_bootstrap = 1000, method = "DL",
                         ci_level = 0.95, seed = NULL) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for bootstrap")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Original estimate
  ma_original <- cbamm_fast(data, method = method, verbose = FALSE)

  # Bootstrap sampling
  boot_estimates <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    # Resample studies with replacement
    boot_indices <- sample(1:k, k, replace = TRUE)
    boot_data <- data[boot_indices, ]

    # Run meta-analysis on bootstrap sample
    tryCatch({
      ma_boot <- cbamm_fast(boot_data, method = method, verbose = FALSE)
      boot_estimates[i] <- ma_boot$estimate
    }, error = function(e) {
      boot_estimates[i] <<- NA
    })
  }

  # Remove any NAs
  boot_estimates <- boot_estimates[!is.na(boot_estimates)]

  # Bootstrap confidence interval (percentile method)
  alpha <- 1 - ci_level
  boot_ci <- quantile(boot_estimates, probs = c(alpha/2, 1 - alpha/2))

  # Bootstrap standard error
  boot_se <- sd(boot_estimates)

  result <- list(
    original_estimate = ma_original$estimate,
    original_se = ma_original$se,
    bootstrap_estimates = boot_estimates,
    bootstrap_ci = boot_ci,
    bootstrap_se = boot_se,
    n_bootstrap = length(boot_estimates),
    ci_level = ci_level
  )

  class(result) <- "bootstrap_ma"
  return(result)
}


#' Cumulative Influence Analysis
#'
#' Combines cumulative meta-analysis with influence diagnostics to identify
#' when influential studies entered the literature.
#'
#' @param data Data frame with meta-analysis data
#' @param order_by Variable to order studies by (e.g., "year")
#'
#' @return Object combining cumulative and influence results
#'
#' @export
cumulative_influence <- function(data, order_by = "year") {
  # Run cumulative meta-analysis
  cum_ma <- cumulative_meta_analysis(data, order_by = order_by)

  # Run influence diagnostics
  influence <- influence_diagnostics(data)

  # Combine results
  result <- list(
    cumulative = cum_ma,
    influence = influence,
    note = "Identifies when influential studies entered the evidence base"
  )

  class(result) <- "cumulative_influence"
  return(result)
}
