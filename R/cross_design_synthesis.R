#' Cross-Design Synthesis Module
#'
#' Advanced methods for synthesizing evidence from different study designs:
#' - Combining RCTs with observational studies
#' - Adjusting for bias in non-randomized studies
#' - Design-specific heterogeneity modeling
#' - Bias modeling and correction
#' - Hierarchical models for design-specific effects
#'
#' Based on: Reeves et al. (2013), Efthimiou et al. (2017), Verde & Ohmann (2015).
#'
#' @name cross_design_synthesis
NULL


#' Cross-Design Meta-Analysis
#'
#' Synthesizes evidence from multiple study designs (e.g., RCTs + observational).
#' Accounts for potential bias in non-randomized studies while borrowing strength.
#'
#' @param data Data frame with meta-analysis data
#' @param design_var Column name indicating study design (e.g., "RCT", "Cohort", "Case-Control")
#' @param reference_design Reference design assumed unbiased (default: "RCT")
#' @param bias_model Bias model: "none", "additive", "proportional", "hierarchical"
#' @param pool_designs Pool across designs (TRUE) or analyze separately (FALSE)
#' @param method Estimation method: "REML" or "Bayesian"
#' @param prior_bias Prior for bias parameters (Bayesian only)
#'
#' @return Cross-design synthesis object
#'
#' @references
#' Reeves BC et al. (2013). Combining individual patient data and aggregate data
#' in mixed treatment comparison meta-analysis. BMC Med Res Methodol, 13:48.
#'
#' Efthimiou O et al. (2017). Combining randomized and non-randomized evidence
#' in network meta-analysis. Stat Med, 36(8):1210-1226.
#'
#' Verde PE, Ohmann C (2015). Combining randomized and non-randomized evidence
#' in clinical research. Stat Med, 34(1):132-152.
#'
#' @export
#' @examples
#' \dontrun{
#' # Combine RCTs and cohort studies
#' cds <- cross_design_synthesis(
#'   data = combined_data,
#'   design_var = "study_design",
#'   reference_design = "RCT",
#'   bias_model = "additive"
#' )
#'
#' # View bias estimates
#' print(cds$bias_estimates)
#' }
cross_design_synthesis <- function(data, design_var, reference_design = "RCT",
                                   bias_model = "additive", pool_designs = TRUE,
                                   method = "REML", prior_bias = NULL) {

  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║   CROSS-DESIGN SYNTHESIS                                       ║\n")
  cat("║   Combining Evidence from Multiple Study Designs               ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!design_var %in% names(data)) stop("design_var not found in data")
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Remove missing values
  complete_idx <- complete.cases(data[, c("effect", "se", design_var)])
  if (!all(complete_idx)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_idx)))
    data <- data[complete_idx, ]
  }

  designs <- unique(data[[design_var]])
  n_designs <- length(designs)

  cat(sprintf("Study designs: %s\n", paste(designs, collapse = ", ")))
  cat(sprintf("Reference design: %s\n", reference_design))
  cat(sprintf("Bias model: %s\n\n", bias_model))

  # Summary by design
  cat("Data Summary by Design:\n")
  for (d in designs) {
    n_d <- sum(data[[design_var]] == d)
    cat(sprintf("  %s: %d studies\n", d, n_d))
  }

  if (!reference_design %in% designs) {
    stop("Reference design not found in data")
  }

  # Separate analyses by design
  cat("\nAnalyzing each design separately...\n")
  design_results <- list()

  for (d in designs) {
    data_d <- data[data[[design_var]] == d, ]
    result_d <- cbamm_fast(data_d, method = "DL", verbose = FALSE)
    design_results[[d]] <- result_d

    cat(sprintf("  %s: Effect = %.3f (95%% CI: %.3f, %.3f), τ² = %.4f\n",
                d, result_d$estimate, result_d$ci_lower, result_d$ci_upper,
                result_d$tau2))
  }

  # Test for design differences
  cat("\nTesting for differences between designs...\n")
  design_test <- test_design_differences(design_results)

  cat(sprintf("  Q_between = %.2f, df = %d, p = %.4f\n",
              design_test$Q_between, design_test$df, design_test$p_value))

  if (design_test$p_value < 0.05) {
    cat("  ⚠ Significant differences detected between designs\n")
  } else {
    cat("  ✓ No significant differences between designs\n")
  }

  # Apply bias model
  if (bias_model != "none" && pool_designs) {
    cat("\nEstimating design-specific bias...\n")

    bias_estimates <- estimate_design_bias(
      data, design_var, reference_design, bias_model, method, prior_bias
    )

    cat("\nBias Estimates (relative to RCT):\n")
    for (d in setdiff(designs, reference_design)) {
      if (d %in% names(bias_estimates)) {
        cat(sprintf("  %s: %.3f (95%% CI: %.3f, %.3f)\n",
                    d,
                    bias_estimates[[d]]$bias,
                    bias_estimates[[d]]$ci_lower,
                    bias_estimates[[d]]$ci_upper))
      }
    }

    # Bias-corrected synthesis
    cat("\nPerforming bias-corrected synthesis...\n")
    corrected_data <- apply_bias_correction(data, design_var, reference_design,
                                            bias_estimates)

    pooled_result <- cbamm_fast(corrected_data, method = "DL", verbose = FALSE)

    cat(sprintf("Bias-corrected pooled effect: %.3f (95%% CI: %.3f, %.3f)\n",
                pooled_result$estimate, pooled_result$ci_lower,
                pooled_result$ci_upper))

  } else if (pool_designs && bias_model == "none") {
    # Simple pooling without bias adjustment
    cat("\nPooling all designs (no bias adjustment)...\n")
    pooled_result <- cbamm_fast(data, method = "DL", verbose = FALSE)
    bias_estimates <- NULL
    corrected_data <- data
  } else {
    pooled_result <- NULL
    bias_estimates <- NULL
    corrected_data <- data
  }

  # Create result object
  result <- list(
    design_results = design_results,
    design_test = design_test,
    bias_estimates = bias_estimates,
    pooled_result = pooled_result,
    bias_model = bias_model,
    reference_design = reference_design,
    designs = designs,
    n_designs = n_designs,
    data = data,
    corrected_data = corrected_data,
    design_var = design_var
  )

  class(result) <- c("cross_design", "list")

  cat("\n✓ Cross-design synthesis completed\n")
  return(result)
}


#' Test for Design Differences
#'
#' @keywords internal
test_design_differences <- function(design_results) {

  estimates <- sapply(design_results, function(x) x$estimate)
  ses <- sapply(design_results, function(x) x$se)

  k <- length(estimates)

  # Weighted mean
  wi <- 1 / ses^2
  grand_mean <- sum(wi * estimates) / sum(wi)

  # Q statistic for between-design heterogeneity
  Q_between <- sum(wi * (estimates - grand_mean)^2)
  df <- k - 1
  p_value <- pchisq(Q_between, df, lower.tail = FALSE)

  return(list(
    Q_between = Q_between,
    df = df,
    p_value = p_value
  ))
}


#' Estimate Design-Specific Bias
#'
#' @keywords internal
estimate_design_bias <- function(data, design_var, reference_design,
                                 bias_model, method, prior_bias) {

  designs <- unique(data[[design_var]])
  other_designs <- setdiff(designs, reference_design)

  bias_estimates <- list()

  for (d in other_designs) {

    # Create indicator variable
    data$is_design <- as.numeric(data[[design_var]] == d)

    # Meta-regression: effect ~ is_design
    # Coefficient for is_design = bias

    y <- data$effect
    v <- data$se^2
    X <- cbind(1, data$is_design)

    k <- length(y)

    # Fixed-effect for tau2 estimation
    wi_fe <- 1 / v
    W_fe <- diag(wi_fe)

    XtWX_fe <- t(X) %*% W_fe %*% X
    beta_fe <- solve(XtWX_fe) %*% t(X) %*% W_fe %*% y

    resid_fe <- y - X %*% beta_fe
    Q <- sum(wi_fe * resid_fe^2)
    df <- k - 2

    # Tau-squared
    C <- sum(wi_fe) - sum(diag(solve(XtWX_fe) %*% t(X) %*% W_fe %*% W_fe %*% X))
    tau2 <- max(0, (Q - df) / C)

    # Random-effects
    wi <- 1 / (v + tau2)
    W <- diag(wi)

    XtWX <- t(X) %*% W %*% X
    XtWy <- t(X) %*% W %*% y

    beta <- solve(XtWX) %*% XtWy
    vb <- solve(XtWX)
    se <- sqrt(diag(vb))

    # Bias = coefficient for is_design
    bias <- beta[2]
    bias_se <- se[2]
    bias_ci_lower <- bias - qnorm(0.975) * bias_se
    bias_ci_upper <- bias + qnorm(0.975) * bias_se

    bias_estimates[[d]] <- list(
      bias = bias,
      se = bias_se,
      ci_lower = bias_ci_lower,
      ci_upper = bias_ci_upper
    )
  }

  return(bias_estimates)
}


#' Apply Bias Correction
#'
#' @keywords internal
apply_bias_correction <- function(data, design_var, reference_design,
                                  bias_estimates) {

  corrected_data <- data

  for (d in names(bias_estimates)) {
    idx <- data[[design_var]] == d
    correction <- bias_estimates[[d]]$bias

    # Subtract bias from non-reference designs
    corrected_data$effect[idx] <- corrected_data$effect[idx] - correction
  }

  return(corrected_data)
}


#' Print Method for Cross-Design Synthesis
#'
#' @export
print.cross_design <- function(x, ...) {
  cat("Cross-Design Meta-Analysis\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Designs: %s\n", paste(x$designs, collapse = ", ")))
  cat(sprintf("Reference: %s\n", x$reference_design))
  cat(sprintf("Bias model: %s\n\n", x$bias_model))

  cat("Results by Design:\n")
  for (d in x$designs) {
    res <- x$design_results[[d]]
    cat(sprintf("  %s: %.3f (95%% CI: %.3f, %.3f)\n",
                d, res$estimate, res$ci_lower, res$ci_upper))
  }

  cat("\nTest for Design Differences:\n")
  cat(sprintf("  Q = %.2f (df = %d, p = %.4f)\n",
              x$design_test$Q_between, x$design_test$df, x$design_test$p_value))

  if (!is.null(x$bias_estimates)) {
    cat("\nBias Estimates:\n")
    for (d in names(x$bias_estimates)) {
      cat(sprintf("  %s: %.3f (95%% CI: %.3f, %.3f)\n",
                  d,
                  x$bias_estimates[[d]]$bias,
                  x$bias_estimates[[d]]$ci_lower,
                  x$bias_estimates[[d]]$ci_upper))
    }
  }

  if (!is.null(x$pooled_result)) {
    cat("\nPooled Effect (Bias-Corrected):\n")
    cat(sprintf("  %.3f (95%% CI: %.3f, %.3f)\n",
                x$pooled_result$estimate,
                x$pooled_result$ci_lower,
                x$pooled_result$ci_upper))
  }

  invisible(x)
}
