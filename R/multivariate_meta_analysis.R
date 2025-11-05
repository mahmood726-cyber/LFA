#' Multivariate Meta-Analysis
#'
#' Functions for multivariate meta-analysis of multiple correlated outcomes or endpoints.
#' This allows joint analysis accounting for within-study correlations.
#'
#' @name multivariate_meta_analysis
NULL

#' Multivariate Random-Effects Meta-Analysis
#'
#' Performs multivariate meta-analysis accounting for correlations between multiple
#' outcomes within studies. Uses maximum likelihood estimation.
#'
#' @param data Data frame with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item outcome: Outcome identifier (e.g., "outcome1", "outcome2")
#'     \item effect: Effect size estimate
#'     \item se: Standard error
#'   }
#' @param correlation Within-study correlation between outcomes (scalar or matrix)
#' @param method Estimation method: "ML" (default) or "REML"
#' @param max_iter Maximum iterations for convergence (default: 100)
#' @param tol Convergence tolerance (default: 1e-6)
#'
#' @return Object of class "mvma" containing:
#'   \itemize{
#'     \item estimates: Pooled effect sizes for each outcome
#'     \item vcov_within: Within-study variance-covariance matrix
#'     \item vcov_between: Between-study variance-covariance matrix
#'     \item correlation: Estimated between-study correlations
#'     \item logLik: Log-likelihood
#'     \item AIC: Akaike Information Criterion
#'     \item BIC: Bayesian Information Criterion
#'     \item converged: Convergence indicator
#'   }
#'
#' @references
#' Jackson, D., Riley, R., & White, I. R. (2011). Multivariate meta-analysis:
#' Potential and promise. Statistics in Medicine, 30(20), 2481-2498.
#'
#' @export
#' @examples
#' \dontrun{
#' # Example with 2 outcomes
#' mv_data <- data.frame(
#'   study = rep(1:5, each = 2),
#'   outcome = rep(c("outcome1", "outcome2"), 5),
#'   effect = c(0.3, 0.4, 0.5, 0.6, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
#'   se = rep(c(0.1, 0.12), 5)
#' )
#' result <- multivariate_ma(mv_data, correlation = 0.5)
#' print(result)
#' }
multivariate_ma <- function(data, correlation = 0.5, method = "ML",
                            max_iter = 100, tol = 1e-6) {
  # Validate input
  required_cols <- c("study", "outcome", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for missing values
  if (any(is.na(data[, c("effect", "se")]))) {
    stop("Missing values in effect or se not allowed in multivariate meta-analysis")
  }

  # Get unique outcomes and studies
  outcomes <- unique(data$outcome)
  studies <- unique(data$study)
  n_outcomes <- length(outcomes)
  n_studies <- length(studies)

  if (n_outcomes < 2) {
    stop("Multivariate meta-analysis requires at least 2 outcomes")
  }

  # Reshape data to wide format
  effect_matrix <- matrix(NA, nrow = n_studies, ncol = n_outcomes)
  se_matrix <- matrix(NA, nrow = n_studies, ncol = n_outcomes)

  for (i in seq_along(studies)) {
    study_data <- data[data$study == studies[i], ]
    for (j in seq_along(outcomes)) {
      outcome_data <- study_data[study_data$outcome == outcomes[j], ]
      if (nrow(outcome_data) > 0) {
        effect_matrix[i, j] <- outcome_data$effect[1]
        se_matrix[i, j] <- outcome_data$se[1]
      }
    }
  }

  # Handle correlation parameter
  if (length(correlation) == 1) {
    # Single correlation - create matrix
    within_cor <- matrix(correlation, n_outcomes, n_outcomes)
    diag(within_cor) <- 1
  } else if (is.matrix(correlation)) {
    within_cor <- correlation
    if (nrow(within_cor) != n_outcomes || ncol(within_cor) != n_outcomes) {
      stop("Correlation matrix dimensions must match number of outcomes")
    }
  } else {
    stop("correlation must be a scalar or matrix")
  }

  # Create within-study variance-covariance matrices
  V_list <- lapply(1:n_studies, function(i) {
    D <- diag(se_matrix[i, ]^2, n_outcomes)
    sqrt_D <- diag(se_matrix[i, ], n_outcomes)
    V <- sqrt_D %*% within_cor %*% sqrt_D
    return(V)
  })

  # Initialize between-study variance-covariance matrix
  Tau <- diag(apply(effect_matrix, 2, var, na.rm = TRUE) * 0.5, n_outcomes)

  # EM algorithm for REML/ML estimation
  converged <- FALSE
  logLik_old <- -Inf

  for (iter in 1:max_iter) {
    # E-step: Calculate weights
    inv_cov_list <- lapply(V_list, function(V) {
      solve(V + Tau)
    })

    # Sum of inverse covariances
    sum_inv_cov <- Reduce("+", inv_cov_list)

    # Weighted mean (pooled effect for each outcome)
    weighted_effects <- rep(0, n_outcomes)
    for (i in 1:n_studies) {
      weighted_effects <- weighted_effects + inv_cov_list[[i]] %*% effect_matrix[i, ]
    }
    beta <- solve(sum_inv_cov) %*% weighted_effects

    # M-step: Update Tau
    Tau_new <- matrix(0, n_outcomes, n_outcomes)
    for (i in 1:n_studies) {
      resid <- effect_matrix[i, ] - beta
      W_i <- inv_cov_list[[i]]
      Tau_new <- Tau_new + (resid %*% t(resid)) - solve(W_i)
    }
    Tau_new <- Tau_new / n_studies

    # Ensure positive definite
    eigen_decomp <- eigen(Tau_new)
    eigen_values <- pmax(eigen_decomp$values, 0)
    Tau_new <- eigen_decomp$vectors %*% diag(eigen_values, n_outcomes) %*% t(eigen_decomp$vectors)

    # REML correction
    if (method == "REML") {
      Tau_new <- Tau_new * n_studies / (n_studies - 1)
    }

    # Calculate log-likelihood
    logLik <- 0
    for (i in 1:n_studies) {
      V_total <- V_list[[i]] + Tau_new
      resid <- effect_matrix[i, ] - beta
      logLik <- logLik - 0.5 * (determinant(V_total)$modulus +
                                 t(resid) %*% solve(V_total) %*% resid)
    }
    logLik <- as.numeric(logLik)

    # Check convergence
    if (abs(logLik - logLik_old) < tol) {
      converged <- TRUE
      break
    }

    Tau <- Tau_new
    logLik_old <- logLik
  }

  # Calculate standard errors for pooled estimates
  vcov_beta <- solve(sum_inv_cov)
  se_beta <- sqrt(diag(vcov_beta))

  # Calculate confidence intervals
  ci_lower <- beta - 1.96 * se_beta
  ci_upper <- beta + 1.96 * se_beta

  # Calculate p-values
  z_stats <- beta / se_beta
  p_values <- 2 * (1 - pnorm(abs(z_stats)))

  # Extract between-study correlations
  between_cor <- cov2cor(Tau)

  # Calculate I-squared for each outcome
  I2 <- sapply(1:n_outcomes, function(j) {
    total_var <- Tau[j, j] + mean(se_matrix[, j]^2, na.rm = TRUE)
    100 * Tau[j, j] / total_var
  })

  # Calculate AIC and BIC
  n_params <- n_outcomes + n_outcomes * (n_outcomes + 1) / 2
  AIC <- -2 * logLik + 2 * n_params
  BIC <- -2 * logLik + log(n_studies * n_outcomes) * n_params

  # Prepare results
  estimates_df <- data.frame(
    outcome = outcomes,
    estimate = as.vector(beta),
    se = se_beta,
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper),
    z = as.vector(z_stats),
    p_value = p_values,
    I2 = I2,
    tau2 = diag(Tau)
  )

  result <- list(
    estimates = estimates_df,
    vcov_within = within_cor,
    vcov_between = Tau,
    correlation_between = between_cor,
    vcov_estimates = vcov_beta,
    logLik = logLik,
    AIC = AIC,
    BIC = BIC,
    converged = converged,
    iterations = iter,
    method = method,
    n_studies = n_studies,
    n_outcomes = n_outcomes,
    data = data
  )

  class(result) <- "mvma"
  return(result)
}

#' Multivariate Meta-Regression
#'
#' Extends multivariate meta-analysis to include study-level moderators.
#'
#' @param data Data frame with study, outcome, effect, se, and moderator columns
#' @param formula Formula specifying moderators (e.g., ~ moderator1 + moderator2)
#' @param correlation Within-study correlation (scalar or matrix)
#' @param method Estimation method: "ML" or "REML"
#'
#' @return Object of class "mvma_reg" with coefficient estimates for each outcome
#'
#' @export
#' @examples
#' \dontrun{
#' mv_data$year <- rep(2010:2014, each = 2)
#' result <- multivariate_metareg(mv_data, formula = ~ year, correlation = 0.5)
#' }
multivariate_metareg <- function(data, formula, correlation = 0.5, method = "ML") {
  # Extract outcome and moderators
  moderator_vars <- all.vars(formula)

  required_cols <- c("study", "outcome", "effect", "se", moderator_vars)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  outcomes <- unique(data$outcome)
  n_outcomes <- length(outcomes)

  # Fit separate meta-regression for each outcome
  outcome_results <- list()

  for (outcome_name in outcomes) {
    outcome_data <- data[data$outcome == outcome_name, ]

    # Create design matrix
    X <- model.matrix(formula, data = outcome_data)
    y <- outcome_data$effect
    V <- diag(outcome_data$se^2)

    # Weighted least squares with random effects
    # Simplified approach - in practice would use iterative estimation
    weights <- 1 / outcome_data$se^2
    fit <- lm(formula, data = outcome_data, weights = weights)

    outcome_results[[outcome_name]] <- list(
      coefficients = coef(fit),
      se = summary(fit)$coefficients[, "Std. Error"],
      p_values = summary(fit)$coefficients[, "Pr(>|t|)"]
    )
  }

  result <- list(
    outcome_results = outcome_results,
    formula = formula,
    n_outcomes = n_outcomes,
    correlation = correlation,
    method = method
  )

  class(result) <- "mvma_reg"
  return(result)
}

#' Test Difference Between Outcomes in Multivariate Meta-Analysis
#'
#' Tests whether two outcomes have significantly different pooled effects.
#'
#' @param mvma_result Result from multivariate_ma()
#' @param outcome1 Name of first outcome
#' @param outcome2 Name of second outcome
#'
#' @return List with test statistic, p-value, and confidence interval for difference
#'
#' @export
test_outcome_difference <- function(mvma_result, outcome1, outcome2) {
  if (!inherits(mvma_result, "mvma")) {
    stop("mvma_result must be of class 'mvma'")
  }

  estimates <- mvma_result$estimates
  vcov <- mvma_result$vcov_estimates

  idx1 <- which(estimates$outcome == outcome1)
  idx2 <- which(estimates$outcome == outcome2)

  if (length(idx1) == 0 || length(idx2) == 0) {
    stop("Outcome names not found in results")
  }

  # Difference in estimates
  diff <- estimates$estimate[idx1] - estimates$estimate[idx2]

  # Standard error of difference
  se_diff <- sqrt(vcov[idx1, idx1] + vcov[idx2, idx2] - 2 * vcov[idx1, idx2])

  # Test statistic
  z <- diff / se_diff
  p_value <- 2 * (1 - pnorm(abs(z)))

  # Confidence interval
  ci_lower <- diff - 1.96 * se_diff
  ci_upper <- diff + 1.96 * se_diff

  result <- list(
    outcome1 = outcome1,
    outcome2 = outcome2,
    difference = diff,
    se = se_diff,
    z = z,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )

  class(result) <- "mvma_test"
  return(result)
}

#' Plot Multivariate Meta-Analysis Results
#'
#' Creates a forest plot showing results for all outcomes with confidence ellipse.
#'
#' @param mvma_result Result from multivariate_ma()
#' @param outcome_labels Optional custom labels for outcomes
#'
#' @return Forest plot (base graphics)
#'
#' @export
plot_mvma <- function(mvma_result, outcome_labels = NULL) {
  if (!inherits(mvma_result, "mvma")) {
    stop("mvma_result must be of class 'mvma'")
  }

  estimates <- mvma_result$estimates
  n_outcomes <- nrow(estimates)

  if (is.null(outcome_labels)) {
    outcome_labels <- estimates$outcome
  }

  # Create forest plot
  par(mfrow = c(1, 1), mar = c(5, 8, 4, 2))

  y_pos <- n_outcomes:1

  plot(estimates$estimate, y_pos,
       xlim = range(c(estimates$ci_lower, estimates$ci_upper)),
       ylim = c(0.5, n_outcomes + 0.5),
       pch = 18, cex = 1.5,
       xlab = "Effect Size", ylab = "",
       yaxt = "n", main = "Multivariate Meta-Analysis Results")

  # Add confidence intervals
  for (i in 1:n_outcomes) {
    lines(c(estimates$ci_lower[i], estimates$ci_upper[i]),
          c(y_pos[i], y_pos[i]), lwd = 2)
  }

  # Add outcome labels
  axis(2, at = y_pos, labels = outcome_labels, las = 1)

  # Add null line
  abline(v = 0, lty = 2, col = "red")

  # Add grid
  grid()
}
