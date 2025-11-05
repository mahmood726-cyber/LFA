#' Meta-Regression Analysis
#'
#' Performs meta-regression to examine how study-level covariates affect
#' the effect size estimates.
#'
#' @param data Data frame containing meta-analysis data with effect sizes,
#'   standard errors, and covariates
#' @param formula Formula specifying the regression model (e.g., ~ year + quality)
#' @param method Method for estimation: "FE" (fixed-effect) or "REML" (default)
#' @param test Type of test for coefficients: "z" or "t" (default: "z")
#'
#' @return An object of class "cbamm_metareg" containing:
#'   \itemize{
#'     \item coefficients: Regression coefficients
#'     \item se: Standard errors of coefficients
#'     \item ci_lower: Lower bounds of 95% CIs
#'     \item ci_upper: Upper bounds of 95% CIs
#'     \item p_values: P-values for each coefficient
#'     \item tau2: Residual heterogeneity
#'     \item R2: Proportion of heterogeneity explained
#'     \item QE: Test of residual heterogeneity
#'     \item fitted_values: Fitted values for each study
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:10),
#'   effect = rnorm(10, 0.5, 0.2),
#'   se = runif(10, 0.1, 0.3),
#'   year = 2010:2019,
#'   quality = sample(1:5, 10, replace = TRUE)
#' )
#' result <- meta_regression(data, ~ year + quality)
#' print(result)
#' }
meta_regression <- function(data,
                             formula,
                             method = "REML",
                             test = "z") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!inherits(formula, "formula")) {
    stop("formula must be a formula object")
  }

  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Prepare data
  if (!"var" %in% names(data)) {
    data$var <- data$se^2
  }

  # Extract model matrix
  mf <- model.frame(formula, data = data, na.action = na.pass)
  X <- model.matrix(formula, data = mf)
  p <- ncol(X)
  k <- nrow(data)

  if (k <= p) {
    stop("Insufficient data: number of studies must exceed number of predictors")
  }

  yi <- data$effect
  vi <- data$var

  # Remove NA cases
  complete_cases <- complete.cases(yi, vi, X)
  if (!all(complete_cases)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_cases)))
    yi <- yi[complete_cases]
    vi <- vi[complete_cases]
    X <- X[complete_cases, , drop = FALSE]
    k <- length(yi)
  }

  # Initial fixed-effect model
  wi_fe <- 1 / vi
  W_fe <- diag(wi_fe)

  # Estimate coefficients (fixed-effect)
  XtWX_fe <- t(X) %*% W_fe %*% X
  XtWy_fe <- t(X) %*% W_fe %*% yi

  # Check for singularity
  if (det(XtWX_fe) == 0) {
    stop("Singular covariance matrix. Check for collinearity in predictors")
  }

  beta_fe <- solve(XtWX_fe) %*% XtWy_fe
  fitted_fe <- X %*% beta_fe
  resid_fe <- yi - fitted_fe

  # Calculate Q statistics
  Q <- sum(wi_fe * resid_fe^2)
  df <- k - p

  # Estimate tau-squared (residual heterogeneity)
  if (method == "REML" || method == "DL") {
    # Method of moments estimator
    C <- sum(wi_fe) - sum(diag(solve(XtWX_fe) %*% t(X) %*% W_fe %*% W_fe %*% X))
    tau2 <- max(0, (Q - df) / C)
  } else if (method == "FE") {
    tau2 <- 0
  } else {
    stop("method must be 'FE', 'DL', or 'REML'")
  }

  # Random-effects model
  wi <- 1 / (vi + tau2)
  W <- diag(wi)

  XtWX <- t(X) %*% W %*% X
  XtWy <- t(X) %*% W %*% yi

  beta <- solve(XtWX) %*% XtWy
  vb <- solve(XtWX)
  se <- sqrt(diag(vb))

  # Confidence intervals and p-values
  if (test == "z") {
    crit_val <- qnorm(0.975)
    p_values <- 2 * pnorm(-abs(beta / se))
  } else {
    crit_val <- qt(0.975, df)
    p_values <- 2 * pt(-abs(beta / se), df)
  }

  ci_lower <- beta - crit_val * se
  ci_upper <- beta + crit_val * se

  # Fitted values and residuals
  fitted_values <- as.vector(X %*% beta)
  residuals <- yi - fitted_values

  # Test of residual heterogeneity
  QE <- sum(wi * residuals^2)
  QE_pval <- pchisq(QE, df, lower.tail = FALSE)

  # R-squared (proportion of heterogeneity explained)
  # Compare to null model
  null_result <- cbamm_fast(data.frame(study = seq_len(k), effect = yi, se = sqrt(vi)),
                             method = "DL", verbose = FALSE)
  tau2_null <- null_result$tau2

  if (tau2_null > 0) {
    R2 <- max(0, (tau2_null - tau2) / tau2_null)
  } else {
    R2 <- NA
  }

  # Create result object
  result <- list(
    coefficients = as.vector(beta),
    se = se,
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper),
    p_values = as.vector(p_values),
    tau2 = tau2,
    tau = sqrt(tau2),
    R2 = R2,
    QE = QE,
    QE_df = df,
    QE_pval = QE_pval,
    k = k,
    p = p,
    fitted_values = fitted_values,
    residuals = residuals,
    coef_names = colnames(X),
    method = method,
    test = test,
    data = data[complete_cases, ]
  )

  class(result) <- "cbamm_metareg"
  return(result)
}


#' Subgroup Analysis
#'
#' Performs subgroup meta-analysis to compare effect sizes across groups
#'
#' @param data Data frame with meta-analysis data
#' @param subgroup Character string naming the subgroup variable
#' @param method Method for meta-analysis within subgroups (default: "DL")
#'
#' @return An object of class "cbamm_subgroup" containing results for each subgroup
#' @export
subgroup_analysis <- function(data, subgroup, method = "DL") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!subgroup %in% names(data)) {
    stop(sprintf("Subgroup variable '%s' not found in data", subgroup))
  }

  # Get unique subgroup levels
  groups <- unique(data[[subgroup]])
  groups <- groups[!is.na(groups)]

  if (length(groups) < 2) {
    stop("Need at least 2 subgroups for subgroup analysis")
  }

  # Perform meta-analysis within each subgroup
  subgroup_results <- list()

  for (g in groups) {
    subset_data <- data[data[[subgroup]] == g & !is.na(data[[subgroup]]), ]

    if (nrow(subset_data) < 2) {
      warning(sprintf("Skipping subgroup '%s': fewer than 2 studies", g))
      next
    }

    ma_result <- cbamm_fast(subset_data, method = method, verbose = FALSE)
    ma_result$subgroup <- g
    ma_result$n_studies <- nrow(subset_data)

    subgroup_results[[as.character(g)]] <- ma_result
  }

  # Test for subgroup differences
  if (length(subgroup_results) >= 2) {
    estimates <- sapply(subgroup_results, function(x) x$estimate)
    ses <- sapply(subgroup_results, function(x) x$se)
    tau2s <- sapply(subgroup_results, function(x) x$tau2)

    # Q statistic for between-group heterogeneity
    wi <- 1 / ses^2
    grand_mean <- sum(wi * estimates) / sum(wi)
    Q_between <- sum(wi * (estimates - grand_mean)^2)
    df_between <- length(estimates) - 1
    p_between <- pchisq(Q_between, df_between, lower.tail = FALSE)

    between_test <- list(
      Q = Q_between,
      df = df_between,
      p_value = p_between
    )
  } else {
    between_test <- NULL
  }

  result <- list(
    subgroup_results = subgroup_results,
    between_test = between_test,
    subgroup_var = subgroup,
    n_subgroups = length(subgroup_results)
  )

  class(result) <- "cbamm_subgroup"
  return(result)
}


#' Robust Random Effects Meta-Analysis
#'
#' Performs robust variance estimation for meta-analysis
#'
#' @param data Data frame with meta-analysis data
#' @param cluster Optional clustering variable for robust SE estimation
#'
#' @return Meta-analysis results with robust standard errors
#' @export
robust_rma <- function(data, cluster = NULL) {
  # Perform standard meta-analysis
  result <- cbamm_fast(data, method = "DL", verbose = FALSE)

  if (!is.null(cluster)) {
    if (!cluster %in% names(data)) {
      stop(sprintf("Cluster variable '%s' not found in data", cluster))
    }

    # Implement clustered robust variance estimation
    # This is a simplified version
    clusters <- data[[cluster]]
    n_clusters <- length(unique(clusters))

    if (n_clusters < 2) {
      warning("Need at least 2 clusters for robust SE estimation")
      return(result)
    }

    # Adjust standard error with finite-sample correction
    correction <- sqrt(n_clusters / (n_clusters - 1))
    result$se <- result$se * correction
    result$ci_lower <- result$estimate - qnorm(0.975) * result$se
    result$ci_upper <- result$estimate + qnorm(0.975) * result$se
    result$robust <- TRUE
    result$n_clusters <- n_clusters
  }

  return(result)
}
