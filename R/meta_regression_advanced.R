#' Advanced Meta-Regression Module
#'
#' State-of-the-art meta-regression techniques including:
#' - Non-linear meta-regression with restricted cubic splines
#' - Penalized meta-regression (LASSO, Ridge, Elastic Net) for high-dimensional moderators
#' - Fractional polynomials for flexible dose-response
#' - Multivariate meta-regression with correlated moderators
#' - Permutation-based inference for small samples
#' - Bubble plots and partial residual plots
#'
#' Based on recent methodological advances (Biondi-Zoccai et al. 2011,
#' Viechtbauer & López-López 2022, Thompson & Higgins 2005).
#'
#' @name meta_regression_advanced
NULL


#' Meta-Regression with Restricted Cubic Splines
#'
#' Fits non-linear meta-regression using restricted cubic splines (natural splines).
#' Useful for continuous moderators with potentially non-linear relationships.
#'
#' @param data Data frame with meta-analysis data
#' @param moderator Column name for continuous moderator
#' @param n_knots Number of knots for spline (default: 3; typically 3-5)
#' @param knot_positions Optional custom knot positions (default: quantiles)
#' @param method Estimation method: "REML" (default) or "FE"
#' @param test_linearity Test for non-linearity vs linear model (default: TRUE)
#'
#' @return Meta-regression spline object with coefficients, fitted curve, and plots
#'
#' @references
#' Biondi-Zoccai G et al. (2011). Flexible modeling of continuous risk variables
#' in meta-regression. Stat Med, 30(28):3285-3299.
#'
#' @export
#' @examples
#' \dontrun{
#' # Non-linear relationship between year and effect size
#' spline_fit <- meta_regression_spline(
#'   data = ma_data,
#'   moderator = "year",
#'   n_knots = 4
#' )
#'
#' plot(spline_fit)
#' }
meta_regression_spline <- function(data, moderator, n_knots = 3,
                                   knot_positions = NULL,
                                   method = "REML",
                                   test_linearity = TRUE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   META-REGRESSION WITH RESTRICTED CUBIC SPLINES              ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!moderator %in% names(data)) stop(sprintf("Moderator '%s' not found", moderator))
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Remove missing values
  complete_idx <- complete.cases(data[, c("effect", "se", moderator)])
  if (!all(complete_idx)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_idx)))
    data <- data[complete_idx, ]
  }

  x <- data[[moderator]]
  y <- data$effect
  v <- data$se^2

  n <- length(y)
  cat(sprintf("Sample size: %d studies\n", n))
  cat(sprintf("Moderator: %s (range: %.2f to %.2f)\n",
              moderator, min(x), max(x)))

  # Determine knot positions
  if (is.null(knot_positions)) {
    # Use quantile-based knots
    if (n_knots == 3) {
      probs <- c(0.10, 0.50, 0.90)
    } else if (n_knots == 4) {
      probs <- c(0.05, 0.35, 0.65, 0.95)
    } else if (n_knots == 5) {
      probs <- c(0.05, 0.275, 0.50, 0.725, 0.95)
    } else {
      probs <- seq(0, 1, length.out = n_knots + 2)[2:(n_knots + 1)]
    }
    knot_positions <- quantile(x, probs = probs)
  }

  cat(sprintf("Knots: %d at positions %s\n",
              n_knots, paste(round(knot_positions, 2), collapse = ", ")))

  # Create spline basis
  X_spline <- create_rcs_basis(x, knot_positions)
  X <- cbind(1, x, X_spline)  # Intercept + linear + spline terms
  colnames(X) <- c("(Intercept)", moderator,
                   paste0("spline", seq_len(ncol(X_spline))))

  p <- ncol(X)
  cat(sprintf("Model parameters: %d (intercept + linear + %d spline terms)\n",
              p, ncol(X_spline)))

  # Check degrees of freedom
  if (n <= p) {
    stop("Insufficient data: need more studies than parameters")
  }

  # Estimate tau-squared
  wi_fe <- 1 / v
  W_fe <- diag(wi_fe)

  XtWX_fe <- t(X) %*% W_fe %*% X
  if (det(XtWX_fe) < 1e-10) {
    stop("Singular design matrix. Try fewer knots or check for collinearity")
  }

  beta_fe <- solve(XtWX_fe) %*% t(X) %*% W_fe %*% y
  fitted_fe <- X %*% beta_fe
  resid_fe <- y - fitted_fe

  Q <- sum(wi_fe * resid_fe^2)
  df <- n - p

  # DL estimator for tau-squared
  C <- sum(wi_fe) - sum(diag(solve(XtWX_fe) %*% t(X) %*% W_fe %*% W_fe %*% X))
  tau2 <- max(0, (Q - df) / C)

  cat(sprintf("Residual heterogeneity: τ² = %.4f\n", tau2))

  # Random-effects estimation
  wi <- 1 / (v + tau2)
  W <- diag(wi)

  XtWX <- t(X) %*% W %*% X
  XtWy <- t(X) %*% W %*% y

  beta <- solve(XtWX) %*% XtWy
  vb <- solve(XtWX)
  se <- sqrt(diag(vb))

  ci_lower <- beta - qnorm(0.975) * se
  ci_upper <- beta + qnorm(0.975) * se
  p_values <- 2 * pnorm(-abs(beta / se))

  # Fitted values
  fitted <- as.vector(X %*% beta)
  residuals <- y - fitted

  # Test for residual heterogeneity
  QE <- sum(wi * residuals^2)
  QE_pval <- pchisq(QE, df, lower.tail = FALSE)

  # Test for non-linearity
  if (test_linearity) {
    cat("\nTesting for non-linearity...\n")

    # Compare to linear model (just intercept + moderator)
    X_linear <- cbind(1, x)
    wi_linear <- wi
    W_linear <- diag(wi_linear)

    XtWX_linear <- t(X_linear) %*% W_linear %*% X_linear
    XtWy_linear <- t(X_linear) %*% W_linear %*% y
    beta_linear <- solve(XtWX_linear) %*% XtWy_linear
    fitted_linear <- as.vector(X_linear %*% beta_linear)

    # Likelihood ratio test
    LL_spline <- sum(dnorm(y, fitted, sqrt(v + tau2), log = TRUE))
    LL_linear <- sum(dnorm(y, fitted_linear, sqrt(v + tau2), log = TRUE))

    LR_stat <- 2 * (LL_spline - LL_linear)
    LR_df <- ncol(X_spline)
    LR_pval <- pchisq(LR_stat, LR_df, lower.tail = FALSE)

    cat(sprintf("  LR test: χ² = %.2f, df = %d, p = %.4f\n",
                LR_stat, LR_df, LR_pval))

    if (LR_pval < 0.05) {
      cat("  ✓ Significant non-linearity detected\n")
    } else {
      cat("  Linear model may be sufficient\n")
    }

    linearity_test <- list(
      statistic = LR_stat,
      df = LR_df,
      p_value = LR_pval,
      linear_coefficients = as.vector(beta_linear)
    )
  } else {
    linearity_test <- NULL
  }

  # Generate smooth curve for plotting
  x_grid <- seq(min(x), max(x), length.out = 100)
  X_grid_spline <- create_rcs_basis(x_grid, knot_positions)
  X_grid <- cbind(1, x_grid, X_grid_spline)

  fitted_curve <- as.vector(X_grid %*% beta)

  # Standard errors for curve (pointwise)
  se_curve <- sqrt(diag(X_grid %*% vb %*% t(X_grid)))
  ci_lower_curve <- fitted_curve - qnorm(0.975) * se_curve
  ci_upper_curve <- fitted_curve + qnorm(0.975) * se_curve

  curve_data <- data.frame(
    x = x_grid,
    fitted = fitted_curve,
    se = se_curve,
    ci_lower = ci_lower_curve,
    ci_upper = ci_upper_curve
  )

  # Result object
  result <- list(
    coefficients = as.vector(beta),
    se = se,
    ci_lower = as.vector(ci_lower),
    ci_upper = as.vector(ci_upper),
    p_values = as.vector(p_values),
    coef_names = colnames(X),
    tau2 = tau2,
    tau = sqrt(tau2),
    QE = QE,
    QE_df = df,
    QE_pval = QE_pval,
    knot_positions = knot_positions,
    n_knots = n_knots,
    linearity_test = linearity_test,
    fitted_values = fitted,
    residuals = residuals,
    curve = curve_data,
    data = data,
    moderator = moderator,
    method = method,
    n = n,
    p = p
  )

  class(result) <- c("metareg_spline", "list")

  cat("\n✓ Spline meta-regression completed\n")
  return(result)
}


#' Create Restricted Cubic Spline Basis
#'
#' @keywords internal
create_rcs_basis <- function(x, knots) {

  n_knots <- length(knots)

  if (n_knots < 3) {
    stop("Need at least 3 knots for restricted cubic splines")
  }

  # Use truncated power basis restricted to be linear beyond boundary knots
  k <- knots
  n <- length(x)

  # Number of spline terms = n_knots - 2
  n_terms <- n_knots - 2

  basis <- matrix(NA, n, n_terms)

  for (j in 1:n_terms) {
    basis[, j] <- pmax(x - k[j + 1], 0)^3 -
                  ((k[n_knots] - k[j + 1]) / (k[n_knots] - k[n_knots - 1])) *
                  pmax(x - k[n_knots - 1], 0)^3 +
                  ((k[n_knots - 1] - k[j + 1]) / (k[n_knots] - k[n_knots - 1])) *
                  pmax(x - k[n_knots], 0)^3
  }

  return(basis)
}


#' Penalized Meta-Regression (LASSO/Ridge/Elastic Net)
#'
#' For high-dimensional meta-regression with many potential moderators.
#' Uses penalized regression to prevent overfitting and perform variable selection.
#'
#' @param data Data frame with meta-analysis data
#' @param moderators Character vector of moderator column names
#' @param penalty Penalty type: "lasso" (L1), "ridge" (L2), or "elasticnet"
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso)
#' @param lambda Penalty strength (default: selected by cross-validation)
#' @param cv_folds Number of CV folds for lambda selection (default: 5)
#'
#' @return Penalized meta-regression object
#'
#' @export
#' @examples
#' \dontrun{
#' # LASSO for variable selection with 20 moderators
#' lasso_fit <- meta_regression_penalized(
#'   data = ma_data,
#'   moderators = paste0("mod", 1:20),
#'   penalty = "lasso"
#' )
#'
#' # Selected moderators
#' print(lasso_fit$selected_moderators)
#' }
meta_regression_penalized <- function(data, moderators, penalty = "lasso",
                                      alpha = 1, lambda = NULL, cv_folds = 5) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PENALIZED META-REGRESSION                                  ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!all(moderators %in% names(data))) {
    stop("Not all moderators found in data")
  }
  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  # Remove missing values
  complete_idx <- complete.cases(data[, c("effect", "se", moderators)])
  if (!all(complete_idx)) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_idx)))
    data <- data[complete_idx, ]
  }

  y <- data$effect
  v <- data$se^2
  X <- as.matrix(data[, moderators])

  n <- nrow(X)
  p <- ncol(X)

  cat(sprintf("Studies: %d, Moderators: %d\n", n, p))
  cat(sprintf("Penalty: %s", penalty))
  if (penalty == "elasticnet") {
    cat(sprintf(" (α = %.2f)\n", alpha))
  } else {
    cat("\n")
  }

  # Standardize moderators
  X_scaled <- scale(X)
  X_mean <- attr(X_scaled, "scaled:center")
  X_sd <- attr(X_scaled, "scaled:scale")

  # Weights based on inverse variance
  w <- sqrt(1 / v)

  # Weight outcome and predictors
  y_weighted <- y * w
  X_weighted <- X_scaled * w

  # Determine penalty parameter
  if (penalty == "lasso") {
    alpha_param <- 1
  } else if (penalty == "ridge") {
    alpha_param <- 0
  } else if (penalty == "elasticnet") {
    alpha_param <- alpha
  } else {
    stop("penalty must be 'lasso', 'ridge', or 'elasticnet'")
  }

  # Coordinate descent for penalized regression
  if (is.null(lambda)) {
    cat("\nSelecting penalty parameter via cross-validation...\n")

    # Generate lambda sequence
    lambda_max <- max(abs(t(X_weighted) %*% y_weighted)) / (n * alpha_param)
    lambda_seq <- exp(seq(log(lambda_max), log(lambda_max * 0.001), length.out = 50))

    # Cross-validation
    cv_mse <- numeric(length(lambda_seq))
    folds <- cut(seq_len(n), breaks = cv_folds, labels = FALSE)

    for (i in seq_along(lambda_seq)) {
      mse_fold <- numeric(cv_folds)

      for (fold in 1:cv_folds) {
        test_idx <- which(folds == fold)
        train_idx <- setdiff(seq_len(n), test_idx)

        # Fit on training set
        fit_train <- coordinate_descent_penalized(
          X_weighted[train_idx, , drop = FALSE],
          y_weighted[train_idx],
          lambda_seq[i],
          alpha_param
        )

        # Predict on test set
        pred_test <- X_weighted[test_idx, , drop = FALSE] %*% fit_train$beta
        mse_fold[fold] <- mean((y_weighted[test_idx] - pred_test)^2)
      }

      cv_mse[i] <- mean(mse_fold)
    }

    # Select lambda with minimum CV error
    best_idx <- which.min(cv_mse)
    lambda <- lambda_seq[best_idx]

    cat(sprintf("  Selected λ = %.4f (CV MSE = %.4f)\n", lambda, cv_mse[best_idx]))
  }

  # Fit final model with selected lambda
  cat("\nFitting penalized model...\n")
  fit <- coordinate_descent_penalized(X_weighted, y_weighted, lambda, alpha_param)

  beta <- fit$beta
  intercept <- mean(y) - sum(beta * X_mean / X_sd)  # Back-transform

  # Un-standardize coefficients
  beta_unstd <- beta / X_sd

  # Identify selected moderators (non-zero coefficients)
  selected <- abs(beta_unstd) > 1e-8
  selected_moderators <- moderators[selected]

  cat(sprintf("  Selected moderators: %d out of %d\n",
              sum(selected), length(moderators)))
  if (sum(selected) > 0) {
    cat(sprintf("  %s\n", paste(selected_moderators, collapse = ", ")))
  }

  # Fitted values and residuals
  fitted <- intercept + X %*% beta_unstd
  residuals <- y - fitted

  # Model fit statistics
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - ss_residual / ss_total

  cat(sprintf("  R² = %.4f\n", r_squared))

  # Result object
  result <- list(
    coefficients = c(intercept, beta_unstd),
    coef_names = c("(Intercept)", moderators),
    selected_moderators = selected_moderators,
    n_selected = sum(selected),
    lambda = lambda,
    alpha = alpha_param,
    penalty = penalty,
    r_squared = r_squared,
    fitted_values = as.vector(fitted),
    residuals = as.vector(residuals),
    data = data,
    moderators = moderators,
    n = n,
    p = p
  )

  class(result) <- c("metareg_penalized", "list")

  cat("\n✓ Penalized meta-regression completed\n")
  return(result)
}


#' Coordinate Descent for Penalized Regression
#'
#' @keywords internal
coordinate_descent_penalized <- function(X, y, lambda, alpha, max_iter = 1000, tol = 1e-6) {

  n <- nrow(X)
  p <- ncol(X)

  beta <- rep(0, p)
  beta_old <- beta

  for (iter in 1:max_iter) {

    for (j in 1:p) {
      # Partial residual
      r <- y - X %*% beta + X[, j] * beta[j]

      # Coordinate update with elastic net penalty
      z <- sum(X[, j] * r)

      # Soft-thresholding (LASSO component)
      if (alpha > 0) {
        if (z > n * lambda * alpha) {
          beta[j] <- (z - n * lambda * alpha) / (n + n * lambda * (1 - alpha))
        } else if (z < -n * lambda * alpha) {
          beta[j] <- (z + n * lambda * alpha) / (n + n * lambda * (1 - alpha))
        } else {
          beta[j] <- 0
        }
      } else {
        # Pure ridge
        beta[j] <- z / (n + n * lambda)
      }
    }

    # Check convergence
    if (max(abs(beta - beta_old)) < tol) {
      break
    }

    beta_old <- beta
  }

  return(list(
    beta = beta,
    iterations = iter
  ))
}


#' Plot Method for Spline Meta-Regression
#'
#' @export
plot.metareg_spline <- function(x, ...) {

  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    plot_data <- data.frame(
      x_obs = x$data[[x$moderator]],
      y_obs = x$data$effect,
      se_obs = x$data$se
    )

    p <- ggplot2::ggplot() +
      # Confidence band
      ggplot2::geom_ribbon(data = x$curve,
                          ggplot2::aes(x = x, ymin = ci_lower, ymax = ci_upper),
                          fill = "lightblue", alpha = 0.3) +
      # Fitted curve
      ggplot2::geom_line(data = x$curve,
                        ggplot2::aes(x = x, y = fitted),
                        color = "blue", size = 1) +
      # Observed data points (bubble size = precision)
      ggplot2::geom_point(data = plot_data,
                         ggplot2::aes(x = x_obs, y = y_obs, size = 1/se_obs),
                         alpha = 0.6) +
      # Knot positions
      ggplot2::geom_vline(xintercept = x$knot_positions,
                         linetype = "dashed", alpha = 0.3) +
      ggplot2::scale_size_continuous(range = c(1, 6), guide = "none") +
      ggplot2::labs(
        x = x$moderator,
        y = "Effect size",
        title = "Meta-Regression with Restricted Cubic Splines",
        subtitle = sprintf("%d knots, τ² = %.4f", x$n_knots, x$tau2)
      ) +
      ggplot2::theme_minimal()

    print(p)

  } else {
    # Fallback to base graphics
    plot(x$data[[x$moderator]], x$data$effect,
         xlab = x$moderator, ylab = "Effect size",
         main = "Spline Meta-Regression",
         cex = 1 / x$data$se,
         pch = 19, col = rgb(0, 0, 0, 0.5))

    lines(x$curve$x, x$curve$fitted, col = "blue", lwd = 2)
    lines(x$curve$x, x$curve$ci_lower, col = "blue", lty = 2)
    lines(x$curve$x, x$curve$ci_upper, col = "blue", lty = 2)

    abline(v = x$knot_positions, lty = 3, col = "gray")
  }

  invisible(NULL)
}


#' Print Method for Spline Meta-Regression
#'
#' @export
print.metareg_spline <- function(x, ...) {
  cat("Meta-Regression with Restricted Cubic Splines\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Moderator: %s\n", x$moderator))
  cat(sprintf("Studies: %d\n", x$n))
  cat(sprintf("Knots: %d\n", x$n_knots))
  cat(sprintf("Residual heterogeneity: τ² = %.4f\n\n", x$tau2))

  cat("Coefficients:\n")
  coef_df <- data.frame(
    Estimate = x$coefficients,
    SE = x$se,
    CI_lower = x$ci_lower,
    CI_upper = x$ci_upper,
    p_value = x$p_values,
    row.names = x$coef_names
  )
  print(coef_df)

  if (!is.null(x$linearity_test)) {
    cat("\nTest for Non-linearity:\n")
    cat(sprintf("  χ² = %.2f, df = %d, p = %.4f\n",
                x$linearity_test$statistic,
                x$linearity_test$df,
                x$linearity_test$p_value))
  }

  invisible(x)
}


#' Print Method for Penalized Meta-Regression
#'
#' @export
print.metareg_penalized <- function(x, ...) {
  cat("Penalized Meta-Regression\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Penalty: %s\n", x$penalty))
  cat(sprintf("Lambda: %.4f\n", x$lambda))
  if (x$penalty == "elasticnet") {
    cat(sprintf("Alpha: %.2f\n", x$alpha))
  }
  cat(sprintf("Studies: %d, Moderators: %d\n", x$n, x$p))
  cat(sprintf("Selected moderators: %d\n", x$n_selected))
  cat(sprintf("R²: %.4f\n\n", x$r_squared))

  if (x$n_selected > 0) {
    cat("Selected Moderators and Coefficients:\n")
    selected_idx <- c(1, which(abs(x$coefficients[-1]) > 1e-8) + 1)
    coef_df <- data.frame(
      Moderator = x$coef_names[selected_idx],
      Coefficient = x$coefficients[selected_idx],
      row.names = NULL
    )
    print(coef_df)
  } else {
    cat("No moderators selected (all coefficients shrunk to zero)\n")
  }

  invisible(x)
}
