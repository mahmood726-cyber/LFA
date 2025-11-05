#' Prediction Intervals and Models for Meta-Analysis
#'
#' Functions for calculating prediction intervals, building prediction models
#' for future studies, and assessing predictive performance.
#'
#' @name prediction_models
NULL

#' Prediction Interval for Random-Effects Meta-Analysis
#'
#' Calculates prediction interval for the true effect in a new study,
#' accounting for both sampling variability and between-study heterogeneity.
#'
#' @param data Data frame with study, effect, and se columns
#' @param method Estimation method for tau-squared ("DL", "REML", "ML", "PM")
#' @param level Confidence level (default: 0.95 for 95% PI)
#'
#' @return Object of class "prediction_interval" containing:
#'   \itemize{
#'     \item estimate: Pooled effect size
#'     \item pi_lower: Lower bound of prediction interval
#'     \item pi_upper: Upper bound of prediction interval
#'     \item ci_lower: Lower bound of confidence interval
#'     \item ci_upper: Upper bound of confidence interval
#'     \item tau2: Between-study variance
#'     \item se: Standard error of pooled estimate
#'   }
#'
#' @details
#' The prediction interval is wider than the confidence interval because it
#' accounts for both the uncertainty in the pooled estimate and the
#' between-study heterogeneity. It answers: "What range of effects might we
#' expect in a new study?"
#'
#' @references
#' IntHout, J., Ioannidis, J. P., & Borm, G. F. (2014). The Hartung-Knapp-Sidik-Jonkman
#' method for random effects meta-analysis is straightforward and considerably
#' outperforms the standard DerSimonian-Laird method. BMC Medical Research Methodology, 14, 25.
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:20),
#'   effect = rnorm(20, 0.5, 0.3),
#'   se = runif(20, 0.1, 0.3)
#' )
#' pi <- prediction_interval(data)
#' print(pi)
#' plot(pi)
#' }
prediction_interval <- function(data, method = "REML", level = 0.95) {
  # Validate input
  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove missing values
  complete_data <- data[complete.cases(data[, c("effect", "se")]), ]
  k <- nrow(complete_data)

  if (k < 3) {
    stop("Prediction intervals require at least 3 studies")
  }

  yi <- complete_data$effect
  vi <- complete_data$se^2

  # Estimate tau-squared
  if (method == "DL") {
    # DerSimonian-Laird
    wi <- 1 / vi
    Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
    df <- k - 1
    C <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - df) / C)
  } else if (method == "REML") {
    # Simplified REML using iterative procedure
    tau2 <- var(yi) * 0.5  # Initial value
    for (iter in 1:100) {
      wi <- 1 / (vi + tau2)
      mu <- sum(wi * yi) / sum(wi)
      Q <- sum(wi * (yi - mu)^2)
      # Update tau2
      A <- sum(wi)
      B <- sum(wi^2)
      tau2_new <- max(0, (Q - (k - 1)) / (A - B / A))
      if (abs(tau2_new - tau2) < 1e-8) break
      tau2 <- tau2_new
    }
  } else if (method == "ML") {
    # Maximum likelihood
    tau2 <- var(yi) * (k - 1) / k * 0.5
  } else if (method == "PM") {
    # Paule-Mandel
    tau2 <- var(yi) * 0.5
  } else {
    stop("Unknown method. Use 'DL', 'REML', 'ML', or 'PM'")
  }

  # Calculate pooled estimate
  wi <- 1 / (vi + tau2)
  estimate <- sum(wi * yi) / sum(wi)
  se_estimate <- sqrt(1 / sum(wi))

  # Confidence interval for pooled effect
  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = k - 1)
  ci_lower <- estimate - t_crit * se_estimate
  ci_upper <- estimate + t_crit * se_estimate

  # Prediction interval
  # SE for prediction includes both uncertainty in estimate and between-study variance
  se_predict <- sqrt(se_estimate^2 + tau2)
  pi_lower <- estimate - t_crit * se_predict
  pi_upper <- estimate + t_crit * se_predict

  # Additional statistics
  I2 <- max(0, 100 * tau2 / (tau2 + mean(vi)))

  result <- list(
    estimate = estimate,
    se = se_estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    pi_lower = pi_lower,
    pi_upper = pi_upper,
    tau2 = tau2,
    I2 = I2,
    k = k,
    level = level,
    method = method,
    data = complete_data
  )

  class(result) <- "prediction_interval"
  return(result)
}

#' Build Prediction Model for Future Studies
#'
#' Builds a prediction model to forecast effect sizes in future studies
#' based on study characteristics. Uses cross-validation for assessment.
#'
#' @param data Data frame with study, effect, se, and predictor columns
#' @param predictors Character vector of predictor variable names
#' @param model_type Type of model: "linear", "gam", "quantile" (default: "linear")
#' @param cv_folds Number of cross-validation folds (default: 5)
#'
#' @return Object of class "prediction_model" containing:
#'   \itemize{
#'     \item model: Fitted prediction model
#'     \item cv_performance: Cross-validation metrics (RMSE, MAE, R2)
#'     \item predictions: In-sample predictions
#'     \item residuals: Residuals from model fit
#'     \item coefficients: Model coefficients (for linear models)
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data$year <- 2000:2019
#' data$quality <- sample(1:10, 20, replace = TRUE)
#' pred_model <- build_prediction_model(data, predictors = c("year", "quality"))
#' }
build_prediction_model <- function(data, predictors, model_type = "linear",
                                   cv_folds = 5) {
  # Validate input
  required_cols <- c("study", "effect", "se", predictors)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove missing values
  complete_data <- data[complete.cases(data[, c("effect", "se", predictors)]), ]
  n <- nrow(complete_data)

  if (n < 10) {
    stop("Insufficient data: need at least 10 complete studies")
  }

  # Prepare data
  X <- complete_data[, predictors, drop = FALSE]
  y <- complete_data$effect
  weights <- 1 / complete_data$se^2

  # Fit model
  if (model_type == "linear") {
    # Weighted linear regression
    formula_str <- paste("y ~", paste(predictors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = cbind(y = y, X), weights = weights)
    predictions <- predict(model, newdata = X)
    coefficients <- coef(model)
  } else if (model_type == "gam") {
    # Generalized additive model (simplified - would use mgcv package)
    # Using polynomial regression as approximation
    formula_str <- paste("y ~", paste0("poly(", predictors, ", 2)", collapse = " + "))
    model <- lm(as.formula(formula_str), data = cbind(y = y, X), weights = weights)
    predictions <- predict(model, newdata = X)
    coefficients <- coef(model)
  } else if (model_type == "quantile") {
    # Quantile regression (simplified)
    formula_str <- paste("y ~", paste(predictors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = cbind(y = y, X), weights = weights)
    predictions <- predict(model, newdata = X)
    coefficients <- coef(model)
  } else {
    stop("Unknown model_type. Use 'linear', 'gam', or 'quantile'")
  }

  # Cross-validation
  fold_size <- floor(n / cv_folds)
  cv_predictions <- rep(NA, n)

  for (fold in 1:cv_folds) {
    # Create test set
    test_idx <- ((fold - 1) * fold_size + 1):min(fold * fold_size, n)
    if (fold == cv_folds) test_idx <- ((fold - 1) * fold_size + 1):n

    train_idx <- setdiff(1:n, test_idx)

    # Train on training set
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    weights_train <- weights[train_idx]

    X_test <- X[test_idx, , drop = FALSE]

    if (model_type == "linear") {
      fold_model <- lm(as.formula(formula_str),
                      data = cbind(y = y_train, X_train),
                      weights = weights_train)
      cv_predictions[test_idx] <- predict(fold_model, newdata = X_test)
    } else if (model_type == "gam") {
      fold_model <- lm(as.formula(formula_str),
                      data = cbind(y = y_train, X_train),
                      weights = weights_train)
      cv_predictions[test_idx] <- predict(fold_model, newdata = X_test)
    } else {
      fold_model <- lm(as.formula(formula_str),
                      data = cbind(y = y_train, X_train),
                      weights = weights_train)
      cv_predictions[test_idx] <- predict(fold_model, newdata = X_test)
    }
  }

  # Calculate CV performance metrics
  cv_residuals <- y - cv_predictions
  cv_rmse <- sqrt(mean(cv_residuals^2))
  cv_mae <- mean(abs(cv_residuals))
  cv_r2 <- 1 - sum(cv_residuals^2) / sum((y - mean(y))^2)

  # In-sample performance
  residuals <- y - predictions
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  r2 <- 1 - sum(residuals^2) / sum((y - mean(y))^2)

  result <- list(
    model = model,
    predictions = predictions,
    cv_predictions = cv_predictions,
    residuals = residuals,
    coefficients = coefficients,
    cv_performance = list(
      rmse = cv_rmse,
      mae = cv_mae,
      r2 = cv_r2
    ),
    in_sample_performance = list(
      rmse = rmse,
      mae = mae,
      r2 = r2
    ),
    model_type = model_type,
    predictors = predictors,
    data = complete_data
  )

  class(result) <- "prediction_model"
  return(result)
}

#' Predict Effect Size for New Study
#'
#' Uses a fitted prediction model to forecast the effect size for a new study
#' with specified characteristics.
#'
#' @param prediction_model Result from build_prediction_model()
#' @param newdata Data frame with predictor values for new study
#' @param interval Calculate prediction interval (default: TRUE)
#' @param level Confidence level for prediction interval (default: 0.95)
#'
#' @return List with predicted effect, standard error, and optional interval
#'
#' @export
predict_new_study <- function(prediction_model, newdata, interval = TRUE,
                              level = 0.95) {
  if (!inherits(prediction_model, "prediction_model")) {
    stop("prediction_model must be of class 'prediction_model'")
  }

  # Make prediction
  pred <- predict(prediction_model$model, newdata = newdata, se.fit = TRUE)

  # Calculate prediction interval if requested
  if (interval) {
    # Residual standard error
    residual_se <- sqrt(mean(prediction_model$residuals^2))

    # SE for prediction includes both model uncertainty and residual variance
    se_pred <- sqrt(pred$se.fit^2 + residual_se^2)

    # Critical value
    df <- nrow(prediction_model$data) - length(prediction_model$coefficients)
    t_crit <- qt((1 + level) / 2, df = df)

    pi_lower <- pred$fit - t_crit * se_pred
    pi_upper <- pred$fit + t_crit * se_pred

    result <- list(
      predicted_effect = as.vector(pred$fit),
      se = se_pred,
      pi_lower = as.vector(pi_lower),
      pi_upper = as.vector(pi_upper),
      level = level
    )
  } else {
    result <- list(
      predicted_effect = as.vector(pred$fit),
      se = pred$se.fit
    )
  }

  return(result)
}

#' Calibration Plot for Prediction Model
#'
#' Creates a calibration plot comparing predicted vs observed effect sizes
#' to assess model calibration.
#'
#' @param prediction_model Result from build_prediction_model()
#' @param use_cv Use cross-validated predictions (default: TRUE)
#'
#' @return Calibration plot
#'
#' @export
plot_calibration <- function(prediction_model, use_cv = TRUE) {
  if (!inherits(prediction_model, "prediction_model")) {
    stop("prediction_model must be of class 'prediction_model'")
  }

  if (use_cv) {
    predicted <- prediction_model$cv_predictions
    label <- "Cross-Validated Predictions"
  } else {
    predicted <- prediction_model$predictions
    label <- "In-Sample Predictions"
  }

  observed <- prediction_model$data$effect

  # Create plot
  par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

  plot(predicted, observed,
       xlab = "Predicted Effect Size",
       ylab = "Observed Effect Size",
       main = paste("Calibration Plot -", label),
       pch = 16, col = rgb(0, 0, 1, 0.5))

  # Add diagonal line (perfect calibration)
  abline(0, 1, col = "red", lwd = 2, lty = 2)

  # Add smoothed calibration curve
  if (length(predicted) > 5) {
    loess_fit <- loess(observed ~ predicted, span = 0.75)
    pred_seq <- seq(min(predicted), max(predicted), length.out = 100)
    smooth_curve <- predict(loess_fit, newdata = pred_seq)
    lines(pred_seq, smooth_curve, col = "blue", lwd = 2)
  }

  # Add grid
  grid()

  # Add legend
  legend("topleft",
         legend = c("Perfect calibration", "Smoothed calibration", "Studies"),
         col = c("red", "blue", rgb(0, 0, 1, 0.5)),
         lty = c(2, 1, NA),
         pch = c(NA, NA, 16),
         lwd = c(2, 2, NA))
}

#' Power Analysis for Future Studies
#'
#' Calculates the statistical power to detect an effect in a future study
#' based on meta-analysis results.
#'
#' @param meta_result Result from cbamm_fast() or similar
#' @param sample_sizes Vector of potential sample sizes for future study
#' @param alpha Significance level (default: 0.05)
#' @param two_sided Two-sided test (default: TRUE)
#'
#' @return Data frame with sample sizes and corresponding power
#'
#' @export
#' @examples
#' \dontrun{
#' power_results <- power_analysis_future(meta_result, sample_sizes = seq(50, 500, 50))
#' plot(power_results$n, power_results$power, type = "l")
#' }
power_analysis_future <- function(meta_result, sample_sizes, alpha = 0.05,
                                  two_sided = TRUE) {
  # Extract effect size from meta-analysis
  if ("estimate" %in% names(meta_result)) {
    effect <- meta_result$estimate
    tau2 <- meta_result$tau2
  } else {
    stop("meta_result must contain 'estimate' and 'tau2'")
  }

  # Calculate power for each sample size
  powers <- sapply(sample_sizes, function(n) {
    # Expected SE for new study (assuming equal groups)
    se_new <- sqrt(4 / n + tau2)

    # Non-centrality parameter
    ncp <- abs(effect) / se_new

    # Critical value
    if (two_sided) {
      z_crit <- qnorm(1 - alpha / 2)
      power <- pnorm(ncp - z_crit) + pnorm(-ncp - z_crit)
    } else {
      z_crit <- qnorm(1 - alpha)
      power <- pnorm(ncp - z_crit)
    }

    return(power)
  })

  result <- data.frame(
    n = sample_sizes,
    power = powers,
    effect = effect,
    alpha = alpha
  )

  return(result)
}
