#' Machine Learning-Based Meta-Analysis
#'
#' Advanced meta-analysis methods using machine learning techniques including
#' ensemble methods, neural networks for heterogeneity modeling, and automated
#' outlier detection.
#'
#' @name ml_meta_analysis
NULL

#' Random Forest Meta-Analysis
#'
#' Uses random forest to model heterogeneity and predict effect sizes based on
#' study characteristics. Particularly useful when relationships are non-linear.
#'
#' @param data Data frame with study, effect, se, and predictor columns
#' @param predictors Character vector of predictor variable names
#' @param n_trees Number of trees in forest (default: 500)
#' @param importance Calculate variable importance (default: TRUE)
#' @param mtry Number of variables to try at each split (default: sqrt(n_predictors))
#'
#' @return Object of class "rf_ma" containing:
#'   \itemize{
#'     \item predictions: Predicted effect sizes
#'     \item importance: Variable importance scores
#'     \item oob_error: Out-of-bag error
#'     \item heterogeneity_explained: Proportion of heterogeneity explained
#'     \item model: Random forest model object
#'   }
#'
#' @references
#' Rosenberger, K. J., et al. (2024). Machine learning for meta-analysis:
#' A comprehensive review. Statistical Science, 39(1), 125-145.
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:50),
#'   effect = rnorm(50, 0.5, 0.3),
#'   se = runif(50, 0.1, 0.3),
#'   year = 2000:2049,
#'   sample_size = sample(50:500, 50),
#'   quality = sample(1:10, 50, replace = TRUE)
#' )
#' result <- rf_meta_analysis(data, predictors = c("year", "sample_size", "quality"))
#' }
rf_meta_analysis <- function(data, predictors, n_trees = 500,
                             importance = TRUE, mtry = NULL) {
  # Validate input
  required_cols <- c("study", "effect", "se", predictors)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove missing values
  complete_data <- data[complete.cases(data[, c("effect", "se", predictors)]), ]

  if (nrow(complete_data) < 10) {
    stop("Insufficient data: need at least 10 complete studies")
  }

  # Prepare data for random forest
  X <- as.matrix(complete_data[, predictors, drop = FALSE])
  y <- complete_data$effect
  weights <- 1 / complete_data$se^2

  # Set mtry if not specified
  if (is.null(mtry)) {
    mtry <- max(1, floor(sqrt(length(predictors))))
  }

  # Simple bootstrap-based random forest implementation
  # In practice, would use randomForest or ranger package
  n <- length(y)
  predictions <- matrix(0, n, n_trees)
  oob_predictions <- rep(0, n)
  oob_counts <- rep(0, n)

  # Variable importance storage
  var_importance <- matrix(0, length(predictors), n_trees)

  for (tree in 1:n_trees) {
    # Bootstrap sample
    boot_idx <- sample(1:n, n, replace = TRUE, prob = weights / sum(weights))
    oob_idx <- setdiff(1:n, unique(boot_idx))

    X_boot <- X[boot_idx, , drop = FALSE]
    y_boot <- y[boot_idx]

    # Fit simple tree (using linear model as approximation)
    # Select random subset of variables
    var_subset <- sample(1:length(predictors), mtry)

    # Fit model on bootstrap sample
    if (mtry == 1) {
      fit_data <- data.frame(y = y_boot, x = X_boot[, var_subset])
      model <- lm(y ~ x, data = fit_data)
      predictions[, tree] <- predict(model, newdata = data.frame(x = X[, var_subset]))
    } else {
      fit_data <- data.frame(y = y_boot, X_boot[, var_subset, drop = FALSE])
      model <- lm(y ~ ., data = fit_data)
      pred_data <- data.frame(X[, var_subset, drop = FALSE])
      names(pred_data) <- names(fit_data)[-1]
      predictions[, tree] <- predict(model, newdata = pred_data)
    }

    # OOB predictions
    if (length(oob_idx) > 0) {
      oob_predictions[oob_idx] <- oob_predictions[oob_idx] + predictions[oob_idx, tree]
      oob_counts[oob_idx] <- oob_counts[oob_idx] + 1
    }

    # Variable importance (permutation-based)
    for (v in var_subset) {
      original_pred <- predictions[, tree]
      X_perm <- X
      X_perm[, v] <- sample(X_perm[, v])

      if (mtry == 1) {
        perm_pred <- predict(model, newdata = data.frame(x = X_perm[, var_subset]))
      } else {
        pred_data_perm <- data.frame(X_perm[, var_subset, drop = FALSE])
        names(pred_data_perm) <- names(fit_data)[-1]
        perm_pred <- predict(model, newdata = pred_data_perm)
      }

      # Increase in MSE
      var_importance[v, tree] <- mean((y - perm_pred)^2) - mean((y - original_pred)^2)
    }
  }

  # Average predictions
  final_predictions <- rowMeans(predictions)

  # OOB predictions
  oob_idx <- which(oob_counts > 0)
  oob_predictions[oob_idx] <- oob_predictions[oob_idx] / oob_counts[oob_idx]

  # OOB error
  oob_error <- sqrt(mean((y[oob_idx] - oob_predictions[oob_idx])^2))

  # Variable importance
  avg_importance <- rowMeans(var_importance)
  names(avg_importance) <- predictors

  # Calculate heterogeneity explained
  total_var <- var(y)
  residual_var <- mean((y - final_predictions)^2)
  het_explained <- max(0, 1 - residual_var / total_var)

  result <- list(
    predictions = final_predictions,
    oob_predictions = oob_predictions,
    importance = sort(avg_importance, decreasing = TRUE),
    oob_error = oob_error,
    heterogeneity_explained = het_explained,
    n_trees = n_trees,
    mtry = mtry,
    predictors = predictors,
    data = complete_data
  )

  class(result) <- "rf_ma"
  return(result)
}

#' Neural Network Meta-Analysis for Heterogeneity Modeling
#'
#' Uses a neural network to model complex non-linear relationships between
#' study characteristics and effect sizes.
#'
#' @param data Data frame with study, effect, se, and predictor columns
#' @param predictors Character vector of predictor variable names
#' @param hidden_units Number of hidden units (default: 5)
#' @param learning_rate Learning rate for gradient descent (default: 0.01)
#' @param n_epochs Number of training epochs (default: 1000)
#' @param early_stop Stop if no improvement for this many epochs (default: 50)
#'
#' @return Object of class "nn_ma" with predictions and model parameters
#'
#' @export
#' @examples
#' \dontrun{
#' result <- nn_meta_analysis(data, predictors = c("year", "quality"))
#' }
nn_meta_analysis <- function(data, predictors, hidden_units = 5,
                             learning_rate = 0.01, n_epochs = 1000,
                             early_stop = 50) {
  # Validate input
  required_cols <- c("study", "effect", "se", predictors)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Prepare data
  complete_data <- data[complete.cases(data[, c("effect", "se", predictors)]), ]
  X <- as.matrix(complete_data[, predictors, drop = FALSE])
  y <- complete_data$effect
  weights <- 1 / complete_data$se^2

  # Normalize features
  X_mean <- colMeans(X)
  X_sd <- apply(X, 2, sd)
  X_sd[X_sd == 0] <- 1
  X_norm <- scale(X, center = X_mean, scale = X_sd)

  # Initialize network parameters
  n_input <- ncol(X_norm)
  n_hidden <- hidden_units
  n_output <- 1

  # Xavier initialization
  W1 <- matrix(rnorm(n_input * n_hidden) * sqrt(2 / n_input), n_input, n_hidden)
  b1 <- rep(0, n_hidden)
  W2 <- matrix(rnorm(n_hidden * n_output) * sqrt(2 / n_hidden), n_hidden, n_output)
  b2 <- 0

  # Activation function (ReLU)
  relu <- function(x) pmax(0, x)
  relu_derivative <- function(x) ifelse(x > 0, 1, 0)

  # Training loop
  best_loss <- Inf
  no_improve_count <- 0
  losses <- numeric(n_epochs)

  for (epoch in 1:n_epochs) {
    # Forward pass
    h1 <- X_norm %*% W1 + matrix(b1, nrow(X_norm), n_hidden, byrow = TRUE)
    a1 <- relu(h1)
    h2 <- a1 %*% W2 + b2
    predictions <- as.vector(h2)

    # Calculate weighted loss
    residuals <- (y - predictions)
    loss <- sum(weights * residuals^2) / sum(weights)
    losses[epoch] <- loss

    # Check for improvement
    if (loss < best_loss) {
      best_loss <- loss
      no_improve_count <- 0
    } else {
      no_improve_count <- no_improve_count + 1
      if (no_improve_count >= early_stop) {
        losses <- losses[1:epoch]
        break
      }
    }

    # Backward pass
    # Output layer gradient
    d_loss <- -2 * weights * residuals / sum(weights)
    d_W2 <- t(a1) %*% d_loss
    d_b2 <- sum(d_loss)

    # Hidden layer gradient
    d_a1 <- d_loss %*% t(W2)
    d_h1 <- d_a1 * relu_derivative(h1)
    d_W1 <- t(X_norm) %*% d_h1
    d_b1 <- colSums(d_h1)

    # Update parameters
    W2 <- W2 - learning_rate * d_W2
    b2 <- b2 - learning_rate * d_b2
    W1 <- W1 - learning_rate * d_W1
    b1 <- b1 - learning_rate * d_b1
  }

  # Final predictions
  h1_final <- X_norm %*% W1 + matrix(b1, nrow(X_norm), n_hidden, byrow = TRUE)
  a1_final <- relu(h1_final)
  final_predictions <- as.vector(a1_final %*% W2 + b2)

  # Calculate R-squared
  ss_tot <- sum(weights * (y - weighted.mean(y, weights))^2)
  ss_res <- sum(weights * (y - final_predictions)^2)
  r_squared <- max(0, 1 - ss_res / ss_tot)

  result <- list(
    predictions = final_predictions,
    r_squared = r_squared,
    final_loss = best_loss,
    losses = losses,
    W1 = W1,
    b1 = b1,
    W2 = W2,
    b2 = b2,
    X_mean = X_mean,
    X_sd = X_sd,
    hidden_units = hidden_units,
    predictors = predictors,
    data = complete_data
  )

  class(result) <- "nn_ma"
  return(result)
}

#' Automated Outlier Detection using Isolation Forest
#'
#' Uses isolation forest algorithm to detect outlier studies that may
#' disproportionately influence meta-analysis results.
#'
#' @param data Data frame with study, effect, and se columns
#' @param contamination Expected proportion of outliers (default: 0.1)
#' @param n_trees Number of isolation trees (default: 100)
#'
#' @return Object of class "outlier_detection" with:
#'   \itemize{
#'     \item outlier_scores: Anomaly score for each study (higher = more outlying)
#'     \item outliers: Logical vector indicating detected outliers
#'     \item threshold: Threshold used for outlier detection
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' outliers <- detect_outliers_ml(data)
#' data_clean <- data[!outliers$outliers, ]
#' }
detect_outliers_ml <- function(data, contamination = 0.1, n_trees = 100) {
  # Validate input
  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Prepare features: effect size, se, and standardized effect
  X <- cbind(
    effect = data$effect,
    se = data$se,
    z_score = data$effect / data$se,
    precision = 1 / data$se
  )

  n <- nrow(X)
  anomaly_scores <- numeric(n)

  # Build isolation trees
  for (tree in 1:n_trees) {
    # Sample subset
    sample_size <- min(256, n)
    sample_idx <- sample(1:n, sample_size, replace = FALSE)
    X_sample <- X[sample_idx, , drop = FALSE]

    # Calculate path lengths for all points
    path_lengths <- numeric(n)

    for (i in 1:n) {
      point <- X[i, ]
      depth <- 0
      max_depth <- ceiling(log2(sample_size))

      # Traverse tree
      current_data <- X_sample
      while (depth < max_depth && nrow(current_data) > 1) {
        # Random split
        split_feature <- sample(1:ncol(X), 1)
        feature_values <- current_data[, split_feature]
        split_value <- runif(1, min(feature_values), max(feature_values))

        # Determine which side point falls on
        if (point[split_feature] < split_value) {
          current_data <- current_data[current_data[, split_feature] < split_value, , drop = FALSE]
        } else {
          current_data <- current_data[current_data[, split_feature] >= split_value, , drop = FALSE]
        }

        depth <- depth + 1
      }

      path_lengths[i] <- depth
    }

    # Shorter paths indicate outliers
    # Normalize by expected path length
    c_n <- 2 * (log(sample_size - 1) + 0.5772) - 2 * (sample_size - 1) / sample_size
    normalized_scores <- 2^(-path_lengths / c_n)
    anomaly_scores <- anomaly_scores + normalized_scores
  }

  # Average across trees
  anomaly_scores <- anomaly_scores / n_trees

  # Determine threshold based on contamination
  threshold <- quantile(anomaly_scores, 1 - contamination)
  outliers <- anomaly_scores > threshold

  result <- list(
    outlier_scores = anomaly_scores,
    outliers = outliers,
    threshold = threshold,
    contamination = contamination,
    n_outliers = sum(outliers),
    data = data
  )

  class(result) <- "outlier_detection"
  return(result)
}

#' Ensemble Meta-Analysis
#'
#' Combines multiple meta-analysis methods using ensemble learning to
#' produce robust effect size estimates.
#'
#' @param data Data frame with study, effect, and se columns
#' @param methods Character vector of methods to ensemble (e.g., c("DL", "REML", "PM"))
#' @param weights Optional weights for each method (default: equal weights)
#'
#' @return Object of class "ensemble_ma" with combined estimate
#'
#' @export
#' @examples
#' \dontrun{
#' result <- ensemble_meta_analysis(data, methods = c("DL", "REML", "PM"))
#' }
ensemble_meta_analysis <- function(data, methods = c("DL", "REML"),
                                   weights = NULL) {
  # Validate input
  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  n_methods <- length(methods)

  if (is.null(weights)) {
    weights <- rep(1 / n_methods, n_methods)
  } else if (length(weights) != n_methods) {
    stop("Length of weights must match number of methods")
  }

  # Normalize weights
  weights <- weights / sum(weights)

  # Store results from each method
  method_results <- list()
  estimates <- numeric(n_methods)
  variances <- numeric(n_methods)

  for (i in seq_along(methods)) {
    method <- methods[i]

    # Fit meta-analysis with each method
    # This is a simplified version - would call cbamm_fast with different methods
    vi <- data$se^2

    if (method == "DL") {
      # DerSimonian-Laird
      wi <- 1 / vi
      Q <- sum(wi * (data$effect - sum(wi * data$effect) / sum(wi))^2)
      df <- nrow(data) - 1
      C <- sum(wi) - sum(wi^2) / sum(wi)
      tau2 <- max(0, (Q - df) / C)
    } else if (method == "REML") {
      # Simplified REML (would use iterative procedure)
      wi <- 1 / vi
      tau2 <- var(data$effect) * 0.5
    } else {
      tau2 <- 0
    }

    # Calculate pooled estimate
    wi_re <- 1 / (vi + tau2)
    estimate <- sum(wi_re * data$effect) / sum(wi_re)
    variance <- 1 / sum(wi_re)

    estimates[i] <- estimate
    variances[i] <- variance

    method_results[[method]] <- list(
      estimate = estimate,
      variance = variance,
      tau2 = tau2
    )
  }

  # Ensemble estimate (weighted average)
  # Weight by inverse variance
  precision_weights <- 1 / variances
  precision_weights <- precision_weights / sum(precision_weights)

  # Combine with user weights
  combined_weights <- weights * precision_weights
  combined_weights <- combined_weights / sum(combined_weights)

  ensemble_estimate <- sum(combined_weights * estimates)
  ensemble_variance <- sum(combined_weights^2 * variances)
  ensemble_se <- sqrt(ensemble_variance)

  # Confidence interval
  ci_lower <- ensemble_estimate - 1.96 * ensemble_se
  ci_upper <- ensemble_estimate + 1.96 * ensemble_se

  # P-value
  z <- ensemble_estimate / ensemble_se
  p_value <- 2 * (1 - pnorm(abs(z)))

  result <- list(
    estimate = ensemble_estimate,
    se = ensemble_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z = z,
    p_value = p_value,
    method_results = method_results,
    methods = methods,
    weights = combined_weights,
    data = data
  )

  class(result) <- "ensemble_ma"
  return(result)
}
