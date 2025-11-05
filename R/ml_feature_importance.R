#' Machine Learning Feature Importance Module
#'
#' Advanced feature importance methods including SHAP-style values for
#' meta-analysis applications. Helps identify which study characteristics
#' and moderators most influence effect sizes.
#'
#' Based on: Lundberg & Lee (2017). A unified approach to interpreting model predictions.
#'
#' @name ml_feature_importance
NULL


#' Calculate SHAP-Style Feature Importance
#'
#' Computes SHAP (SHapley Additive exPlanations) style values for understanding
#' which features contribute most to effect size predictions in meta-analysis.
#'
#' @param model Trained ML model (from ml_predict_effects)
#' @param data Data frame with features
#' @param features Character vector of feature names
#' @param method ML method used: "rf", "xgboost", "svm", "nn"
#' @param n_samples Number of samples for approximation (default: 100)
#' @param baseline_value Baseline for comparison (default: mean of training data)
#'
#' @return List with:
#'   \itemize{
#'     \item shap_values: Matrix of SHAP values (n_obs × n_features)
#'     \item feature_importance: Global feature importance ranking
#'     \item base_value: Baseline prediction value
#'     \item plots: SHAP summary plots
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Train model first
#' ml_result <- ml_predict_effects(data, predictors = c("year", "n", "quality"),
#'                                  method = "rf")
#'
#' # Calculate SHAP values
#' shap_result <- calculate_shap_importance(
#'   ml_result$model,
#'   data = data,
#'   features = c("year", "n", "quality"),
#'   method = "rf"
#' )
#'
#' # Plot
#' plot_shap_summary(shap_result)
#' }
calculate_shap_importance <- function(model, data, features, method,
                                     n_samples = 100, baseline_value = NULL) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   SHAP-STYLE FEATURE IMPORTANCE                              ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  n_obs <- nrow(data)
  n_feat <- length(features)

  cat(sprintf("Computing SHAP values for %d observations\n", n_obs))
  cat(sprintf("Features: %d (%s)\n", n_feat, paste(features, collapse = ", ")))
  cat(sprintf("Method: %s\n", method))
  cat(sprintf("Samples for approximation: %d\n\n", n_samples))

  # Extract feature matrix
  X <- as.matrix(data[, features, drop = FALSE])

  # Determine baseline value
  if (is.null(baseline_value)) {
    baseline_value <- colMeans(X)
  }

  # Initialize SHAP value matrix
  shap_values <- matrix(0, nrow = n_obs, ncol = n_feat)
  colnames(shap_values) <- features

  # Progress indicator
  cat("Computing SHAP values... (this may take a few minutes)\n")

  # For each observation
  for (i in 1:n_obs) {

    if (i %% max(1, floor(n_obs / 10)) == 0) {
      cat(sprintf("  Progress: %d/%d (%.0f%%)\n", i, n_obs, 100 * i / n_obs))
    }

    # Current observation
    x_current <- X[i, ]

    # For each feature
    for (j in 1:n_feat) {

      # Monte Carlo approximation of Shapley value
      marginal_contributions <- numeric(n_samples)

      for (s in 1:n_samples) {

        # Random subset of other features
        other_features <- setdiff(1:n_feat, j)
        n_subset <- sample(0:length(other_features), 1)

        if (n_subset > 0) {
          subset_features <- sample(other_features, n_subset)
        } else {
          subset_features <- integer(0)
        }

        # Prediction with feature j included
        x_with <- baseline_value
        x_with[c(subset_features, j)] <- x_current[c(subset_features, j)]
        pred_with <- predict_ml_model(model, matrix(x_with, nrow = 1), method)

        # Prediction without feature j
        x_without <- baseline_value
        if (length(subset_features) > 0) {
          x_without[subset_features] <- x_current[subset_features]
        }
        pred_without <- predict_ml_model(model, matrix(x_without, nrow = 1), method)

        # Marginal contribution
        marginal_contributions[s] <- pred_with - pred_without
      }

      # SHAP value = average marginal contribution
      shap_values[i, j] <- mean(marginal_contributions)
    }
  }

  cat("\n✓ SHAP values computed\n")

  # Global feature importance (mean absolute SHAP value)
  global_importance <- colMeans(abs(shap_values))
  importance_df <- data.frame(
    feature = features,
    importance = global_importance,
    mean_shap = colMeans(shap_values),
    sd_shap = apply(shap_values, 2, sd)
  )
  importance_df <- importance_df[order(-importance_df$importance), ]

  cat("\nGlobal Feature Importance (Mean |SHAP|):\n")
  print(importance_df, row.names = FALSE, digits = 4)

  # Calculate base prediction (using baseline features)
  base_prediction <- predict_ml_model(model, matrix(baseline_value, nrow = 1), method)

  result <- list(
    shap_values = shap_values,
    global_importance = importance_df,
    base_value = base_prediction,
    baseline_features = baseline_value,
    data = data,
    features = features,
    method = method
  )

  class(result) <- c("shap_importance", "list")

  return(result)
}


#' Predict from ML Model (Internal Helper)
#'
#' @keywords internal
predict_ml_model <- function(model, X, method) {

  if (method == "rf") {
    # Random Forest
    if (requireNamespace("randomForest", quietly = TRUE)) {
      pred <- predict(model, newdata = as.data.frame(X))
    } else {
      stop("randomForest package required")
    }

  } else if (method == "xgboost") {
    # XGBoost
    if (requireNamespace("xgboost", quietly = TRUE)) {
      pred <- predict(model, newdata = xgboost::xgb.DMatrix(X))
    } else {
      stop("xgboost package required")
    }

  } else if (method == "svm") {
    # SVM
    if (requireNamespace("e1071", quietly = TRUE)) {
      pred <- predict(model, newdata = as.data.frame(X))
    } else {
      stop("e1071 package required")
    }

  } else if (method == "nn") {
    # Neural Network
    if (requireNamespace("nnet", quietly = TRUE)) {
      pred <- predict(model, newdata = as.data.frame(X))
    } else {
      stop("nnet package required")
    }

  } else {
    stop("Unknown method: ", method)
  }

  return(as.numeric(pred))
}


#' Permutation Feature Importance
#'
#' Calculate permutation importance by measuring prediction degradation when
#' each feature is randomly shuffled.
#'
#' @param model Trained ML model
#' @param data Data frame with features
#' @param features Feature names
#' @param outcome Outcome variable name
#' @param method ML method
#' @param n_repeats Number of permutation repeats (default: 10)
#'
#' @return Data frame with permutation importance scores
#'
#' @export
calculate_permutation_importance <- function(model, data, features, outcome,
                                            method, n_repeats = 10) {

  cat("Computing permutation feature importance...\n")

  X <- as.matrix(data[, features, drop = FALSE])
  y_true <- data[[outcome]]

  # Baseline prediction error
  y_pred_baseline <- predict_ml_model(model, X, method)
  error_baseline <- mean((y_true - y_pred_baseline)^2)  # MSE

  # Store importance
  importance_scores <- matrix(NA, nrow = length(features), ncol = n_repeats)
  rownames(importance_scores) <- features

  for (i in seq_along(features)) {
    feat <- features[i]

    for (r in 1:n_repeats) {
      # Permute this feature
      X_permuted <- X
      X_permuted[, i] <- sample(X_permuted[, i])

      # Predict with permuted feature
      y_pred_permuted <- predict_ml_model(model, X_permuted, method)
      error_permuted <- mean((y_true - y_pred_permuted)^2)

      # Importance = increase in error
      importance_scores[i, r] <- error_permuted - error_baseline
    }
  }

  # Summarize
  importance_df <- data.frame(
    feature = features,
    importance_mean = rowMeans(importance_scores),
    importance_sd = apply(importance_scores, 1, sd)
  )
  importance_df <- importance_df[order(-importance_df$importance_mean), ]

  cat("\nPermutation Feature Importance:\n")
  print(importance_df, row.names = FALSE, digits = 4)

  return(importance_df)
}


#' Partial Dependence Plot Data
#'
#' Generate data for partial dependence plots showing marginal effect of each feature.
#'
#' @param model Trained ML model
#' @param data Data frame with features
#' @param features Feature names
#' @param method ML method
#' @param n_grid Number of grid points (default: 50)
#'
#' @return List of partial dependence data for each feature
#'
#' @export
calculate_partial_dependence <- function(model, data, features, method, n_grid = 50) {

  cat("Computing partial dependence...\n")

  X <- as.matrix(data[, features, drop = FALSE])

  pdp_list <- list()

  for (feat in features) {
    feat_idx <- which(features == feat)

    # Grid of values for this feature
    feat_values <- X[, feat_idx]
    grid_values <- seq(min(feat_values), max(feat_values), length.out = n_grid)

    # Average predictions at each grid point
    pdp_values <- numeric(n_grid)

    for (i in 1:n_grid) {
      # Set feature to grid value for all observations
      X_modified <- X
      X_modified[, feat_idx] <- grid_values[i]

      # Average prediction
      preds <- predict_ml_model(model, X_modified, method)
      pdp_values[i] <- mean(preds)
    }

    pdp_list[[feat]] <- data.frame(
      feature_value = grid_values,
      partial_dependence = pdp_values
    )
  }

  cat(sprintf("✓ Partial dependence computed for %d features\n", length(features)))

  return(pdp_list)
}


#' Plot SHAP Summary
#'
#' Create summary plot of SHAP values showing feature importance and impact.
#'
#' @param shap_result Result from calculate_shap_importance
#' @param top_n Number of top features to show (default: all)
#'
#' @export
plot_shap_summary <- function(shap_result, top_n = NULL) {

  if (!inherits(shap_result, "shap_importance")) {
    stop("Input must be a shap_importance object")
  }

  shap_values <- shap_result$shap_values
  features <- shap_result$features

  if (is.null(top_n)) {
    top_n <- length(features)
  }

  # Order features by importance
  importance_order <- shap_result$global_importance$feature[1:min(top_n, length(features))]

  # SHAP summary plot (beeswarm-style)
  par(mar = c(5, 8, 4, 2))

  plot(NULL, xlim = range(shap_values), ylim = c(0.5, top_n + 0.5),
       xlab = "SHAP Value (impact on model output)",
       ylab = "",
       main = "SHAP Summary Plot",
       yaxt = "n")

  for (i in 1:top_n) {
    feat <- importance_order[i]
    feat_idx <- which(features == feat)
    shap_vals <- shap_values[, feat_idx]

    # Add jitter to y position
    y_pos <- rep(top_n - i + 1, length(shap_vals))
    y_jitter <- y_pos + runif(length(shap_vals), -0.3, 0.3)

    # Color by feature value (normalized)
    feat_values <- shap_result$data[[feat]]
    feat_norm <- (feat_values - min(feat_values)) / (max(feat_values) - min(feat_values))
    colors <- rgb(feat_norm, 0, 1 - feat_norm, alpha = 0.6)

    points(shap_vals, y_jitter, pch = 19, col = colors, cex = 0.8)
  }

  axis(2, at = 1:top_n, labels = rev(importance_order), las = 1)
  abline(v = 0, lty = 2, col = "gray50")

  # Add color legend
  legend("topright", legend = c("High", "Low"), pch = 19,
         col = c(rgb(1, 0, 0, 0.6), rgb(0, 0, 1, 0.6)),
         title = "Feature Value", bty = "n")

  invisible(NULL)
}


#' Plot Feature Importance Comparison
#'
#' Compare multiple feature importance methods.
#'
#' @param shap_importance SHAP importance result
#' @param permutation_importance Permutation importance result
#'
#' @export
plot_importance_comparison <- function(shap_importance, permutation_importance) {

  # Merge importance scores
  shap_df <- shap_importance$global_importance[, c("feature", "importance")]
  names(shap_df)[2] <- "SHAP"

  perm_df <- permutation_importance[, c("feature", "importance_mean")]
  names(perm_df)[2] <- "Permutation"

  merged <- merge(shap_df, perm_df, by = "feature")

  # Normalize to 0-1 scale
  merged$SHAP_norm <- merged$SHAP / max(merged$SHAP)
  merged$Permutation_norm <- merged$Permutation / max(merged$Permutation, na.rm = TRUE)

  # Sort by SHAP importance
  merged <- merged[order(-merged$SHAP_norm), ]

  # Plot
  n_feat <- nrow(merged)
  par(mar = c(5, 8, 4, 2))

  barplot(t(as.matrix(merged[, c("SHAP_norm", "Permutation_norm")])),
          beside = TRUE,
          horiz = TRUE,
          names.arg = merged$feature,
          las = 1,
          col = c("steelblue", "orange"),
          xlab = "Normalized Importance",
          main = "Feature Importance Comparison",
          xlim = c(0, 1.2))

  legend("topright", legend = c("SHAP", "Permutation"),
         fill = c("steelblue", "orange"), bty = "n")

  invisible(NULL)
}


#' Print Method for SHAP Importance
#'
#' @export
print.shap_importance <- function(x, ...) {
  cat("SHAP Feature Importance Analysis\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Features: %d\n", length(x$features)))
  cat(sprintf("Observations: %d\n\n", nrow(x$shap_values)))

  cat("Global Feature Importance Ranking:\n")
  print(x$global_importance, row.names = FALSE, digits = 4)

  cat(sprintf("\nBase prediction value: %.4f\n", x$base_value))

  invisible(x)
}
