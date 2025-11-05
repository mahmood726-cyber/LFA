#' Test ML Feature Importance Module
#'
#' Comprehensive tests for ml_feature_importance.R including SHAP values,
#' permutation importance, and partial dependence.

library(testthat)

context("ML Feature Importance")

# ============================================================================
# Test Data Setup
# ============================================================================

# Helper function to generate test data and train simple model
generate_ml_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)

  data <- data.frame(
    outcome = rnorm(n, mean = 5, sd = 2),
    feature1 = rnorm(n, mean = 10, sd = 3),
    feature2 = rnorm(n, mean = 20, sd = 5),
    feature3 = runif(n, min = 0, max = 1)
  )

  # Make outcome dependent on features
  data$outcome <- 2 + 0.5 * data$feature1 + 0.3 * data$feature2 +
    2 * data$feature3 + rnorm(n, 0, 1)

  data
}

train_rf_model <- function(data) {
  skip_if_not_installed("randomForest")

  features <- c("feature1", "feature2", "feature3")
  X <- data[, features]
  y <- data$outcome

  model <- randomForest::randomForest(
    x = X,
    y = y,
    ntree = 50
  )

  list(model = model, features = features, data = data)
}


# ============================================================================
# Test: predict_ml_model() Internal Helper
# ============================================================================

test_that("predict_ml_model works with random forest", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 50))
  model <- trained$model
  data <- trained$data

  X_test <- as.matrix(data[1:5, c("feature1", "feature2", "feature3")])

  predictions <- predict_ml_model(model, X_test, method = "rf")

  expect_type(predictions, "double")
  expect_equal(length(predictions), 5)
  expect_true(all(is.finite(predictions)))
})

test_that("predict_ml_model fails with unknown method", {
  data <- generate_ml_test_data(n = 50)
  X_test <- as.matrix(data[1:5, c("feature1", "feature2", "feature3")])

  expect_error(
    predict_ml_model(NULL, X_test, method = "unknown_method"),
    "Unknown method"
  )
})


# ============================================================================
# Test: calculate_shap_importance()
# ============================================================================

test_that("calculate_shap_importance works with random forest", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 50))

  # Use small sample for faster testing
  result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:10, ],
    features = trained$features,
    method = "rf",
    n_samples = 10  # Small for speed
  )

  expect_s3_class(result, "shap_importance")
  expect_type(result, "list")

  # Check SHAP values matrix
  expect_true("shap_values" %in% names(result))
  expect_true(is.matrix(result$shap_values))
  expect_equal(nrow(result$shap_values), 10)
  expect_equal(ncol(result$shap_values), 3)
  expect_equal(colnames(result$shap_values), trained$features)

  # Check global importance
  expect_true("global_importance" %in% names(result))
  expect_true(is.data.frame(result$global_importance))
  expect_equal(nrow(result$global_importance), 3)
  expect_true(all(c("feature", "importance", "mean_shap", "sd_shap") %in%
                    names(result$global_importance)))

  # Check base value
  expect_true("base_value" %in% names(result))
  expect_true(is.numeric(result$base_value))
  expect_equal(length(result$base_value), 1)

  # Check method
  expect_equal(result$method, "rf")
})

test_that("SHAP values have correct properties", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 30))

  result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:5, ],
    features = trained$features,
    method = "rf",
    n_samples = 20
  )

  shap_values <- result$shap_values

  # SHAP values should sum to approximately (prediction - baseline)
  # This is the additivity property
  for (i in 1:nrow(shap_values)) {
    X_current <- as.matrix(trained$data[i, trained$features])
    pred_current <- predict_ml_model(result$model, X_current, "rf")

    shap_sum <- sum(shap_values[i, ])
    diff_from_base <- pred_current - result$base_value

    # Allow tolerance due to Monte Carlo approximation
    expect_equal(shap_sum, diff_from_base, tolerance = 0.5)
  }
})

test_that("calculate_shap_importance global importance ordering makes sense", {
  skip_if_not_installed("randomForest")

  # Generate data where feature3 has largest effect
  set.seed(789)
  data <- data.frame(
    outcome = rnorm(50),
    feature1 = rnorm(50, 10, 1),
    feature2 = rnorm(50, 20, 2),
    feature3 = rnorm(50, 5, 3)
  )

  # Feature3 has strong effect, feature1 weak
  data$outcome <- 2 + 0.1 * data$feature1 + 0.2 * data$feature2 +
    2.0 * data$feature3 + rnorm(50, 0, 0.5)

  features <- c("feature1", "feature2", "feature3")
  model <- randomForest::randomForest(
    x = data[, features],
    y = data$outcome,
    ntree = 50
  )

  result <- calculate_shap_importance(
    model = model,
    data = data[1:10, ],
    features = features,
    method = "rf",
    n_samples = 15
  )

  importance_df <- result$global_importance

  # Feature3 should generally have high importance
  feature3_rank <- which(importance_df$feature == "feature3")
  expect_true(feature3_rank <= 2)  # Should be in top 2
})

test_that("calculate_shap_importance accepts custom baseline", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 40))

  custom_baseline <- c(10, 20, 0.5)
  names(custom_baseline) <- trained$features

  result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:5, ],
    features = trained$features,
    method = "rf",
    n_samples = 10,
    baseline_value = custom_baseline
  )

  expect_equal(result$baseline_features, custom_baseline)
})


# ============================================================================
# Test: calculate_permutation_importance()
# ============================================================================

test_that("calculate_permutation_importance works", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 50))

  result <- calculate_permutation_importance(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 5
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("feature", "importance_mean", "importance_sd") %in% names(result)))

  # Importance should be non-negative (permuting should increase or maintain error)
  expect_true(all(result$importance_mean >= -0.1))  # Small negative values possible due to randomness

  # Features should be ordered by importance
  expect_true(all(diff(result$importance_mean) <= 0))
})

test_that("permutation importance detects important features", {
  skip_if_not_installed("randomForest")

  # Generate data where feature1 has no effect
  set.seed(456)
  data <- data.frame(
    outcome = rnorm(80),
    feature1 = rnorm(80, 10, 2),  # No relationship
    feature2 = rnorm(80, 20, 5),
    feature3 = rnorm(80, 5, 3)
  )

  # Only feature2 and feature3 affect outcome
  data$outcome <- 2 + 0.5 * data$feature2 + 1.0 * data$feature3 + rnorm(80, 0, 0.5)

  features <- c("feature1", "feature2", "feature3")
  model <- randomForest::randomForest(
    x = data[, features],
    y = data$outcome,
    ntree = 50
  )

  result <- calculate_permutation_importance(
    model = model,
    data = data,
    features = features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 10
  )

  # feature1 should have lowest importance
  feature1_importance <- result$importance_mean[result$feature == "feature1"]
  expect_true(feature1_importance < max(result$importance_mean))
})

test_that("permutation importance n_repeats affects stability", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 60))

  result_few <- calculate_permutation_importance(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 3
  )

  result_many <- calculate_permutation_importance(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 15
  )

  # More repeats should generally give lower SD
  mean_sd_few <- mean(result_few$importance_sd)
  mean_sd_many <- mean(result_many$importance_sd)

  expect_true(mean_sd_many <= mean_sd_few * 1.5)  # Allow some tolerance
})


# ============================================================================
# Test: calculate_partial_dependence()
# ============================================================================

test_that("calculate_partial_dependence works", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 50))

  result <- calculate_partial_dependence(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    method = "rf",
    n_grid = 20
  )

  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_equal(names(result), trained$features)

  # Check each feature's PDP
  for (feat in trained$features) {
    pdp <- result[[feat]]

    expect_true(is.data.frame(pdp))
    expect_equal(nrow(pdp), 20)
    expect_true(all(c("feature_value", "partial_dependence") %in% names(pdp)))

    # Feature values should span range
    feat_range <- range(trained$data[[feat]])
    expect_true(min(pdp$feature_value) >= feat_range[1] - 1e-6)
    expect_true(max(pdp$feature_value) <= feat_range[2] + 1e-6)

    # Partial dependence should be finite
    expect_true(all(is.finite(pdp$partial_dependence)))
  }
})

test_that("partial dependence captures monotonic relationships", {
  skip_if_not_installed("randomForest")

  # Generate data with strong monotonic relationship for feature1
  set.seed(321)
  data <- data.frame(
    outcome = rnorm(100),
    feature1 = seq(0, 10, length.out = 100),
    feature2 = rnorm(100, 20, 2)
  )

  # Strong positive linear relationship
  data$outcome <- 5 + 2 * data$feature1 + rnorm(100, 0, 0.5)

  features <- c("feature1", "feature2")
  model <- randomForest::randomForest(
    x = data[, features],
    y = data$outcome,
    ntree = 50
  )

  result <- calculate_partial_dependence(
    model = model,
    data = data,
    features = features,
    method = "rf",
    n_grid = 30
  )

  pdp_feature1 <- result$feature1

  # For feature1, partial dependence should generally increase
  # (allowing for some non-monotonicity due to RF)
  first_quartile <- pdp_feature1$partial_dependence[1:7]
  last_quartile <- pdp_feature1$partial_dependence[24:30]

  expect_true(mean(last_quartile) > mean(first_quartile))
})


# ============================================================================
# Test: plot_shap_summary()
# ============================================================================

test_that("plot_shap_summary works", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 30))

  shap_result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:10, ],
    features = trained$features,
    method = "rf",
    n_samples = 10
  )

  # Should not error
  expect_silent(plot_shap_summary(shap_result))

  # Test with top_n parameter
  expect_silent(plot_shap_summary(shap_result, top_n = 2))
})

test_that("plot_shap_summary fails with wrong input type", {
  expect_error(
    plot_shap_summary(list(some = "data")),
    "Input must be a shap_importance object"
  )
})


# ============================================================================
# Test: plot_importance_comparison()
# ============================================================================

test_that("plot_importance_comparison works", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 40))

  shap_result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:10, ],
    features = trained$features,
    method = "rf",
    n_samples = 10
  )

  perm_result <- calculate_permutation_importance(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 5
  )

  # Should not error
  expect_silent(plot_importance_comparison(shap_result, perm_result))
})


# ============================================================================
# Test: Print Method
# ============================================================================

test_that("print method for shap_importance works", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 30))

  shap_result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:8, ],
    features = trained$features,
    method = "rf",
    n_samples = 10
  )

  expect_output(print(shap_result), "SHAP Feature Importance Analysis")
  expect_output(print(shap_result), "Method: rf")
  expect_output(print(shap_result), "Features: 3")
  expect_output(print(shap_result), "Observations: 8")
  expect_output(print(shap_result), "Global Feature Importance Ranking")
})


# ============================================================================
# Integration Tests
# ============================================================================

test_that("SHAP and permutation importance are correlated", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 60))

  shap_result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:20, ],
    features = trained$features,
    method = "rf",
    n_samples = 20
  )

  perm_result <- calculate_permutation_importance(
    model = trained$model,
    data = trained$data,
    features = trained$features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 10
  )

  # Merge and check correlation
  shap_imp <- shap_result$global_importance
  shap_imp <- shap_imp[order(shap_imp$feature), ]

  perm_imp <- perm_result[order(perm_result$feature), ]

  # Rankings should be somewhat similar
  # (exact match not expected due to different methods)
  shap_order <- order(-shap_imp$importance)
  perm_order <- order(-perm_imp$importance_mean)

  # At least top feature should often match
  # or be within top 2
  top_shap <- shap_order[1]
  top_perm <- perm_order[1]

  expect_true(abs(top_shap - top_perm) <= 1)
})

test_that("feature importance works with XGBoost", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("randomForest")

  data <- generate_ml_test_data(n = 80)
  features <- c("feature1", "feature2", "feature3")

  X_train <- as.matrix(data[1:60, features])
  y_train <- data$outcome[1:60]

  dtrain <- xgboost::xgb.DMatrix(X_train, label = y_train)

  model <- xgboost::xgboost(
    data = dtrain,
    nrounds = 20,
    verbose = 0,
    objective = "reg:squarederror"
  )

  # Test SHAP with XGBoost
  shap_result <- calculate_shap_importance(
    model = model,
    data = data[1:10, ],
    features = features,
    method = "xgboost",
    n_samples = 10
  )

  expect_s3_class(shap_result, "shap_importance")
  expect_equal(shap_result$method, "xgboost")

  # Test permutation importance with XGBoost
  perm_result <- calculate_permutation_importance(
    model = model,
    data = data[1:30, ],
    features = features,
    outcome = "outcome",
    method = "xgboost",
    n_repeats = 5
  )

  expect_true(is.data.frame(perm_result))
  expect_equal(nrow(perm_result), 3)
})

test_that("feature importance methods handle different data types", {
  skip_if_not_installed("randomForest")

  # Data with varying scales
  set.seed(999)
  data <- data.frame(
    outcome = rnorm(50, 100, 20),
    feature1 = rnorm(50, 1000, 300),  # Large scale
    feature2 = rnorm(50, 0.01, 0.005),  # Small scale
    feature3 = runif(50, -5, 5)
  )

  data$outcome <- 50 + 0.05 * data$feature1 + 100 * data$feature2 +
    5 * data$feature3 + rnorm(50, 0, 5)

  features <- c("feature1", "feature2", "feature3")
  model <- randomForest::randomForest(
    x = data[, features],
    y = data$outcome,
    ntree = 50
  )

  # Should handle different scales
  shap_result <- calculate_shap_importance(
    model = model,
    data = data[1:10, ],
    features = features,
    method = "rf",
    n_samples = 10
  )

  expect_s3_class(shap_result, "shap_importance")
  expect_true(all(is.finite(shap_result$shap_values)))

  perm_result <- calculate_permutation_importance(
    model = model,
    data = data,
    features = features,
    outcome = "outcome",
    method = "rf",
    n_repeats = 5
  )

  expect_true(all(is.finite(perm_result$importance_mean)))
})


# ============================================================================
# Edge Cases and Error Handling
# ============================================================================

test_that("feature importance handles small sample sizes", {
  skip_if_not_installed("randomForest")

  trained <- train_rf_model(generate_ml_test_data(n = 20))

  # Small data
  result <- calculate_shap_importance(
    model = trained$model,
    data = trained$data[1:3, ],
    features = trained$features,
    method = "rf",
    n_samples = 5
  )

  expect_s3_class(result, "shap_importance")
  expect_equal(nrow(result$shap_values), 3)
})

test_that("feature importance handles single feature", {
  skip_if_not_installed("randomForest")

  data <- data.frame(
    outcome = rnorm(50),
    feature1 = rnorm(50, 10, 2)
  )
  data$outcome <- 2 + 0.5 * data$feature1 + rnorm(50, 0, 0.5)

  model <- randomForest::randomForest(
    x = data[, "feature1", drop = FALSE],
    y = data$outcome,
    ntree = 30
  )

  result <- calculate_shap_importance(
    model = model,
    data = data[1:10, ],
    features = "feature1",
    method = "rf",
    n_samples = 10
  )

  expect_s3_class(result, "shap_importance")
  expect_equal(ncol(result$shap_values), 1)
  expect_equal(nrow(result$global_importance), 1)
})
