#' Test Core Statistics Module
#'
#' Comprehensive tests for core_statistics.R functions that are used
#' across multiple meta-analysis methods.

library(testthat)

context("Core Statistics Module")

# ============================================================================
# Test: pool_effects_random()
# ============================================================================

test_that("pool_effects_random works with DL method", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result <- pool_effects_random(y, v, method = "DL")

  expect_type(result, "list")
  expect_true("estimate" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("tau2" %in% names(result))
  expect_true("I2" %in% names(result))

  # Check estimate is reasonable
  expect_true(result$estimate > 0.2 && result$estimate < 0.8)

  # Check CI bounds
  expect_true(result$ci_lower < result$estimate)
  expect_true(result$ci_upper > result$estimate)

  # Check tau2 is non-negative
  expect_true(result$tau2 >= 0)

  # Check I2 is between 0 and 100
  expect_true(result$I2 >= 0 && result$I2 <= 100)

  # Check weights sum to 1
  expect_equal(sum(result$weights), 1, tolerance = 1e-10)
})

test_that("pool_effects_random works with REML method", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result <- pool_effects_random(y, v, method = "REML")

  expect_type(result, "list")
  expect_true(result$tau2 >= 0)
  expect_equal(result$method, "REML")
})

test_that("pool_effects_random works with PM method", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result <- pool_effects_random(y, v, method = "PM")

  expect_type(result, "list")
  expect_true(result$tau2 >= 0)
  expect_equal(result$method, "PM")
})

test_that("pool_effects_random works with ML method", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result <- pool_effects_random(y, v, method = "ML")

  expect_type(result, "list")
  expect_true(result$tau2 >= 0)
  expect_equal(result$method, "ML")
})

test_that("pool_effects_random handles minimum studies", {
  y <- c(0.5, 0.6)
  v <- c(0.01, 0.02)

  result <- pool_effects_random(y, v, method = "DL")

  expect_type(result, "list")
  expect_equal(result$k, 2)
})

test_that("pool_effects_random fails with too few studies", {
  y <- c(0.5)
  v <- c(0.01)

  expect_error(
    pool_effects_random(y, v, method = "DL"),
    "Need at least 2 studies"
  )
})

test_that("pool_effects_random fails with mismatched lengths", {
  y <- c(0.5, 0.3, 0.7)
  v <- c(0.01, 0.02)

  expect_error(
    pool_effects_random(y, v, method = "DL"),
    "Length of y and v must match"
  )
})

test_that("pool_effects_random respects conf_level", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result_95 <- pool_effects_random(y, v, method = "DL", conf_level = 0.95)
  result_99 <- pool_effects_random(y, v, method = "DL", conf_level = 0.99)

  # 99% CI should be wider than 95% CI
  width_95 <- result_95$ci_upper - result_95$ci_lower
  width_99 <- result_99$ci_upper - result_99$ci_lower

  expect_true(width_99 > width_95)
})


# ============================================================================
# Test: estimate_tau2()
# ============================================================================

test_that("estimate_tau2 returns non-negative values", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  methods <- c("DL", "REML", "PM", "ML")

  for (method in methods) {
    tau2 <- estimate_tau2(y, v, method)
    expect_true(tau2 >= 0, info = paste("Method:", method))
  }
})

test_that("estimate_tau2 accepts pre-computed Q", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  wi <- 1 / v
  theta <- sum(wi * y) / sum(wi)
  Q <- sum(wi * (y - theta)^2)
  df <- length(y) - 1

  tau2_with_Q <- estimate_tau2(y, v, method = "DL", Q = Q, df = df)
  tau2_without_Q <- estimate_tau2(y, v, method = "DL")

  expect_equal(tau2_with_Q, tau2_without_Q, tolerance = 1e-6)
})

test_that("estimate_tau2 fails with unknown method", {
  y <- c(0.5, 0.3, 0.7)
  v <- c(0.01, 0.02, 0.015)

  expect_error(
    estimate_tau2(y, v, method = "UNKNOWN"),
    "Unknown method"
  )
})


# ============================================================================
# Test: calculate_effect_size()
# ============================================================================

test_that("calculate_effect_size works for SMD", {
  result <- calculate_effect_size(
    n1 = 50, n2 = 50,
    mean1 = 10, mean2 = 8,
    sd1 = 2, sd2 = 2,
    type = "SMD"
  )

  expect_type(result, "list")
  expect_true("effect" %in% names(result))
  expect_true("variance" %in% names(result))
  expect_true("se" %in% names(result))

  # Effect should be positive (mean1 > mean2)
  expect_true(result$effect > 0)

  # Hedges' g should be slightly smaller than Cohen's d
  # due to small sample correction
  expect_true(result$effect < 1.1)

  # SE should equal sqrt(variance)
  expect_equal(result$se, sqrt(result$variance), tolerance = 1e-10)
})

test_that("calculate_effect_size works for MD", {
  result <- calculate_effect_size(
    n1 = 50, n2 = 50,
    mean1 = 10, mean2 = 8,
    sd1 = 2, sd2 = 2,
    type = "MD"
  )

  expect_type(result, "list")

  # Mean difference should equal 10 - 8 = 2
  expect_equal(result$effect, 2)

  # Variance should equal sd1^2/n1 + sd2^2/n2
  expected_var <- 2^2/50 + 2^2/50
  expect_equal(result$variance, expected_var, tolerance = 1e-10)
})

test_that("calculate_effect_size handles unequal sample sizes", {
  result <- calculate_effect_size(
    n1 = 30, n2 = 70,
    mean1 = 12, mean2 = 10,
    sd1 = 3, sd2 = 2.5,
    type = "SMD"
  )

  expect_true(result$effect > 0)
  expect_true(result$variance > 0)
})

test_that("calculate_effect_size fails with unknown type", {
  expect_error(
    calculate_effect_size(
      n1 = 50, n2 = 50,
      mean1 = 10, mean2 = 8,
      sd1 = 2, sd2 = 2,
      type = "UNKNOWN"
    ),
    "Unsupported effect size type"
  )
})


# ============================================================================
# Test: calculate_ci()
# ============================================================================

test_that("calculate_ci works with normal method", {
  result <- calculate_ci(estimate = 0.5, se = 0.1, method = "normal")

  expect_type(result, "list")
  expect_true("ci_lower" %in% names(result))
  expect_true("ci_upper" %in% names(result))

  # CI should be centered on estimate
  expect_equal(
    (result$ci_lower + result$ci_upper) / 2,
    0.5,
    tolerance = 1e-10
  )

  # Check approximate width for 95% CI
  expected_width <- 2 * qnorm(0.975) * 0.1
  actual_width <- result$ci_upper - result$ci_lower
  expect_equal(actual_width, expected_width, tolerance = 1e-10)
})

test_that("calculate_ci works with t method", {
  result <- calculate_ci(
    estimate = 0.5,
    se = 0.1,
    method = "t",
    df = 10
  )

  expect_type(result, "list")

  # t-based CI should be wider than normal CI for small df
  result_normal <- calculate_ci(estimate = 0.5, se = 0.1, method = "normal")

  width_t <- result$ci_upper - result$ci_lower
  width_normal <- result_normal$ci_upper - result_normal$ci_lower

  expect_true(width_t > width_normal)
})

test_that("calculate_ci respects conf_level", {
  result_95 <- calculate_ci(estimate = 0.5, se = 0.1, conf_level = 0.95)
  result_99 <- calculate_ci(estimate = 0.5, se = 0.1, conf_level = 0.99)

  width_95 <- result_95$ci_upper - result_95$ci_lower
  width_99 <- result_99$ci_upper - result_99$ci_lower

  expect_true(width_99 > width_95)
})

test_that("calculate_ci fails without df for t method", {
  expect_error(
    calculate_ci(estimate = 0.5, se = 0.1, method = "t"),
    "df required"
  )
})

test_that("calculate_ci fails with unknown method", {
  expect_error(
    calculate_ci(estimate = 0.5, se = 0.1, method = "unknown"),
    "Unknown method"
  )
})


# ============================================================================
# Test: calculate_prediction_interval()
# ============================================================================

test_that("calculate_prediction_interval works correctly", {
  result <- calculate_prediction_interval(
    estimate = 0.5,
    se = 0.1,
    tau = 0.2,
    k = 10
  )

  expect_type(result, "list")
  expect_true("pi_lower" %in% names(result))
  expect_true("pi_upper" %in% names(result))
  expect_true("se_pred" %in% names(result))

  # PI should be centered on estimate
  expect_equal(
    (result$pi_lower + result$pi_upper) / 2,
    0.5,
    tolerance = 1e-10
  )

  # Prediction SE should incorporate tau
  expected_se_pred <- sqrt(0.1^2 + 0.2^2)
  expect_equal(result$se_pred, expected_se_pred, tolerance = 1e-10)
})

test_that("prediction interval wider than confidence interval", {
  # PI accounts for both sampling error and between-study heterogeneity
  # CI only accounts for sampling error

  estimate <- 0.5
  se <- 0.1
  tau <- 0.2
  k <- 10

  pi <- calculate_prediction_interval(estimate, se, tau, k)
  ci <- calculate_ci(estimate, se)

  width_pi <- pi$pi_upper - pi$pi_lower
  width_ci <- ci$ci_upper - ci$ci_lower

  expect_true(width_pi > width_ci)
})

test_that("prediction interval uses t-distribution for small k", {
  # For k <= 30, should use t-distribution
  pi_small <- calculate_prediction_interval(0.5, 0.1, 0.2, k = 5)
  pi_large <- calculate_prediction_interval(0.5, 0.1, 0.2, k = 100)

  # Both should be valid
  expect_true(pi_small$pi_lower < 0.5)
  expect_true(pi_small$pi_upper > 0.5)
  expect_true(pi_large$pi_lower < 0.5)
  expect_true(pi_large$pi_upper > 0.5)
})


# ============================================================================
# Test: calculate_heterogeneity_stats()
# ============================================================================

test_that("calculate_heterogeneity_stats works correctly", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result <- calculate_heterogeneity_stats(y, v)

  expect_type(result, "list")
  expect_true("Q" %in% names(result))
  expect_true("I2" %in% names(result))
  expect_true("H2" %in% names(result))
  expect_true("tau2" %in% names(result))
  expect_true("tau" %in% names(result))

  # Q should be positive
  expect_true(result$Q > 0)

  # I2 should be between 0 and 100
  expect_true(result$I2 >= 0 && result$I2 <= 100)

  # H2 should be >= 1
  expect_true(result$H2 >= 1)

  # tau should equal sqrt(tau2)
  expect_equal(result$tau, sqrt(result$tau2), tolerance = 1e-10)

  # Q_df should equal k - 1
  expect_equal(result$Q_df, length(y) - 1)
})

test_that("calculate_heterogeneity_stats accepts pre-computed tau2", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  result_with_tau2 <- calculate_heterogeneity_stats(y, v, tau2 = 0.05)

  expect_equal(result_with_tau2$tau2, 0.05)
  expect_equal(result_with_tau2$tau, sqrt(0.05), tolerance = 1e-10)
})

test_that("I2 is 0 when Q < df", {
  # Create homogeneous data
  y <- rep(0.5, 5)
  v <- rep(0.01, 5)

  result <- calculate_heterogeneity_stats(y, v)

  # I2 should be 0 or very close to it
  expect_true(result$I2 < 5)
})


# ============================================================================
# Test: validate_meta_data()
# ============================================================================

test_that("validate_meta_data passes with valid data", {
  data <- data.frame(
    study = paste0("S", 1:5),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6),
    se = c(0.1, 0.12, 0.09, 0.11, 0.1)
  )

  expect_true(
    validate_meta_data(
      data,
      required_cols = c("study", "effect", "se"),
      numeric_cols = c("effect", "se"),
      positive_cols = "se"
    )
  )
})

test_that("validate_meta_data fails with missing columns", {
  data <- data.frame(
    study = paste0("S", 1:5),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6)
  )

  expect_error(
    validate_meta_data(data, required_cols = c("study", "effect", "se")),
    "Missing required columns"
  )
})

test_that("validate_meta_data fails with non-numeric columns", {
  data <- data.frame(
    study = paste0("S", 1:5),
    effect = as.character(c(0.5, 0.3, 0.7, 0.4, 0.6)),
    se = c(0.1, 0.12, 0.09, 0.11, 0.1)
  )

  expect_error(
    validate_meta_data(
      data,
      required_cols = c("study", "effect", "se"),
      numeric_cols = c("effect", "se")
    ),
    "must be numeric"
  )
})

test_that("validate_meta_data fails with non-positive values", {
  data <- data.frame(
    study = paste0("S", 1:5),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6),
    se = c(0.1, -0.12, 0.09, 0.11, 0.1)
  )

  expect_error(
    validate_meta_data(
      data,
      required_cols = c("study", "effect", "se"),
      positive_cols = "se"
    ),
    "must contain only positive values"
  )
})

test_that("validate_meta_data warns about missing values", {
  data <- data.frame(
    study = paste0("S", 1:5),
    effect = c(0.5, NA, 0.7, 0.4, 0.6),
    se = c(0.1, 0.12, 0.09, 0.11, 0.1)
  )

  expect_warning(
    validate_meta_data(data, required_cols = c("study", "effect", "se")),
    "rows with missing values"
  )
})

test_that("validate_meta_data fails with non-data.frame", {
  data <- list(effect = c(0.5, 0.3), se = c(0.1, 0.12))

  expect_error(
    validate_meta_data(data, required_cols = c("effect", "se")),
    "data must be a data frame"
  )
})


# ============================================================================
# Test: detect_outliers()
# ============================================================================

test_that("detect_outliers works with IQR method", {
  y <- c(1, 2, 3, 4, 5, 100)  # 100 is clear outlier

  outliers <- detect_outliers(y, method = "iqr", threshold = 1.5)

  expect_type(outliers, "logical")
  expect_equal(length(outliers), length(y))

  # Last value (100) should be detected as outlier
  expect_true(outliers[6])

  # First five should not be outliers
  expect_false(any(outliers[1:5]))
})

test_that("detect_outliers works with zscore method", {
  set.seed(123)
  y <- c(rnorm(20, mean = 0, sd = 1), 10)  # 10 is clear outlier

  outliers <- detect_outliers(y, method = "zscore", threshold = 3)

  expect_type(outliers, "logical")

  # Last value should likely be detected
  expect_true(outliers[21])
})

test_that("detect_outliers threshold affects detection", {
  y <- c(1, 2, 3, 4, 5, 10)

  outliers_strict <- detect_outliers(y, method = "iqr", threshold = 1.0)
  outliers_lenient <- detect_outliers(y, method = "iqr", threshold = 3.0)

  # Stricter threshold should detect more outliers
  expect_true(sum(outliers_strict) >= sum(outliers_lenient))
})

test_that("detect_outliers handles no outliers", {
  y <- seq(1, 10, by = 1)  # Regular sequence, no outliers

  outliers <- detect_outliers(y, method = "iqr", threshold = 1.5)

  # Should detect few or no outliers
  expect_true(sum(outliers) <= 2)
})

test_that("detect_outliers fails with unknown method", {
  y <- c(1, 2, 3, 4, 5)

  expect_error(
    detect_outliers(y, method = "unknown"),
    "Unknown outlier detection method"
  )
})


# ============================================================================
# Integration Tests
# ============================================================================

test_that("pool_effects_random integrates with calculate_heterogeneity_stats", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  pool_result <- pool_effects_random(y, v, method = "DL")
  het_result <- calculate_heterogeneity_stats(y, v, tau2 = pool_result$tau2)

  # tau2 should match
  expect_equal(pool_result$tau2, het_result$tau2)

  # Q and I2 should match
  expect_equal(pool_result$Q, het_result$Q)
  expect_equal(pool_result$I2, het_result$I2)
})

test_that("effect size calculation integrates with pooling", {
  # Calculate effect sizes
  es1 <- calculate_effect_size(50, 50, 10, 8, 2, 2, "SMD")
  es2 <- calculate_effect_size(60, 60, 9, 7, 2.5, 2.5, "SMD")
  es3 <- calculate_effect_size(40, 40, 11, 9, 1.8, 1.8, "SMD")

  y <- c(es1$effect, es2$effect, es3$effect)
  v <- c(es1$variance, es2$variance, es3$variance)

  # Pool effect sizes
  pool_result <- pool_effects_random(y, v, method = "DL")

  expect_true(pool_result$estimate > 0)
  expect_true(pool_result$tau2 >= 0)
})

test_that("CI and PI integration makes sense", {
  y <- c(0.5, 0.3, 0.7, 0.4, 0.6)
  v <- c(0.01, 0.02, 0.015, 0.018, 0.012)

  pool_result <- pool_effects_random(y, v, method = "DL")

  ci <- calculate_ci(pool_result$estimate, pool_result$se)
  pi <- calculate_prediction_interval(
    pool_result$estimate,
    pool_result$se,
    pool_result$tau,
    pool_result$k
  )

  # CI from pool_result should match calculated CI
  expect_equal(pool_result$ci_lower, ci$ci_lower, tolerance = 1e-6)
  expect_equal(pool_result$ci_upper, ci$ci_upper, tolerance = 1e-6)

  # PI should be wider than CI when tau > 0
  if (pool_result$tau > 0) {
    width_ci <- ci$ci_upper - ci$ci_lower
    width_pi <- pi$pi_upper - pi$pi_lower
    expect_true(width_pi > width_ci)
  }
})
