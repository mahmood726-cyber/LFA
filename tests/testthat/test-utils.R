test_that("calculate_i2 works correctly", {
  # I2 should be 0 when Q <= df
  expect_equal(calculate_i2(Q = 5, df = 10), 0)

  # I2 should be positive when Q > df
  result <- calculate_i2(Q = 20, df = 10)
  expect_gt(result, 0)
  expect_lte(result, 100)

  # I2 = 50% when Q = 2*df
  expect_equal(calculate_i2(Q = 20, df = 10), 50)
})

test_that("safe_divide works correctly", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(c(10, 20), c(2, 4)), c(5, 5))

  # Division by zero returns NA by default
  expect_true(is.na(safe_divide(10, 0)))

  # Can specify custom default
  expect_equal(safe_divide(10, 0, default = 0), 0)
})

test_that("se_to_var and var_to_se are inverses", {
  se <- 0.15
  var <- se_to_var(se)
  expect_equal(var, se^2)

  se_back <- var_to_se(var)
  expect_equal(se_back, se)
})

test_that("format_ci works correctly", {
  result <- format_ci(0.25, 0.75, digits = 2)
  expect_equal(result, "(0.25, 0.75)")

  result <- format_ci(0.123, 0.789, digits = 3)
  expect_equal(result, "(0.123, 0.789)")
})

test_that("format_p works correctly", {
  # Regular p-value
  expect_match(format_p(0.045), "^0\\.045$")

  # Very small p-value
  expect_match(format_p(0.0001), "^< 0\\.001$")

  # Can change threshold
  expect_match(format_p(0.005, threshold = 0.01), "^< 0\\.010$")
})

test_that("prediction_interval works correctly", {
  result <- prediction_interval(estimate = 0.5, se = 0.1, tau2 = 0.04, k = 10)

  expect_type(result, "list")
  expect_true(!is.null(result$lower))
  expect_true(!is.null(result$upper))
  expect_true(result$lower < result$upper)
  expect_equal(result$df, 8)  # k - 2
})

test_that("detect_outliers_iqr works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55, 5.0, 0.45, 0.65, 0.52),
    se = rep(0.1, 10)
  )

  outliers <- detect_outliers_iqr(data)

  expect_type(outliers, "logical")
  expect_equal(length(outliers), nrow(data))
  expect_true(outliers[7])  # Study 7 with effect = 5.0 should be outlier
})
