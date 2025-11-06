test_that("cbamm_fast works with valid input", {
  # Create test data
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  result <- cbamm_fast(data)

  # Check that result has expected structure
  expect_s3_class(result, "cbamm")
  expect_true(!is.null(result$estimate))
  expect_true(!is.null(result$se))
  expect_true(!is.null(result$ci_lower))
  expect_true(!is.null(result$ci_upper))
  expect_true(!is.null(result$tau2))
  expect_true(!is.null(result$I2))
  expect_true(!is.null(result$Q))
  expect_true(!is.null(result$p_value))

  # Check that estimate is numeric
  expect_type(result$estimate, "double")
  expect_type(result$se, "double")

  # Check that confidence interval is properly ordered
  expect_true(result$ci_lower < result$ci_upper)

  # Check that tau2 and I2 are non-negative
  expect_gte(result$tau2, 0)
  expect_gte(result$I2, 0)
})

test_that("cbamm_fast validates input data", {
  # Test with non-data.frame input
  expect_error(cbamm_fast(c(1, 2, 3)), "data must be a data frame")

  # Test with missing columns
  bad_data <- data.frame(study = 1:5, effect = rnorm(5))
  expect_error(cbamm_fast(bad_data), "Missing required columns")

  # Test with too few studies
  small_data <- data.frame(
    study = "Study1",
    effect = 0.5,
    se = 0.1
  )
  expect_error(cbamm_fast(small_data), "Need at least 2 studies")
})

test_that("cbamm_fast handles different methods", {
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  result_dl <- cbamm_fast(data, method = "DL")
  result_reml <- cbamm_fast(data, method = "REML")

  expect_s3_class(result_dl, "cbamm")
  expect_s3_class(result_reml, "cbamm")
  expect_equal(result_dl$method, "DL")
  expect_equal(result_reml$method, "REML")
})

test_that("cbamm_fast handles missing values", {
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, NA, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  expect_warning(result <- cbamm_fast(data), "Removed.*missing values")
  expect_equal(result$k, 4)
})
