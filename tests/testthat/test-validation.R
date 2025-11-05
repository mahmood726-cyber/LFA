test_that("validate_cbamm_data works correctly", {
  # Valid data
  valid_data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  result <- validate_cbamm_data(valid_data)
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)

  # Invalid data - not a data frame
  result <- validate_cbamm_data(c(1, 2, 3))
  expect_false(result$valid)
  expect_gt(length(result$errors), 0)

  # Missing required columns
  bad_data <- data.frame(study = 1:5, effect = rnorm(5))
  result <- validate_cbamm_data(bad_data)
  expect_false(result$valid)
  expect_match(result$errors[1], "Missing required columns")

  # Non-positive standard errors
  bad_se <- data.frame(
    study = paste0("Study", 1:3),
    effect = c(0.5, 0.6, 0.4),
    se = c(0.1, 0, 0.12)
  )
  result <- validate_cbamm_data(bad_se)
  expect_false(result$valid)
})

test_that("standardize_cbamm_data works correctly", {
  data <- data.frame(
    studyname = c("A", "B", "C"),
    estimate = c(0.5, 0.6, 0.4),
    stderr = c(0.1, 0.15, 0.12)
  )

  result <- standardize_cbamm_data(data,
                                   effect_col = "estimate",
                                   se_col = "stderr",
                                   study_col = "studyname")

  expect_true("study" %in% names(result))
  expect_true("effect" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("var" %in% names(result))

  # Check var is computed correctly
  expect_equal(result$var, result$se^2)
})

test_that("standardize_cbamm_data adds study names if missing", {
  data <- data.frame(
    effect = c(0.5, 0.6, 0.4),
    se = c(0.1, 0.15, 0.12)
  )

  result <- standardize_cbamm_data(data)

  expect_true("study" %in% names(result))
  expect_equal(nrow(result), 3)
})
