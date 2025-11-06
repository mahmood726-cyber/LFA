test_that("compute_transport_weights works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11),
    age_mean = c(55, 60, 65, 58, 62),
    female_pct = c(0.45, 0.50, 0.55, 0.48, 0.52)
  )

  target_population <- list(
    age_mean = 60,
    female_pct = 0.50
  )

  result <- compute_transport_weights(data, target_population)

  expect_type(result, "list")
  expect_true(!is.null(result$weights))
  expect_true(!is.null(result$normalized_weights))
  expect_equal(length(result$weights), nrow(data))
  expect_equal(sum(result$normalized_weights), 1, tolerance = 1e-10)
  expect_true(all(result$weights > 0))
})

test_that("compute_transport_weights validates input", {
  data <- data.frame(
    study = paste0("Study", 1:3),
    effect = c(0.5, 0.6, 0.4),
    se = c(0.1, 0.15, 0.12)
  )

  # Missing target_population
  expect_error(compute_transport_weights(data, NULL), "must be a named list")

  # Target without names
  expect_error(compute_transport_weights(data, list(60, 0.5)),
               "must have named elements")

  # Covariate not in data
  target <- list(age_mean = 60)
  expect_error(compute_transport_weights(data, target),
               "not found in data")
})

test_that("compute_analysis_weights works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11)
  )

  weights <- compute_analysis_weights(data)

  expect_type(weights, "double")
  expect_equal(length(weights), nrow(data))
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights > 0))
})
