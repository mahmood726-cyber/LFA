test_that("meta_regression works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55, 0.45, 0.65, 0.48, 0.58),
    se = rep(0.1, 10),
    year = 2011:2020,
    quality = c(3, 4, 5, 3, 4, 5, 4, 3, 5, 4)
  )

  result <- meta_regression(data, ~ year)

  expect_s3_class(result, "cbamm_metareg")
  expect_equal(length(result$coefficients), 2)  # Intercept + year
  expect_true(!is.null(result$tau2))
  expect_true(!is.null(result$R2))
})

test_that("meta_regression validates input", {
  data <- data.frame(
    study = paste0("Study", 1:3),
    effect = c(0.5, 0.6, 0.4),
    se = c(0.1, 0.15, 0.12)
  )

  # Not a formula
  expect_error(meta_regression(data, "year"), "must be a formula")

  # Missing effect or se columns
  bad_data <- data.frame(study = 1:5, effect = rnorm(5))
  expect_error(meta_regression(bad_data, ~ 1), "must contain 'effect' and 'se'")
})

test_that("subgroup_analysis works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55, 0.45, 0.65, 0.48, 0.58),
    se = rep(0.1, 10),
    region = rep(c("North", "South"), each = 5)
  )

  result <- subgroup_analysis(data, subgroup = "region")

  expect_s3_class(result, "cbamm_subgroup")
  expect_equal(result$n_subgroups, 2)
  expect_true(!is.null(result$between_test))
})

test_that("robust_rma works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:6),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11, 0.14),
    cluster = c(1, 1, 2, 2, 3, 3)
  )

  result <- robust_rma(data, cluster = "cluster")

  expect_s3_class(result, "cbamm")
  expect_true(result$robust)
  expect_equal(result$n_clusters, 3)
})
