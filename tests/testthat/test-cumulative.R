test_that("cumulative_meta_analysis works correctly", {
  data <- data.frame(
    study = paste0("Study", 1:6),
    effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55),
    se = c(0.1, 0.15, 0.12, 0.18, 0.11, 0.14),
    year = 2015:2020
  )

  result <- cumulative_meta_analysis(data, order_by = "year")

  expect_s3_class(result, "cbamm_cumulative")
  expect_true(!is.null(result$cumulative_results))
  expect_true(!is.null(result$final_estimate))
  expect_equal(nrow(result$cumulative_results), 5)  # k-1 rows
  expect_equal(result$n_studies, 6)
})

test_that("cumulative_meta_analysis detects stability", {
  # Create data where estimates stabilize
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.48, 0.51, 0.49, 0.50, 0.505, 0.502, 0.501, 0.500, 0.501),
    se = rep(0.1, 10),
    year = 2011:2020
  )

  result <- cumulative_meta_analysis(data, order_by = "year",
                                      stability_threshold = 0.05)

  expect_true(result$is_stable)
  expect_true(!is.na(result$stability_index))
})

test_that("cumulative_meta_analysis validates input", {
  # Too few studies
  small_data <- data.frame(
    study = "Study1",
    effect = 0.5,
    se = 0.1
  )

  expect_error(cumulative_meta_analysis(small_data), "Need at least 2 studies")

  # Missing order_by column
  data <- data.frame(
    study = paste0("Study", 1:5),
    effect = rnorm(5),
    se = runif(5, 0.1, 0.3)
  )

  expect_error(cumulative_meta_analysis(data, order_by = "year"),
               "Column.*not found")
})
