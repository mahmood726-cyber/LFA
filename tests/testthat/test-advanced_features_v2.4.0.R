#' Test Advanced Features from v2.4.0
#'
#' Comprehensive tests for v2.4.0 advanced modules:
#' - Advanced heterogeneity estimators
#' - Component network meta-analysis
#' - Prevalence meta-analysis
#' - Advanced meta-regression (splines, penalized)
#' - Cross-design synthesis

library(testthat)

context("Advanced Features v2.4.0")

# ============================================================================
# Test: Advanced Heterogeneity Estimators
# ============================================================================

test_that("meta_analysis_advanced works with Paule-Mandel estimator", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35, 0.65, 0.5),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14, 0.09, 0.11)
  )

  result <- meta_analysis_advanced(data, method = "PM", test = "z")

  expect_type(result, "list")
  expect_s3_class(result, "cbamm_advanced")

  # Check essential components
  expect_true("estimate" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("tau2" %in% names(result))
  expect_true("method" %in% names(result))

  # Check tau2 is non-negative
  expect_true(result$tau2 >= 0)

  # Check method
  expect_equal(result$method, "PM")
})

test_that("meta_analysis_advanced works with all estimators", {
  data <- data.frame(
    study = paste0("Study", 1:8),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14)
  )

  methods <- c("PM", "SJ", "EB", "ML", "REML", "DL", "HS", "HE")

  for (method in methods) {
    result <- meta_analysis_advanced(data, method = method, test = "z")

    expect_s3_class(result, "cbamm_advanced", info = paste("Method:", method))
    expect_true(result$tau2 >= 0, info = paste("Method:", method))
    expect_equal(result$method, method, info = paste("Method:", method))
  }
})

test_that("meta_analysis_advanced with HKSJ adjustment", {
  data <- data.frame(
    study = paste0("Study", 1:6),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13)
  )

  result_z <- meta_analysis_advanced(data, method = "PM", test = "z")
  result_knha <- meta_analysis_advanced(data, method = "PM", test = "knha")

  # KNHA should give wider confidence intervals for small studies
  width_z <- result_z$ci_upper - result_z$ci_lower
  width_knha <- result_knha$ci_upper - result_knha$ci_lower

  expect_true(width_knha >= width_z)
})

test_that("meta_analysis_advanced with prediction interval", {
  data <- data.frame(
    study = paste0("Study", 1:8),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14)
  )

  result <- meta_analysis_advanced(
    data,
    method = "PM",
    prediction_interval = TRUE
  )

  expect_true("pi_lower" %in% names(result))
  expect_true("pi_upper" %in% names(result))

  # PI should be wider than CI when tau2 > 0
  if (result$tau2 > 0) {
    width_ci <- result$ci_upper - result$ci_lower
    width_pi <- result$pi_upper - result$pi_lower
    expect_true(width_pi > width_ci)
  }
})

test_that("meta_analysis_advanced method comparison", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35, 0.65, 0.5),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14, 0.09, 0.11)
  )

  result <- meta_analysis_advanced(
    data,
    method = "PM",
    compare_methods = TRUE
  )

  expect_true("method_comparison" %in% names(result))
  expect_true(is.data.frame(result$method_comparison))

  # Should compare multiple methods
  expect_true(nrow(result$method_comparison) >= 4)
  expect_true(all(c("method", "estimate", "tau2", "I2") %in%
                    names(result$method_comparison)))
})


# ============================================================================
# Test: Component Network Meta-Analysis
# ============================================================================

test_that("component_nma works with additive model", {
  skip_if_not_installed("netmeta")

  # Simple CNMA data
  data <- data.frame(
    studyid = rep(paste0("S", 1:6), each = 2),
    treatment = c("A", "B", "A", "C", "B", "C", "A", "AB", "B", "BC", "A", "ABC"),
    effect = c(0, 0.5, 0, 0.6, 0.5, 0.7, 0, 0.8, 0.5, 1.0, 0, 1.2),
    se = rep(0.2, 12),
    stringsAsFactors = FALSE
  )

  # Define components (A, B, C are single components)
  components <- list(
    A = "A",
    B = "B",
    C = "C",
    AB = c("A", "B"),
    BC = c("B", "C"),
    ABC = c("A", "B", "C")
  )

  result <- component_nma(
    data = data,
    studyid = "studyid",
    treatment = "treatment",
    effect = "effect",
    se = "se",
    components = components,
    model = "additive",
    reference = "A"
  )

  expect_type(result, "list")
  expect_s3_class(result, "component_nma")

  # Check component effects
  expect_true("component_effects" %in% names(result))
  expect_true(is.data.frame(result$component_effects))

  # Check predictions
  expect_true("predictions" %in% names(result))

  # Check model type
  expect_equal(result$model, "additive")
})

test_that("component_nma encoding is correct", {
  skip_if_not_installed("netmeta")

  data <- data.frame(
    studyid = rep(paste0("S", 1:4), each = 2),
    treatment = c("A", "AB", "A", "B", "B", "BC", "A", "ABC"),
    effect = c(0, 0.5, 0, 0.4, 0.4, 0.8, 0, 1.0),
    se = rep(0.2, 8),
    stringsAsFactors = FALSE
  )

  components <- list(
    A = "A",
    B = "B",
    C = "C",
    AB = c("A", "B"),
    BC = c("B", "C"),
    ABC = c("A", "B", "C")
  )

  result <- component_nma(
    data = data,
    studyid = "studyid",
    treatment = "treatment",
    effect = "effect",
    se = "se",
    components = components,
    model = "additive"
  )

  # Check encoding matrix
  expect_true("encoding_matrix" %in% names(result))
  encoding <- result$encoding_matrix

  # AB should have 1 for A and B
  expect_equal(encoding["AB", "A"], 1)
  expect_equal(encoding["AB", "B"], 1)
  expect_equal(encoding["AB", "C"], 0)

  # ABC should have 1 for all
  expect_equal(encoding["ABC", "A"], 1)
  expect_equal(encoding["ABC", "B"], 1)
  expect_equal(encoding["ABC", "C"], 1)
})


# ============================================================================
# Test: Prevalence Meta-Analysis
# ============================================================================

test_that("meta_proportion works with logit transformation", {
  data <- data.frame(
    study = paste0("Study", 1:8),
    events = c(15, 22, 18, 30, 25, 20, 28, 17),
    n = c(100, 120, 110, 150, 130, 115, 140, 105)
  )

  result <- meta_proportion(
    data,
    transformation = "logit",
    method = "REML"
  )

  expect_type(result, "list")
  expect_s3_class(result, "meta_proportion")

  # Check pooled proportion
  expect_true("pooled_proportion" %in% names(result))
  expect_true(result$pooled_proportion >= 0 && result$pooled_proportion <= 1)

  # Check CI
  expect_true("ci_lower" %in% names(result))
  expect_true("ci_upper" %in% names(result))
  expect_true(result$ci_lower >= 0 && result$ci_lower <= 1)
  expect_true(result$ci_upper >= 0 && result$ci_upper <= 1)
  expect_true(result$ci_lower < result$ci_upper)

  # Check transformation
  expect_equal(result$transformation, "logit")
})

test_that("meta_proportion works with all transformations", {
  data <- data.frame(
    study = paste0("Study", 1:6),
    events = c(15, 22, 18, 30, 25, 20),
    n = c(100, 120, 110, 150, 130, 115)
  )

  transformations <- c("logit", "arcsine", "freeman_tukey", "log", "identity")

  for (trans in transformations) {
    result <- meta_proportion(data, transformation = trans, method = "DL")

    expect_s3_class(result, "meta_proportion", info = paste("Transform:", trans))
    expect_true(result$pooled_proportion >= 0 && result$pooled_proportion <= 1,
                info = paste("Transform:", trans))
  }
})

test_that("meta_proportion handles extreme proportions", {
  data <- data.frame(
    study = paste0("Study", 1:5),
    events = c(0, 1, 2, 98, 100),  # Include 0% and 100%
    n = c(50, 50, 50, 100, 100)
  )

  result <- meta_proportion(
    data,
    transformation = "logit",
    continuity_correction = 0.5
  )

  expect_s3_class(result, "meta_proportion")
  expect_true(result$pooled_proportion >= 0 && result$pooled_proportion <= 1)
})

test_that("meta_proportion with prediction interval", {
  data <- data.frame(
    study = paste0("Study", 1:8),
    events = c(15, 22, 18, 30, 25, 20, 28, 17),
    n = c(100, 120, 110, 150, 130, 115, 140, 105)
  )

  result <- meta_proportion(
    data,
    transformation = "logit",
    prediction_interval = TRUE
  )

  expect_true("pi_lower" %in% names(result))
  expect_true("pi_upper" %in% names(result))

  # PI should be wider than CI
  width_ci <- result$ci_upper - result$ci_lower
  width_pi <- result$pi_upper - result$pi_lower

  if (result$tau2 > 0) {
    expect_true(width_pi > width_ci)
  }
})


# ============================================================================
# Test: Advanced Meta-Regression with Splines
# ============================================================================

test_that("meta_regression_spline works", {
  skip_if_not_installed("metafor")

  data <- data.frame(
    study = paste0("Study", 1:15),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35, 0.65, 0.5,
               0.52, 0.48, 0.58, 0.42, 0.62),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14, 0.09, 0.11,
           0.10, 0.12, 0.09, 0.13, 0.08),
    year = 2010:2024
  )

  result <- meta_regression_spline(
    data,
    moderator = "year",
    n_knots = 3
  )

  expect_type(result, "list")
  expect_s3_class(result, "cbamm_metareg_spline")

  # Check spline components
  expect_true("spline_terms" %in% names(result))
  expect_true("knot_locations" %in% names(result))
  expect_equal(length(result$knot_locations), 3)

  # Check predictions
  expect_true("fitted_values" %in% names(result))
  expect_equal(length(result$fitted_values), 15)
})

test_that("meta_regression_spline captures non-linear trends", {
  skip_if_not_installed("metafor")

  # Generate data with quadratic trend
  set.seed(456)
  year <- seq(2000, 2020, by = 2)
  effect <- 0.3 + 0.05 * (year - 2010) - 0.003 * (year - 2010)^2 + rnorm(11, 0, 0.1)

  data <- data.frame(
    study = paste0("Study", 1:11),
    effect = effect,
    se = rep(0.1, 11),
    year = year
  )

  result <- meta_regression_spline(data, moderator = "year", n_knots = 3)

  # Check model fit
  expect_true("aic" %in% names(result))
  expect_true("bic" %in% names(result))

  # Fitted values should capture the pattern
  expect_true(cor(result$fitted_values, data$effect) > 0.5)
})


# ============================================================================
# Test: Penalized Meta-Regression
# ============================================================================

test_that("meta_regression_penalized works with LASSO", {
  skip_if_not_installed("metafor")

  set.seed(789)
  data <- data.frame(
    study = paste0("Study", 1:30),
    effect = rnorm(30, 0.5, 0.2),
    se = runif(30, 0.08, 0.15),
    mod1 = rnorm(30, 0, 1),
    mod2 = rnorm(30, 0, 1),
    mod3 = rnorm(30, 0, 1),
    mod4 = rnorm(30, 0, 1),
    mod5 = rnorm(30, 0, 1)
  )

  # Make mod1 and mod2 predictive
  data$effect <- data$effect + 0.3 * data$mod1 + 0.2 * data$mod2

  result <- meta_regression_penalized(
    data,
    moderators = c("mod1", "mod2", "mod3", "mod4", "mod5"),
    penalty = "lasso",
    alpha = 1.0,
    lambda = 0.1
  )

  expect_type(result, "list")
  expect_s3_class(result, "cbamm_metareg_penalized")

  # Check coefficients
  expect_true("coefficients" %in% names(result))
  expect_equal(length(result$coefficients), 5)

  # Some coefficients should be shrunk toward zero
  expect_true(any(abs(result$coefficients) < 0.05))
})

test_that("meta_regression_penalized works with Ridge", {
  skip_if_not_installed("metafor")

  set.seed(321)
  data <- data.frame(
    study = paste0("Study", 1:25),
    effect = rnorm(25, 0.5, 0.2),
    se = runif(25, 0.08, 0.15),
    mod1 = rnorm(25, 0, 1),
    mod2 = rnorm(25, 0, 1),
    mod3 = rnorm(25, 0, 1)
  )

  result <- meta_regression_penalized(
    data,
    moderators = c("mod1", "mod2", "mod3"),
    penalty = "ridge",
    alpha = 0.0,
    lambda = 0.1
  )

  expect_s3_class(result, "cbamm_metareg_penalized")
  expect_equal(result$penalty, "ridge")

  # Ridge shouldn't set coefficients exactly to zero
  # (but will shrink them)
  expect_true(all(result$coefficients != 0))
})

test_that("meta_regression_penalized works with Elastic Net", {
  skip_if_not_installed("metafor")

  set.seed(654)
  data <- data.frame(
    study = paste0("Study", 1:30),
    effect = rnorm(30, 0.5, 0.2),
    se = runif(30, 0.08, 0.15),
    mod1 = rnorm(30, 0, 1),
    mod2 = rnorm(30, 0, 1),
    mod3 = rnorm(30, 0, 1),
    mod4 = rnorm(30, 0, 1)
  )

  result <- meta_regression_penalized(
    data,
    moderators = c("mod1", "mod2", "mod3", "mod4"),
    penalty = "elastic_net",
    alpha = 0.5,
    lambda = 0.1
  )

  expect_s3_class(result, "cbamm_metareg_penalized")
  expect_equal(result$penalty, "elastic_net")
  expect_equal(result$alpha, 0.5)
})


# ============================================================================
# Test: Cross-Design Synthesis
# ============================================================================

test_that("cross_design_synthesis works with additive bias model", {
  skip_if_not_installed("metafor")

  data <- data.frame(
    study = paste0("Study", 1:12),
    effect = c(0.5, 0.48, 0.52, 0.49,  # RCT
               0.6, 0.65, 0.58, 0.62,  # Cohort (biased upward)
               0.7, 0.68, 0.72, 0.69), # Case-control (more biased)
    se = rep(0.1, 12),
    design = rep(c("RCT", "Cohort", "CaseControl"), each = 4)
  )

  result <- cross_design_synthesis(
    data,
    design_var = "design",
    reference_design = "RCT",
    bias_model = "additive"
  )

  expect_type(result, "list")
  expect_s3_class(result, "cross_design_synthesis")

  # Check components
  expect_true("overall_estimate" %in% names(result))
  expect_true("design_effects" %in% names(result))
  expect_true("bias_estimates" %in% names(result))

  # Check bias estimates
  bias_df <- result$bias_estimates

  expect_true(is.data.frame(bias_df))
  expect_true("design" %in% names(bias_df))
  expect_true("bias" %in% names(bias_df))

  # Reference design should have bias = 0
  ref_bias <- bias_df$bias[bias_df$design == "RCT"]
  expect_equal(ref_bias, 0)

  # Other designs should have non-zero bias
  cohort_bias <- bias_df$bias[bias_df$design == "Cohort"]
  expect_true(cohort_bias != 0)
})

test_that("cross_design_synthesis works with multiplicative bias", {
  skip_if_not_installed("metafor")

  data <- data.frame(
    study = paste0("Study", 1:9),
    effect = c(0.5, 0.48, 0.52,  # RCT
               0.6, 0.58, 0.62,  # Cohort
               0.7, 0.68, 0.72), # Case-control
    se = rep(0.1, 9),
    design = rep(c("RCT", "Cohort", "CaseControl"), each = 3)
  )

  result <- cross_design_synthesis(
    data,
    design_var = "design",
    reference_design = "RCT",
    bias_model = "multiplicative"
  )

  expect_s3_class(result, "cross_design_synthesis")
  expect_equal(result$bias_model, "multiplicative")
})

test_that("cross_design_synthesis hierarchical synthesis works", {
  skip_if_not_installed("metafor")

  data <- data.frame(
    study = paste0("Study", 1:10),
    effect = c(0.5, 0.48, 0.52, 0.49, 0.51,  # RCT
               0.6, 0.58, 0.62, 0.59, 0.61), # Cohort
    se = rep(0.1, 10),
    design = rep(c("RCT", "Cohort"), each = 5)
  )

  result <- cross_design_synthesis(
    data,
    design_var = "design",
    reference_design = "RCT",
    hierarchical = TRUE
  )

  expect_true("hierarchical_estimate" %in% names(result))
  expect_true("between_design_tau2" %in% names(result))
})


# ============================================================================
# Integration Tests
# ============================================================================

test_that("advanced heterogeneity integrates with core statistics", {
  data <- data.frame(
    study = paste0("Study", 1:8),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45, 0.55, 0.35),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13, 0.08, 0.14)
  )

  result <- meta_analysis_advanced(data, method = "PM")

  # Should be able to use core statistics functions
  het_stats <- calculate_heterogeneity_stats(data$effect, data$se^2, result$tau2)

  expect_true(!is.null(het_stats))
  expect_equal(het_stats$tau2, result$tau2)
})

test_that("prevalence MA integrates with heterogeneity assessment", {
  data <- data.frame(
    study = paste0("Study", 1:10),
    events = c(15, 22, 18, 30, 25, 20, 28, 17, 23, 19),
    n = c(100, 120, 110, 150, 130, 115, 140, 105, 125, 108)
  )

  result <- meta_proportion(data, transformation = "logit")

  # Should report heterogeneity
  expect_true("tau2" %in% names(result))
  expect_true("I2" %in% names(result))
  expect_true(result$I2 >= 0 && result$I2 <= 100)
})


# ============================================================================
# Print Methods
# ============================================================================

test_that("print methods work for advanced features", {
  data <- data.frame(
    study = paste0("Study", 1:6),
    effect = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.45),
    se = c(0.1, 0.12, 0.09, 0.11, 0.10, 0.13)
  )

  result_het <- meta_analysis_advanced(data, method = "PM")
  expect_output(print(result_het), "Advanced Meta-Analysis")

  data_prev <- data.frame(
    study = paste0("Study", 1:6),
    events = c(15, 22, 18, 30, 25, 20),
    n = c(100, 120, 110, 150, 130, 115)
  )

  result_prev <- meta_proportion(data_prev, transformation = "logit")
  expect_output(print(result_prev), "Meta-Analysis of Proportions")
})
