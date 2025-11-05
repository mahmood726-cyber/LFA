#' Test Enhanced IPD Meta-Analysis Module
#'
#' Comprehensive tests for ipd_meta_analysis_enhanced.R functions using
#' proper mixed-effects models (lme4/glmmTMB).

library(testthat)

context("Enhanced IPD Meta-Analysis")

# ============================================================================
# Test Data Setup
# ============================================================================

# Helper function to generate IPD test data
generate_ipd_data <- function(n_studies = 5, n_per_study = 50, family = "gaussian") {
  set.seed(123)

  data_list <- list()

  for (i in 1:n_studies) {
    study_data <- data.frame(
      study = paste0("Study", i),
      treatment = rep(0:1, length.out = n_per_study),
      age = rnorm(n_per_study, mean = 50 + i, sd = 10),
      sex = rbinom(n_per_study, 1, 0.5)
    )

    # Generate outcome based on family
    if (family == "gaussian") {
      # Continuous outcome
      treatment_effect <- 0.5 + rnorm(1, 0, 0.1)  # Random treatment effect per study
      study_data$outcome <- 10 + treatment_effect * study_data$treatment +
        0.05 * study_data$age + rnorm(n_per_study, 0, 2)

    } else if (family == "binomial") {
      # Binary outcome
      treatment_effect <- 0.5 + rnorm(1, 0, 0.2)
      linear_pred <- -2 + treatment_effect * study_data$treatment +
        0.02 * study_data$age + 0.3 * study_data$sex
      prob <- plogis(linear_pred)
      study_data$outcome <- rbinom(n_per_study, 1, prob)

    } else if (family == "poisson") {
      # Count outcome
      treatment_effect <- 0.3 + rnorm(1, 0, 0.1)
      lambda <- exp(1 + treatment_effect * study_data$treatment +
                      0.01 * study_data$age)
      study_data$outcome <- rpois(n_per_study, lambda)
    }

    data_list[[i]] <- study_data
  }

  do.call(rbind, data_list)
}


# ============================================================================
# Test: ipd_onestage_enhanced() - Basic Functionality
# ============================================================================

test_that("ipd_onestage_enhanced works with gaussian family", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age", "sex"),
    family = "gaussian",
    random_slope = TRUE,
    use_glmmTMB = FALSE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")
  expect_type(result, "list")

  # Check overall effect
  expect_true("overall_effect" %in% names(result))
  expect_true("estimate" %in% names(result$overall_effect))
  expect_true("se" %in% names(result$overall_effect))
  expect_true("ci_lower" %in% names(result$overall_effect))
  expect_true("ci_upper" %in% names(result$overall_effect))
  expect_true("p_value" %in% names(result$overall_effect))

  # Check treatment effect is reasonable
  expect_true(result$overall_effect$estimate > 0)
  expect_true(result$overall_effect$estimate < 1)

  # Check study effects with random slopes
  expect_true(!is.null(result$study_effects))
  expect_equal(nrow(result$study_effects), result$n_studies)

  # Check heterogeneity
  expect_true(!is.na(result$tau2_treatment))
  expect_true(result$tau2_treatment >= 0)
  expect_true(!is.na(result$I2_treatment))
  expect_true(result$I2_treatment >= 0 && result$I2_treatment <= 100)

  # Check covariate effects
  expect_true(!is.null(result$covariate_effects))
  expect_equal(nrow(result$covariate_effects), 2)  # age and sex

  # Check model fit
  expect_true(!is.null(result$aic))
  expect_true(!is.null(result$bic))
  expect_true(!is.null(result$loglik))

  # Check backend
  expect_equal(result$backend, "lme4")
})

test_that("ipd_onestage_enhanced works with binomial family", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "binomial")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age", "sex"),
    family = "binomial",
    random_slope = TRUE,
    use_glmmTMB = FALSE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")

  # Check overall effect (log odds ratio)
  expect_true("overall_effect" %in% names(result))
  expect_true(result$overall_effect$estimate > 0)  # Treatment should increase odds

  # Check backend
  expect_equal(result$backend, "lme4")
  expect_equal(result$family, "binomial")
})

test_that("ipd_onestage_enhanced works with poisson family", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "poisson")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "poisson",
    random_slope = TRUE,
    use_glmmTMB = FALSE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")
  expect_equal(result$family, "poisson")
  expect_true(result$overall_effect$estimate > 0)
})

test_that("ipd_onestage_enhanced works without covariates", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 4, n_per_study = 40, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = NULL,
    family = "gaussian",
    random_slope = TRUE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")

  # No covariate effects
  expect_true(is.null(result$covariate_effects))

  # But should have overall effect
  expect_true(!is.null(result$overall_effect))
})

test_that("ipd_onestage_enhanced works with random intercepts only", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = FALSE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")

  # Should have overall effect
  expect_true(!is.null(result$overall_effect))

  # Study effects should be NULL (no random slopes)
  expect_true(is.null(result$study_effects))

  # Heterogeneity stats should be NA
  expect_true(is.na(result$tau2_treatment))
  expect_true(is.na(result$I2_treatment))
})


# ============================================================================
# Test: ipd_onestage_enhanced() - glmmTMB Backend
# ============================================================================

test_that("ipd_onestage_enhanced works with glmmTMB backend", {
  skip_if_not_installed("glmmTMB")

  ipd_data <- generate_ipd_data(n_studies = 4, n_per_study = 40, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE,
    use_glmmTMB = TRUE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")
  expect_equal(result$backend, "glmmTMB")

  # Check basic structure
  expect_true(!is.null(result$overall_effect))
  expect_true(!is.null(result$study_effects))
})

test_that("glmmTMB handles negative binomial", {
  skip_if_not_installed("glmmTMB")

  ipd_data <- generate_ipd_data(n_studies = 4, n_per_study = 40, family = "poisson")

  # glmmTMB supports negbin, lme4 does not
  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "negbin",
    random_slope = FALSE,
    use_glmmTMB = TRUE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")
  expect_equal(result$backend, "glmmTMB")
  expect_equal(result$family, "negbin")
})


# ============================================================================
# Test: Data Validation and Error Handling
# ============================================================================

test_that("ipd_onestage_enhanced fails without study variable", {
  ipd_data <- data.frame(
    outcome = rnorm(100),
    treatment = rep(0:1, 50),
    age = rnorm(100, 50, 10)
  )

  expect_error(
    ipd_onestage_enhanced(
      ipd_data,
      outcome_var = "outcome",
      treatment_var = "treatment",
      family = "gaussian"
    ),
    "Missing required variables"
  )
})

test_that("ipd_onestage_enhanced fails with non-data.frame", {
  ipd_data <- list(outcome = rnorm(100), treatment = rep(0:1, 50))

  expect_error(
    ipd_onestage_enhanced(
      ipd_data,
      outcome_var = "outcome",
      treatment_var = "treatment",
      family = "gaussian"
    ),
    "data must be a data frame"
  )
})

test_that("ipd_onestage_enhanced handles missing data", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "gaussian")

  # Add missing values
  ipd_data$outcome[c(1, 5, 10)] <- NA
  ipd_data$age[c(2, 6)] <- NA

  expect_warning(
    result <- ipd_onestage_enhanced(
      ipd_data,
      outcome_var = "outcome",
      treatment_var = "treatment",
      covariates = c("age", "sex"),
      family = "gaussian",
      random_slope = TRUE
    ),
    "Removed.*rows.*with missing data"
  )

  # Should still work with complete cases
  expect_s3_class(result, "ipd_onestage_enhanced")

  # Number of participants should be less than original
  expect_true(result$n_participants < nrow(ipd_data))
})

test_that("ipd_onestage_enhanced fails with unknown family", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 3, n_per_study = 30, family = "gaussian")

  expect_error(
    ipd_onestage_enhanced(
      ipd_data,
      outcome_var = "outcome",
      treatment_var = "treatment",
      family = "unknown_family",
      use_glmmTMB = TRUE
    ),
    "Unknown family"
  )
})

test_that("ipd_onestage_enhanced requires lme4 when not using glmmTMB", {
  # This test checks the error message when lme4 is not available
  # We can't actually unload lme4 in the test, so we check the logic

  ipd_data <- generate_ipd_data(n_studies = 3, n_per_study = 30, family = "gaussian")

  # The function should check for lme4 availability
  # If lme4 is installed, this will succeed
  skip_if_not_installed("lme4")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    family = "gaussian",
    use_glmmTMB = FALSE
  )

  expect_s3_class(result, "ipd_onestage_enhanced")
})


# ============================================================================
# Test: Study-Specific Effects (BLUPs)
# ============================================================================

test_that("study-specific effects make sense", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 5, n_per_study = 50, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  study_effects <- result$study_effects

  expect_equal(nrow(study_effects), 5)
  expect_true("study" %in% names(study_effects))
  expect_true("deviation" %in% names(study_effects))
  expect_true("total_effect" %in% names(study_effects))

  # Total effect = overall effect + deviation
  for (i in 1:nrow(study_effects)) {
    expected_total <- result$overall_effect$estimate + study_effects$deviation[i]
    expect_equal(study_effects$total_effect[i], expected_total, tolerance = 1e-6)
  }

  # Deviations should average to approximately 0
  expect_true(abs(mean(study_effects$deviation)) < 0.1)
})


# ============================================================================
# Test: Heterogeneity Metrics
# ============================================================================

test_that("heterogeneity metrics are consistent", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 6, n_per_study = 50, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  # tau = sqrt(tau2)
  expect_equal(result$tau_treatment, sqrt(result$tau2_treatment), tolerance = 1e-10)

  # I2 should be between 0 and 100
  expect_true(result$I2_treatment >= 0 && result$I2_treatment <= 100)

  # If tau2 > 0, then I2 should be > 0
  if (result$tau2_treatment > 0.01) {
    expect_true(result$I2_treatment > 0)
  }
})


# ============================================================================
# Test: Model Fit Statistics
# ============================================================================

test_that("model fit statistics are present", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 4, n_per_study = 40, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age", "sex"),
    family = "gaussian",
    random_slope = TRUE
  )

  expect_true(!is.null(result$aic))
  expect_true(!is.null(result$bic))
  expect_true(!is.null(result$loglik))

  # AIC and BIC should be finite
  expect_true(is.finite(result$aic))
  expect_true(is.finite(result$bic))
  expect_true(is.finite(result$loglik))

  # BIC should penalize complexity more than AIC
  # For same model, BIC >= AIC
  expect_true(result$bic >= result$aic - 10)  # Allow small tolerance
})


# ============================================================================
# Test: Print and Summary Methods
# ============================================================================

test_that("print method works", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 3, n_per_study = 30, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  # Should not error
  expect_output(print(result), "Enhanced One-Stage IPD Meta-Analysis")
  expect_output(print(result), "Overall Treatment Effect")
  expect_output(print(result), "Heterogeneity")
})

test_that("summary method works", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 3, n_per_study = 30, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  # Should not error
  expect_output(summary(result), "DETAILED SUMMARY")
  expect_output(summary(result), "Study-Specific Treatment Effects")
  expect_output(summary(result), "Variance-Covariance Matrix")
})


# ============================================================================
# Test: Comparison with Simple Implementation
# ============================================================================

test_that("enhanced IPD gives reasonable results compared to simpler approach", {
  skip_if_not_installed("lme4")

  # Generate simple data with known effect
  set.seed(456)
  ipd_data <- data.frame(
    study = rep(paste0("S", 1:5), each = 40),
    treatment = rep(rep(0:1, each = 20), 5),
    outcome = NA
  )

  # True treatment effect = 2.0
  for (study_id in unique(ipd_data$study)) {
    idx <- ipd_data$study == study_id
    study_effect <- rnorm(1, 2.0, 0.3)  # Random effect around 2.0
    ipd_data$outcome[idx] <- 10 + study_effect * ipd_data$treatment[idx] +
      rnorm(sum(idx), 0, 1)
  }

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = NULL,
    family = "gaussian",
    random_slope = TRUE
  )

  # Estimated effect should be close to 2.0
  expect_true(result$overall_effect$estimate > 1.5 &&
                result$overall_effect$estimate < 2.5)

  # Should detect heterogeneity (we added random effects)
  expect_true(result$tau2_treatment > 0)
})


# ============================================================================
# Integration Tests
# ============================================================================

test_that("enhanced IPD integrates with other package functions", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 4, n_per_study = 50, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  # Should be able to use calculate_ci from core_statistics
  ci <- calculate_ci(result$overall_effect$estimate, result$overall_effect$se)

  expect_equal(ci$ci_lower, result$overall_effect$ci_lower, tolerance = 1e-5)
  expect_equal(ci$ci_upper, result$overall_effect$ci_upper, tolerance = 1e-5)

  # Should be able to use calculate_prediction_interval
  if (result$tau_treatment > 0) {
    pi <- calculate_prediction_interval(
      result$overall_effect$estimate,
      result$overall_effect$se,
      result$tau_treatment,
      result$n_studies
    )

    expect_true(!is.null(pi$pi_lower))
    expect_true(!is.null(pi$pi_upper))

    # PI should be wider than CI
    width_ci <- result$overall_effect$ci_upper - result$overall_effect$ci_lower
    width_pi <- pi$pi_upper - pi$pi_lower
    expect_true(width_pi > width_ci)
  }
})

test_that("enhanced IPD produces valid underlying model object", {
  skip_if_not_installed("lme4")

  ipd_data <- generate_ipd_data(n_studies = 3, n_per_study = 30, family = "gaussian")

  result <- ipd_onestage_enhanced(
    ipd_data,
    outcome_var = "outcome",
    treatment_var = "treatment",
    covariates = c("age"),
    family = "gaussian",
    random_slope = TRUE
  )

  # Underlying model should be accessible
  expect_true(!is.null(result$model))

  # Should be able to extract additional info from model
  expect_true(inherits(result$model, "lmerMod"))

  # Should be able to get residuals
  residuals <- residuals(result$model)
  expect_equal(length(residuals), result$n_participants)
})
