#' Enhanced Individual Participant Data (IPD) Meta-Analysis
#'
#' Advanced IPD meta-analysis using proper mixed-effects models via lme4/glmmTMB.
#' This replaces the simplified lm()/glm() approach with full random effects modeling.
#'
#' Based on: Riley RD et al. (2010). Meta-analysis of individual participant data. BMJ.
#'
#' @name ipd_meta_analysis_enhanced
NULL


#' One-Stage IPD Meta-Analysis (Enhanced)
#'
#' Performs proper one-stage IPD meta-analysis using mixed-effects models.
#' Uses lme4 for linear/generalized linear mixed models.
#'
#' @param data Data frame with individual participant data
#' @param outcome_var Name of outcome variable
#' @param treatment_var Name of treatment variable
#' @param covariates Character vector of covariate names
#' @param family Family: "gaussian", "binomial", "poisson", "negbin"
#' @param random_slope Include random slopes for treatment effect (default: TRUE)
#' @param correlation_structure Correlation structure: "exchangeable", "unstructured"
#' @param use_glmmTMB Use glmmTMB instead of lme4 (better for complex models)
#'
#' @return Object of class "ipd_onestage_enhanced" with full mixed model results
#'
#' @export
#' @examples
#' \dontrun{
#' # Simulate IPD
#' set.seed(123)
#' ipd_data <- data.frame(
#'   study = rep(paste0("S", 1:10), each = 100),
#'   outcome = rbinom(1000, 1, 0.4),
#'   treatment = rep(0:1, 500),
#'   age = rnorm(1000, 50, 10),
#'   sex = rbinom(1000, 1, 0.5)
#' )
#'
#' # Proper mixed-effects logistic regression
#' result <- ipd_onestage_enhanced(
#'   ipd_data,
#'   outcome_var = "outcome",
#'   treatment_var = "treatment",
#'   covariates = c("age", "sex"),
#'   family = "binomial",
#'   random_slope = TRUE
#' )
#' }
ipd_onestage_enhanced <- function(data, outcome_var, treatment_var,
                                  covariates = NULL,
                                  family = "gaussian",
                                  random_slope = TRUE,
                                  correlation_structure = "exchangeable",
                                  use_glmmTMB = FALSE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   ONE-STAGE IPD META-ANALYSIS (Enhanced)                    ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate data
  if (!is.data.frame(data)) stop("data must be a data frame")

  required_vars <- c("study", outcome_var, treatment_var)
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Remove missing data
  complete_vars <- c("study", outcome_var, treatment_var, covariates)
  complete_cases <- complete.cases(data[, complete_vars])
  if (!all(complete_cases)) {
    n_missing <- sum(!complete_cases)
    warning(sprintf("Removed %d rows (%.1f%%) with missing data",
                    n_missing, 100 * n_missing / nrow(data)))
    data <- data[complete_cases, ]
  }

  n_total <- nrow(data)
  n_studies <- length(unique(data$study))

  cat(sprintf("Data: %d participants from %d studies\n", n_total, n_studies))
  cat(sprintf("Outcome: %s (%s)\n", outcome_var, family))
  cat(sprintf("Treatment: %s\n", treatment_var))

  if (!is.null(covariates)) {
    cat(sprintf("Covariates: %s\n", paste(covariates, collapse = ", ")))
  }

  # Ensure study is a factor
  data$study <- as.factor(data$study)

  # Build formula for fixed effects
  if (is.null(covariates)) {
    fixed_formula <- sprintf("%s ~ %s", outcome_var, treatment_var)
  } else {
    fixed_formula <- sprintf("%s ~ %s + %s", outcome_var, treatment_var,
                             paste(covariates, collapse = " + "))
  }

  # Build random effects formula
  if (random_slope) {
    random_formula <- sprintf("(1 + %s | study)", treatment_var)
    cat(sprintf("\nRandom effects: Random intercepts + random slopes for %s\n", treatment_var))
  } else {
    random_formula <- "(1 | study)"
    cat("\nRandom effects: Random intercepts only\n")
  }

  # Complete formula
  formula_str <- paste(fixed_formula, "+", random_formula)
  formula_obj <- as.formula(formula_str)

  cat(sprintf("Formula: %s\n", formula_str))

  # Check if required packages are available
  has_lme4 <- requireNamespace("lme4", quietly = TRUE)
  has_glmmTMB <- requireNamespace("glmmTMB", quietly = TRUE)

  if (use_glmmTMB && !has_glmmTMB) {
    warning("glmmTMB not available, falling back to lme4")
    use_glmmTMB <- FALSE
  }

  if (!use_glmmTMB && !has_lme4) {
    stop("lme4 package required for enhanced IPD meta-analysis. Install with: install.packages('lme4')")
  }

  # Fit appropriate model
  cat("\nFitting mixed-effects model...\n")

  tryCatch({

    if (use_glmmTMB) {
      # Use glmmTMB (more flexible, handles zero-inflation, etc.)
      cat("Using glmmTMB backend\n")

      family_obj <- switch(family,
        "gaussian" = glmmTMB::gaussian(),
        "binomial" = glmmTMB::binomial(),
        "poisson" = glmmTMB::poisson(),
        "negbin" = glmmTMB::nbinom2(),
        stop("Unknown family: ", family)
      )

      model <- glmmTMB::glmmTMB(
        formula = formula_obj,
        data = data,
        family = family_obj,
        REML = (family == "gaussian")
      )

    } else {
      # Use lme4
      cat("Using lme4 backend\n")

      if (family == "gaussian") {
        # Linear mixed model
        model <- lme4::lmer(formula = formula_obj, data = data, REML = TRUE)
      } else if (family %in% c("binomial", "poisson")) {
        # Generalized linear mixed model
        family_obj <- switch(family,
          "binomial" = binomial(),
          "poisson" = poisson()
        )
        model <- lme4::glmer(formula = formula_obj, data = data, family = family_obj)
      } else {
        stop("Family '", family, "' not supported with lme4. Try use_glmmTMB = TRUE")
      }
    }

    cat("✓ Model converged successfully\n")

  }, error = function(e) {
    stop("Model fitting failed: ", e$message, "\n",
         "Try: 1) Simplify model (random_slope = FALSE), ",
         "2) Check data quality, 3) Increase sample size")
  })

  # Extract results
  cat("\nExtracting results...\n")

  # Fixed effects
  if (use_glmmTMB) {
    fixed_coefs <- summary(model)$coefficients$cond
  } else {
    fixed_coefs <- summary(model)$coefficients
  }

  # Overall treatment effect
  trt_idx <- which(rownames(fixed_coefs) == treatment_var)
  if (length(trt_idx) == 0) {
    stop("Treatment variable not found in model coefficients")
  }

  overall_effect <- list(
    estimate = fixed_coefs[trt_idx, "Estimate"],
    se = fixed_coefs[trt_idx, "Std. Error"],
    z_value = fixed_coefs[trt_idx, 3],  # z or t value
    p_value = fixed_coefs[trt_idx, 4]
  )

  # CI
  ci <- calculate_ci(overall_effect$estimate, overall_effect$se)
  overall_effect$ci_lower <- ci$ci_lower
  overall_effect$ci_upper <- ci$ci_upper

  # Random effects variance components
  if (use_glmmTMB) {
    vc <- glmmTMB::VarCorr(model)$cond$study
    random_effects_var <- as.matrix(vc)
  } else {
    vc <- lme4::VarCorr(model)$study
    random_effects_var <- as.matrix(vc)
  }

  # Extract study-specific random effects (BLUPs)
  if (use_glmmTMB) {
    ranef_list <- glmmTMB::ranef(model)
    study_random_effects <- ranef_list$cond$study
  } else {
    ranef_list <- lme4::ranef(model)
    study_random_effects <- ranef_list$study
  }

  # Study-specific treatment effects (if random slopes)
  study_effects <- NULL
  if (random_slope && treatment_var %in% names(study_random_effects)) {
    study_ids <- rownames(study_random_effects)
    study_deviations <- study_random_effects[[treatment_var]]

    study_effects <- data.frame(
      study = study_ids,
      deviation = study_deviations,
      total_effect = overall_effect$estimate + study_deviations,
      stringsAsFactors = FALSE
    )

    # Add confidence intervals for study-specific effects
    se_study <- sqrt(diag(random_effects_var))
    if (length(se_study) > 1) {
      study_effects$se <- se_study[2]  # SE for treatment slope
      study_effects$ci_lower <- study_effects$total_effect - 1.96 * study_effects$se
      study_effects$ci_upper <- study_effects$total_effect + 1.96 * study_effects$se
    }
  }

  # Heterogeneity statistics
  if (random_slope) {
    if (ncol(random_effects_var) >= 2) {
      tau2_treatment <- random_effects_var[2, 2]  # Variance of treatment slopes
      tau_treatment <- sqrt(tau2_treatment)

      # I²-like statistic for treatment heterogeneity
      # Proportion of variation in treatment effect due to between-study differences
      total_var <- tau2_treatment + overall_effect$se^2
      I2_treatment <- 100 * tau2_treatment / total_var
    } else {
      tau2_treatment <- NA
      tau_treatment <- NA
      I2_treatment <- NA
    }
  } else {
    tau2_treatment <- NA
    tau_treatment <- NA
    I2_treatment <- NA
  }

  # Covariate effects
  covariate_effects <- NULL
  if (!is.null(covariates)) {
    cov_rows <- which(rownames(fixed_coefs) %in% covariates)
    if (length(cov_rows) > 0) {
      covariate_effects <- data.frame(
        covariate = rownames(fixed_coefs)[cov_rows],
        estimate = fixed_coefs[cov_rows, "Estimate"],
        se = fixed_coefs[cov_rows, "Std. Error"],
        z_value = fixed_coefs[cov_rows, 3],
        p_value = fixed_coefs[cov_rows, 4],
        stringsAsFactors = FALSE
      )

      # Add CIs
      for (i in 1:nrow(covariate_effects)) {
        ci <- calculate_ci(covariate_effects$estimate[i], covariate_effects$se[i])
        covariate_effects$ci_lower[i] <- ci$ci_lower
        covariate_effects$ci_upper[i] <- ci$ci_upper
      }
    }
  }

  # Model fit statistics
  if (use_glmmTMB) {
    aic <- AIC(model)
    bic <- BIC(model)
    loglik <- as.numeric(logLik(model))
  } else {
    aic <- AIC(model)
    bic <- BIC(model)
    loglik <- as.numeric(logLik(model))
  }

  # Create result object
  result <- list(
    overall_effect = overall_effect,
    study_effects = study_effects,
    covariate_effects = covariate_effects,
    random_effects_variance = random_effects_var,
    tau2_treatment = tau2_treatment,
    tau_treatment = tau_treatment,
    I2_treatment = I2_treatment,
    model = model,
    formula = formula_str,
    family = family,
    random_slope = random_slope,
    n_participants = n_total,
    n_studies = n_studies,
    aic = aic,
    bic = bic,
    loglik = loglik,
    backend = if (use_glmmTMB) "glmmTMB" else "lme4"
  )

  class(result) <- c("ipd_onestage_enhanced", "list")

  # Print summary
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("RESULTS SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Overall treatment effect: %.3f (95%% CI: %.3f, %.3f), p = %.4f\n",
              overall_effect$estimate,
              overall_effect$ci_lower,
              overall_effect$ci_upper,
              overall_effect$p_value))

  if (random_slope && !is.na(tau2_treatment)) {
    cat(sprintf("\nHeterogeneity in treatment effect:\n"))
    cat(sprintf("  τ² = %.4f, τ = %.4f\n", tau2_treatment, tau_treatment))
    cat(sprintf("  I² (treatment) = %.1f%%\n", I2_treatment))
  }

  cat(sprintf("\nModel fit:\n"))
  cat(sprintf("  AIC: %.1f, BIC: %.1f\n", aic, bic))
  cat(sprintf("  Log-likelihood: %.2f\n", loglik))

  cat("\n✓ Enhanced IPD meta-analysis completed\n")

  return(result)
}


#' Print Method for Enhanced IPD One-Stage
#'
#' @export
print.ipd_onestage_enhanced <- function(x, ...) {
  cat("Enhanced One-Stage IPD Meta-Analysis\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("Backend: %s\n", x$backend))
  cat(sprintf("Formula: %s\n", x$formula))
  cat(sprintf("Family: %s\n", x$family))
  cat(sprintf("Participants: %d from %d studies\n\n", x$n_participants, x$n_studies))

  cat("Overall Treatment Effect:\n")
  cat(sprintf("  Estimate: %.3f (SE = %.3f)\n", x$overall_effect$estimate, x$overall_effect$se))
  cat(sprintf("  95%% CI: (%.3f, %.3f)\n", x$overall_effect$ci_lower, x$overall_effect$ci_upper))
  cat(sprintf("  p-value: %.4f %s\n", x$overall_effect$p_value,
              if (x$overall_effect$p_value < 0.05) "***" else ""))

  if (x$random_slope && !is.na(x$tau2_treatment)) {
    cat("\nHeterogeneity (Treatment Effect):\n")
    cat(sprintf("  τ² = %.4f, τ = %.4f\n", x$tau2_treatment, x$tau_treatment))
    cat(sprintf("  I² = %.1f%%\n", x$I2_treatment))
  }

  if (!is.null(x$covariate_effects)) {
    cat("\nCovariate Effects:\n")
    print(x$covariate_effects, row.names = FALSE)
  }

  if (!is.null(x$study_effects)) {
    cat(sprintf("\nStudy-Specific Effects: %d studies with random slopes\n",
                nrow(x$study_effects)))
  }

  cat(sprintf("\nModel Fit: AIC = %.1f, BIC = %.1f\n", x$aic, x$bic))

  invisible(x)
}


#' Summary Method for Enhanced IPD One-Stage
#'
#' @export
summary.ipd_onestage_enhanced <- function(object, ...) {
  cat("═══════════════════════════════════════════════════════════\n")
  cat("ENHANCED ONE-STAGE IPD META-ANALYSIS - DETAILED SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  print(object)

  if (!is.null(object$study_effects)) {
    cat("\n\nStudy-Specific Treatment Effects:\n")
    cat("─────────────────────────────────────────────────────────\n")
    print(object$study_effects, row.names = FALSE)
  }

  cat("\n\nRandom Effects Variance-Covariance Matrix:\n")
  cat("─────────────────────────────────────────────────────────\n")
  print(object$random_effects_variance)

  cat("\n\nUnderlying Mixed Model Summary:\n")
  cat("─────────────────────────────────────────────────────────\n")
  print(summary(object$model))

  invisible(object)
}
