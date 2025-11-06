#' Individual Participant Data (IPD) Meta-Analysis
#'
#' Methods for meta-analysis of individual participant data, offering more power
#' and flexibility than aggregate data meta-analysis.
#'
#' @name ipd_meta_analysis
NULL

#' One-Stage IPD Meta-Analysis
#'
#' Performs one-stage IPD meta-analysis by analyzing all individual participant data
#' simultaneously with random effects for between-study heterogeneity.
#'
#' @param data Data frame with individual participant data containing:
#'   \itemize{
#'     \item study: Study identifier
#'     \item outcome: Outcome variable
#'     \item treatment: Treatment indicator (0/1 or factor)
#'     \item Additional covariates as needed
#'   }
#' @param outcome_var Name of outcome variable
#' @param treatment_var Name of treatment variable
#' @param covariates Character vector of covariate names (optional)
#' @param family Family for GLM: "gaussian", "binomial", or "poisson"
#' @param random_slope Logical indicating if treatment effect should vary by study
#'
#' @return Object of class "ipd_onestage" containing:
#'   \itemize{
#'     \item overall_effect: Overall treatment effect
#'     \item study_effects: Study-specific effects
#'     \item covariate_effects: Effects of covariates
#'     \item heterogeneity: Between-study variance
#'     \item model: Underlying model object
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Simulate IPD
#' ipd_data <- data.frame(
#'   study = rep(1:5, each = 100),
#'   outcome = rnorm(500),
#'   treatment = rep(0:1, 250),
#'   age = rnorm(500, 50, 10),
#'   sex = sample(0:1, 500, replace = TRUE)
#' )
#'
#' result <- ipd_onestage(ipd_data, outcome_var = "outcome",
#'                        treatment_var = "treatment",
#'                        covariates = c("age", "sex"))
#' print(result)
#' }
ipd_onestage <- function(data, outcome_var, treatment_var,
                         covariates = NULL, family = "gaussian",
                         random_slope = TRUE) {
  # Validate data
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_vars <- c("study", outcome_var, treatment_var)
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Remove missing data
  complete_vars <- c("study", outcome_var, treatment_var, covariates)
  complete_cases <- complete.cases(data[, complete_vars])
  if (!all(complete_cases)) {
    warning(sprintf("Removed %d rows with missing data", sum(!complete_cases)))
    data <- data[complete_cases, ]
  }

  # Build formula
  if (is.null(covariates)) {
    formula_str <- sprintf("%s ~ %s", outcome_var, treatment_var)
  } else {
    formula_str <- sprintf("%s ~ %s + %s", outcome_var, treatment_var,
                           paste(covariates, collapse = " + "))
  }

  # Add random effects
  # This is simplified - full implementation would use lme4 or nlme
  if (random_slope) {
    formula_str <- paste0(formula_str, " + study + study:", treatment_var)
  } else {
    formula_str <- paste0(formula_str, " + study")
  }

  formula_obj <- as.formula(formula_str)

  # Fit model (simplified - treating study as fixed effect)
  # Full implementation would use lmer() or glmer()
  if (family == "gaussian") {
    model <- lm(formula_obj, data = data)
  } else if (family == "binomial") {
    model <- glm(formula_obj, data = data, family = binomial())
  } else if (family == "poisson") {
    model <- glm(formula_obj, data = data, family = poisson())
  } else {
    stop("family must be 'gaussian', 'binomial', or 'poisson'")
  }

  # Extract overall treatment effect
  coef_summary <- summary(model)$coefficients
  trt_idx <- which(rownames(coef_summary) == treatment_var)

  overall_effect <- list(
    estimate = coef_summary[trt_idx, "Estimate"],
    se = coef_summary[trt_idx, "Std. Error"],
    ci_lower = coef_summary[trt_idx, "Estimate"] - 1.96 * coef_summary[trt_idx, "Std. Error"],
    ci_upper = coef_summary[trt_idx, "Estimate"] + 1.96 * coef_summary[trt_idx, "Std. Error"],
    p_value = coef_summary[trt_idx, 4]  # p-value column
  )

  # Extract study-specific effects (if random slope)
  study_effects <- NULL
  if (random_slope) {
    # Get interaction coefficients
    interaction_rows <- grep(paste0("study.*:", treatment_var), rownames(coef_summary))
    if (length(interaction_rows) > 0) {
      study_ids <- unique(data$study)
      study_effects <- data.frame(
        study = study_ids[-1],  # First study is baseline
        effect_deviation = coef_summary[interaction_rows, "Estimate"],
        stringsAsFactors = FALSE
      )
      study_effects$total_effect <- overall_effect$estimate + study_effects$effect_deviation
    }
  }

  # Extract covariate effects
  covariate_effects <- NULL
  if (!is.null(covariates)) {
    cov_rows <- which(rownames(coef_summary) %in% covariates)
    if (length(cov_rows) > 0) {
      covariate_effects <- data.frame(
        covariate = rownames(coef_summary)[cov_rows],
        estimate = coef_summary[cov_rows, "Estimate"],
        se = coef_summary[cov_rows, "Std. Error"],
        p_value = coef_summary[cov_rows, 4],
        stringsAsFactors = FALSE
      )
    }
  }

  # Estimate heterogeneity (simplified)
  if (!is.null(study_effects)) {
    tau2 <- var(study_effects$effect_deviation)
  } else {
    tau2 <- 0
  }

  result <- list(
    overall_effect = overall_effect,
    study_effects = study_effects,
    covariate_effects = covariate_effects,
    heterogeneity = list(
      tau2 = tau2,
      tau = sqrt(tau2)
    ),
    model = model,
    data = data,
    n_studies = length(unique(data$study)),
    n_participants = nrow(data),
    family = family,
    note = "Simplified one-stage IPD. For full mixed-effects models, use lme4 package."
  )

  class(result) <- "ipd_onestage"
  return(result)
}


#' Two-Stage IPD Meta-Analysis
#'
#' Performs two-stage IPD meta-analysis: first analyzes each study separately,
#' then pools the study-specific estimates.
#'
#' @param data Data frame with IPD
#' @param outcome_var Name of outcome variable
#' @param treatment_var Name of treatment variable
#' @param covariates Character vector of covariates
#' @param family Family for GLM
#' @param method Pooling method: "DL" or "REML"
#'
#' @return Object of class "ipd_twostage" with study-specific and pooled results
#'
#' @export
ipd_twostage <- function(data, outcome_var, treatment_var,
                         covariates = NULL, family = "gaussian",
                         method = "DL") {
  # Validate data
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Get unique studies
  studies <- unique(data$study)
  k <- length(studies)

  # Stage 1: Analyze each study separately
  study_results <- data.frame(
    study = character(k),
    estimate = numeric(k),
    se = numeric(k),
    n = integer(k),
    stringsAsFactors = FALSE
  )

  for (i in 1:k) {
    study_data <- data[data$study == studies[i], ]

    # Build formula
    if (is.null(covariates)) {
      formula_str <- sprintf("%s ~ %s", outcome_var, treatment_var)
    } else {
      formula_str <- sprintf("%s ~ %s + %s", outcome_var, treatment_var,
                             paste(covariates, collapse = " + "))
    }

    formula_obj <- as.formula(formula_str)

    # Fit model
    tryCatch({
      if (family == "gaussian") {
        model <- lm(formula_obj, data = study_data)
      } else if (family == "binomial") {
        model <- glm(formula_obj, data = study_data, family = binomial())
      } else if (family == "poisson") {
        model <- glm(formula_obj, data = study_data, family = poisson())
      }

      coef_summary <- summary(model)$coefficients
      trt_idx <- which(rownames(coef_summary) == treatment_var)

      study_results$study[i] <- as.character(studies[i])
      study_results$estimate[i] <- coef_summary[trt_idx, "Estimate"]
      study_results$se[i] <- coef_summary[trt_idx, "Std. Error"]
      study_results$n[i] <- nrow(study_data)

    }, error = function(e) {
      warning(sprintf("Could not analyze study %s: %s", studies[i], e$message))
      study_results$study[i] <<- as.character(studies[i])
      study_results$estimate[i] <<- NA
      study_results$se[i] <<- NA
      study_results$n[i] <<- nrow(study_data)
    })
  }

  # Remove studies with failed analysis
  study_results <- study_results[!is.na(study_results$estimate), ]

  # Stage 2: Pool study-specific estimates
  pooled_result <- cbamm_fast(study_results, method = method, verbose = FALSE)

  result <- list(
    study_results = study_results,
    pooled_result = pooled_result,
    n_studies = nrow(study_results),
    total_participants = sum(study_results$n),
    method = method,
    family = family
  )

  class(result) <- "ipd_twostage"
  return(result)
}


#' IPD vs Aggregate Data Comparison
#'
#' Compares results from IPD meta-analysis with aggregate data meta-analysis
#' to assess information gain from IPD.
#'
#' @param ipd_result IPD meta-analysis result
#' @param aggregate_result Aggregate data meta-analysis result
#'
#' @return List comparing the two approaches
#'
#' @export
compare_ipd_aggregate <- function(ipd_result, aggregate_result) {
  # Extract estimates
  if (inherits(ipd_result, "ipd_onestage")) {
    ipd_est <- ipd_result$overall_effect$estimate
    ipd_se <- ipd_result$overall_effect$se
  } else if (inherits(ipd_result, "ipd_twostage")) {
    ipd_est <- ipd_result$pooled_result$estimate
    ipd_se <- ipd_result$pooled_result$se
  } else {
    stop("ipd_result must be from ipd_onestage() or ipd_twostage()")
  }

  if (inherits(aggregate_result, "cbamm")) {
    agg_est <- aggregate_result$estimate
    agg_se <- aggregate_result$se
  } else {
    stop("aggregate_result must be a cbamm object")
  }

  # Calculate differences
  est_diff <- ipd_est - agg_est
  se_ratio <- ipd_se / agg_se
  precision_gain <- (1 / agg_se^2) - (1 / ipd_se^2)

  # Test for difference
  z_stat <- est_diff / sqrt(ipd_se^2 + agg_se^2)
  p_value <- 2 * pnorm(-abs(z_stat))

  comparison <- list(
    ipd_estimate = ipd_est,
    ipd_se = ipd_se,
    aggregate_estimate = agg_est,
    aggregate_se = agg_se,
    difference = est_diff,
    se_ratio = se_ratio,
    precision_gain_percent = precision_gain / (1 / agg_se^2) * 100,
    test_statistic = z_stat,
    p_value = p_value,
    interpretation = if (p_value < 0.05)
      "Significant difference between IPD and aggregate estimates"
    else
      "No significant difference between approaches"
  )

  return(comparison)
}


#' IPD Meta-Analysis with Treatment-Covariate Interactions
#'
#' Examines whether treatment effects vary by participant characteristics
#' (effect modification / subgroup effects).
#'
#' @param data IPD data frame
#' @param outcome_var Outcome variable name
#' @param treatment_var Treatment variable name
#' @param moderator Moderator variable name
#' @param covariates Additional covariates
#' @param family Model family
#'
#' @return Object with interaction results
#'
#' @export
ipd_interaction_analysis <- function(data, outcome_var, treatment_var,
                                      moderator, covariates = NULL,
                                      family = "gaussian") {
  # Build formula with interaction
  if (is.null(covariates)) {
    formula_str <- sprintf("%s ~ %s * %s + study", outcome_var, treatment_var, moderator)
  } else {
    formula_str <- sprintf("%s ~ %s * %s + %s + study",
                           outcome_var, treatment_var, moderator,
                           paste(covariates, collapse = " + "))
  }

  formula_obj <- as.formula(formula_str)

  # Fit model
  if (family == "gaussian") {
    model <- lm(formula_obj, data = data)
  } else if (family == "binomial") {
    model <- glm(formula_obj, data = data, family = binomial())
  } else if (family == "poisson") {
    model <- glm(formula_obj, data = data, family = poisson())
  }

  coef_summary <- summary(model)$coefficients

  # Extract interaction effect
  interaction_term <- paste0(treatment_var, ":", moderator)
  if (!interaction_term %in% rownames(coef_summary)) {
    interaction_term <- paste0(moderator, ":", treatment_var)
  }

  if (interaction_term %in% rownames(coef_summary)) {
    interaction_idx <- which(rownames(coef_summary) == interaction_term)

    interaction_result <- list(
      estimate = coef_summary[interaction_idx, "Estimate"],
      se = coef_summary[interaction_idx, "Std. Error"],
      t_stat = coef_summary[interaction_idx, "t value"],
      p_value = coef_summary[interaction_idx, 4],
      interpretation = if (coef_summary[interaction_idx, 4] < 0.05)
        sprintf("Treatment effect varies significantly by %s (p < 0.05)", moderator)
      else
        sprintf("No evidence that treatment effect varies by %s", moderator)
    )
  } else {
    interaction_result <- list(error = "Interaction term not found in model")
  }

  result <- list(
    interaction = interaction_result,
    model = model,
    moderator = moderator,
    formula = formula_obj
  )

  class(result) <- "ipd_interaction"
  return(result)
}


#' Missing Data Imputation for IPD Meta-Analysis
#'
#' Performs multiple imputation for missing data in IPD meta-analysis using
#' a simplified chained equations approach.
#'
#' @param data IPD data frame with missing values
#' @param n_imputations Number of imputations (default: 5)
#' @param seed Random seed
#'
#' @return List of imputed datasets
#'
#' @export
ipd_impute_missing <- function(data, n_imputations = 5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Identify columns with missing data
  missing_cols <- names(data)[colSums(is.na(data)) > 0]

  if (length(missing_cols) == 0) {
    return(list(data))  # No missing data
  }

  # Create list to store imputed datasets
  imputed_datasets <- vector("list", n_imputations)

  for (m in 1:n_imputations) {
    imputed_data <- data

    # Simple imputation for each variable with missing data
    for (col in missing_cols) {
      if (is.numeric(data[[col]])) {
        # Predictive mean matching (simplified)
        complete_data <- data[!is.na(data[[col]]), ]
        missing_idx <- which(is.na(data[[col]]))

        # Predict using other variables
        pred_formula <- as.formula(paste(col, "~ ."))
        pred_model <- lm(pred_formula, data = complete_data[, !names(complete_data) %in% c("study")])

        # Predict for missing
        predicted <- predict(pred_model, newdata = data[missing_idx, ])

        # Add random noise
        residual_sd <- sd(pred_model$residuals)
        imputed_values <- predicted + rnorm(length(predicted), 0, residual_sd)

        imputed_data[missing_idx, col] <- imputed_values
      } else {
        # For categorical: sample from observed distribution
        imputed_data[[col]][is.na(imputed_data[[col]])] <-
          sample(na.omit(data[[col]]), sum(is.na(data[[col]])), replace = TRUE)
      }
    }

    imputed_datasets[[m]] <- imputed_data
  }

  return(imputed_datasets)
}
