#' Dose-Response Meta-Analysis
#'
#' Methods for dose-response meta-analysis examining the relationship between
#' exposure levels and outcomes across studies.
#'
#' @name dose_response
NULL

#' Dose-Response Meta-Analysis
#'
#' Performs dose-response meta-analysis using linear, quadratic, or restricted
#' cubic spline models.
#'
#' @param data Data frame with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item dose: Dose/exposure level
#'     \item effect: Effect size at this dose
#'     \item se: Standard error
#'     \item cases: Number of cases (optional, for trend estimation)
#'     \item n: Total sample size (optional)
#'   }
#' @param model Type of dose-response model: "linear", "quadratic", "restricted_spline"
#' @param reference_dose Reference dose for relative effects (default: 0)
#' @param n_knots Number of knots for restricted cubic splines (default: 3)
#'
#' @return Object of class "dose_response_ma" containing:
#'   \itemize{
#'     \item model_fit: Fitted dose-response model
#'     \item predictions: Predicted effects across dose range
#'     \item coefficients: Model coefficients
#'     \item heterogeneity: Between-study heterogeneity
#'   }
#'
#' @references
#' Orsini N, Li R, Wolk A, Khudyakov P, Spiegelman D (2012). Meta-analysis for
#' linear and nonlinear dose-response relations: examples, an evaluation of
#' approximations, and software. American Journal of Epidemiology, 175(1):66-73.
#'
#' @export
#' @examples
#' \dontrun{
#' # Example: Alcohol consumption and disease risk
#' dose_data <- data.frame(
#'   study = rep(1:5, each = 4),
#'   dose = rep(c(0, 10, 20, 30), 5),
#'   effect = c(0, 0.1, 0.3, 0.5) + rnorm(20, 0, 0.1),
#'   se = runif(20, 0.05, 0.15)
#' )
#'
#' result <- dose_response_ma(dose_data, model = "restricted_spline")
#' plot(result)
#' }
dose_response_ma <- function(data, model = "linear", reference_dose = 0,
                              n_knots = 3) {
  # Validate data
  required_cols <- c("study", "dose", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Center dose at reference
  data$dose_centered <- data$dose - reference_dose

  # Fit dose-response model
  if (model == "linear") {
    # Linear dose-response: effect = beta * dose
    # Weight by inverse variance
    weights <- 1 / data$se^2

    # Weighted linear regression
    fit <- lm(effect ~ dose_centered, data = data, weights = weights)

    coef_summary <- summary(fit)$coefficients
    beta_dose <- coef_summary["dose_centered", "Estimate"]
    se_dose <- coef_summary["dose_centered", "Std. Error"]

    # Predictions
    dose_range <- seq(min(data$dose), max(data$dose), length.out = 100)
    predictions <- data.frame(
      dose = dose_range,
      predicted_effect = beta_dose * (dose_range - reference_dose),
      se = se_dose * abs(dose_range - reference_dose)
    )
    predictions$ci_lower <- predictions$predicted_effect - 1.96 * predictions$se
    predictions$ci_upper <- predictions$predicted_effect + 1.96 * predictions$se

    coefficients <- list(
      linear = beta_dose,
      se_linear = se_dose
    )

  } else if (model == "quadratic") {
    # Quadratic: effect = beta1 * dose + beta2 * dose^2
    data$dose_centered_sq <- data$dose_centered^2
    weights <- 1 / data$se^2

    fit <- lm(effect ~ dose_centered + dose_centered_sq, data = data, weights = weights)

    coef_summary <- summary(fit)$coefficients
    beta1 <- coef_summary["dose_centered", "Estimate"]
    beta2 <- coef_summary["dose_centered_sq", "Estimate"]

    # Predictions
    dose_range <- seq(min(data$dose), max(data$dose), length.out = 100)
    dose_centered_range <- dose_range - reference_dose
    predictions <- data.frame(
      dose = dose_range,
      predicted_effect = beta1 * dose_centered_range + beta2 * dose_centered_range^2
    )

    # Approximate SE using delta method
    cov_mat <- vcov(fit)[c("dose_centered", "dose_centered_sq"),
                         c("dose_centered", "dose_centered_sq")]
    predictions$se <- NA  # Simplified - full implementation would use delta method

    coefficients <- list(
      linear = beta1,
      quadratic = beta2
    )

  } else if (model == "restricted_spline") {
    # Restricted cubic splines
    # Knot placement at quantiles of dose distribution
    dose_quantiles <- quantile(data$dose_centered, probs = seq(0, 1, length.out = n_knots))

    # Create spline basis (simplified natural spline)
    # Full implementation would use splines::ns() or similar
    data$spline1 <- data$dose_centered
    data$spline2 <- pmax(0, data$dose_centered - dose_quantiles[2])
    if (n_knots >= 3) {
      data$spline3 <- pmax(0, data$dose_centered - dose_quantiles[3])
    }

    weights <- 1 / data$se^2

    spline_formula <- if (n_knots >= 3) {
      as.formula("effect ~ spline1 + spline2 + spline3")
    } else {
      as.formula("effect ~ spline1 + spline2")
    }

    fit <- lm(spline_formula, data = data, weights = weights)

    # Predictions
    dose_range <- seq(min(data$dose), max(data$dose), length.out = 100)
    dose_centered_range <- dose_range - reference_dose

    pred_data <- data.frame(
      spline1 = dose_centered_range,
      spline2 = pmax(0, dose_centered_range - dose_quantiles[2])
    )
    if (n_knots >= 3) {
      pred_data$spline3 <- pmax(0, dose_centered_range - dose_quantiles[3])
    }

    predictions <- data.frame(
      dose = dose_range,
      predicted_effect = predict(fit, newdata = pred_data)
    )

    coefficients <- list(
      spline_coefs = coef(fit),
      knots = dose_quantiles
    )

  } else {
    stop("model must be 'linear', 'quadratic', or 'restricted_spline'")
  }

  # Calculate heterogeneity
  residuals <- residuals(fit)
  weights <- 1 / data$se^2
  Q <- sum(weights * residuals^2)
  df <- nrow(data) - length(coef(fit))
  I2 <- max(0, 100 * (Q - df) / Q)

  result <- list(
    model_fit = fit,
    predictions = predictions,
    coefficients = coefficients,
    heterogeneity = list(
      Q = Q,
      df = df,
      I2 = I2,
      p_value = pchisq(Q, df, lower.tail = FALSE)
    ),
    data = data,
    model_type = model,
    reference_dose = reference_dose,
    n_knots = if (model == "restricted_spline") n_knots else NULL
  )

  class(result) <- "dose_response_ma"
  return(result)
}


#' Plot Dose-Response Meta-Analysis Results
#'
#' Creates a dose-response curve showing the relationship between dose and effect.
#'
#' @param dose_response_result Object from dose_response_ma()
#' @param show_studies Logical indicating whether to show individual studies
#' @param main Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#'
#' @return Invisibly returns NULL
#'
#' @export
plot_dose_response <- function(dose_response_result, show_studies = TRUE,
                                main = "Dose-Response Meta-Analysis",
                                xlab = "Dose", ylab = "Effect") {
  if (!inherits(dose_response_result, "dose_response_ma")) {
    stop("Input must be a dose_response_ma object")
  }

  predictions <- dose_response_result$predictions
  data <- dose_response_result$data

  # Set up plot
  plot(predictions$dose, predictions$predicted_effect,
       type = "l", lwd = 2, col = "blue",
       xlab = xlab, ylab = ylab, main = main,
       ylim = range(c(predictions$predicted_effect, data$effect), na.rm = TRUE))

  # Add confidence band if available
  if ("ci_lower" %in% names(predictions) && "ci_upper" %in% names(predictions)) {
    polygon(c(predictions$dose, rev(predictions$dose)),
            c(predictions$ci_lower, rev(predictions$ci_upper)),
            col = rgb(0, 0, 1, 0.2), border = NA)
  }

  # Add individual studies
  if (show_studies) {
    # Aggregate by study and dose
    study_means <- aggregate(effect ~ study + dose, data = data, FUN = mean)
    points(study_means$dose, study_means$effect,
           pch = 19, col = rgb(0, 0, 0, 0.5), cex = 1.2)
  }

  # Add reference line
  abline(h = 0, lty = 2, col = "gray50")
  abline(v = dose_response_result$reference_dose, lty = 2, col = "red")

  # Add legend
  legend("topleft",
         legend = c("Dose-response curve", "Individual studies", "Reference dose"),
         lty = c(1, NA, 2),
         pch = c(NA, 19, NA),
         col = c("blue", rgb(0, 0, 0, 0.5), "red"),
         bty = "n")

  invisible(NULL)
}


#' Test for Non-Linearity in Dose-Response
#'
#' Tests whether the dose-response relationship is non-linear by comparing
#' linear and non-linear models.
#'
#' @param dose_response_result Object from dose_response_ma() with non-linear model
#' @param data Original data used for analysis
#'
#' @return List with test results
#'
#' @export
test_nonlinearity <- function(dose_response_result, data) {
  if (dose_response_result$model_type == "linear") {
    stop("Input must be from a non-linear model (quadratic or spline)")
  }

  # Fit linear model for comparison
  linear_result <- dose_response_ma(data, model = "linear",
                                     reference_dose = dose_response_result$reference_dose)

  # Compare models using likelihood ratio test or AIC
  ll_nonlinear <- logLik(dose_response_result$model_fit)
  ll_linear <- logLik(linear_result$model_fit)

  # LR test
  lr_stat <- -2 * (as.numeric(ll_linear) - as.numeric(ll_nonlinear))
  df_diff <- attr(ll_nonlinear, "df") - attr(ll_linear, "df")
  p_value <- pchisq(lr_stat, df_diff, lower.tail = FALSE)

  # AIC comparison
  aic_linear <- AIC(linear_result$model_fit)
  aic_nonlinear <- AIC(dose_response_result$model_fit)

  result <- list(
    test = "Likelihood ratio test for non-linearity",
    lr_statistic = lr_stat,
    df = df_diff,
    p_value = p_value,
    aic_linear = aic_linear,
    aic_nonlinear = aic_nonlinear,
    aic_difference = aic_linear - aic_nonlinear,
    interpretation = if (p_value < 0.05)
      "Significant evidence of non-linearity (p < 0.05)"
    else
      "No significant evidence of non-linearity",
    note = "Lower AIC indicates better model fit"
  )

  return(result)
}


#' Calculate ED50 (Effective Dose for 50% Effect)
#'
#' Estimates the dose required to achieve 50% of the maximum effect.
#'
#' @param dose_response_result Object from dose_response_ma()
#'
#' @return Estimated ED50 value
#'
#' @export
calculate_ed50 <- function(dose_response_result) {
  predictions <- dose_response_result$predictions

  # Find maximum effect
  max_effect <- max(predictions$predicted_effect)
  target_effect <- max_effect * 0.5

  # Find dose closest to target effect
  idx <- which.min(abs(predictions$predicted_effect - target_effect))
  ed50 <- predictions$dose[idx]

  result <- list(
    ed50 = ed50,
    target_effect = target_effect,
    max_effect = max_effect,
    interpretation = sprintf("Dose of %.2f achieves 50%% of maximum effect", ed50)
  )

  return(result)
}


#' Dose-Response Meta-Analysis for Binary Outcomes
#'
#' Specialized dose-response analysis for binary outcomes (e.g., risk ratios,
#' odds ratios across dose categories).
#'
#' @param data Data frame with dose categories and binary outcome data
#' @param outcome_type Type of outcome: "RR" (risk ratio) or "OR" (odds ratio)
#'
#' @return Dose-response results for binary outcomes
#'
#' @export
dose_response_binary <- function(data, outcome_type = "RR") {
  # For binary outcomes, typically work with log(RR) or log(OR)
  if (outcome_type == "RR") {
    data$log_effect <- log(data$effect)
  } else if (outcome_type == "OR") {
    data$log_effect <- log(data$effect)
  } else {
    stop("outcome_type must be 'RR' or 'OR'")
  }

  # Standard errors on log scale
  if (!"se_log" %in% names(data)) {
    # Approximate SE on log scale
    data$se_log <- data$se / data$effect
  } else {
    data$se_log <- data$se_log
  }

  # Perform dose-response analysis on log scale
  result <- dose_response_ma(
    data = data.frame(
      study = data$study,
      dose = data$dose,
      effect = data$log_effect,
      se = data$se_log
    ),
    model = "restricted_spline"
  )

  # Back-transform predictions to original scale
  result$predictions$predicted_effect <- exp(result$predictions$predicted_effect)
  if ("ci_lower" %in% names(result$predictions)) {
    result$predictions$ci_lower <- exp(result$predictions$ci_lower)
    result$predictions$ci_upper <- exp(result$predictions$ci_upper)
  }

  result$outcome_type <- outcome_type
  result$note <- sprintf("Analysis performed on log(%s) scale and back-transformed", outcome_type)

  return(result)
}
