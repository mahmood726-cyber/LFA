#' Publication Bias Assessment Methods
#'
#' State-of-the-art methods for detecting and adjusting for publication bias
#' based on recent methodological research (2024-2025)
#'
#' @name publication_bias
NULL

#' Egger's Regression Test for Publication Bias
#'
#' Tests for funnel plot asymmetry using regression of standardized effect
#' sizes on precision (Egger et al., 1997). In the absence of publication
#' bias, the intercept is expected to be zero.
#'
#' @param data Data frame with effect and se columns
#' @param alpha Significance level (default: 0.05)
#'
#' @return List containing:
#'   \itemize{
#'     \item intercept: Regression intercept (bias estimate)
#'     \item se: Standard error of intercept
#'     \item t_stat: t-statistic
#'     \item p_value: P-value for test of intercept = 0
#'     \item ci_lower: Lower 95% CI for intercept
#'     \item ci_upper: Upper 95% CI for intercept
#'     \item significant: Logical indicating if bias is detected
#'   }
#'
#' @references
#' Egger M, Davey Smith G, Schneider M, Minder C (1997). Bias in meta-analysis
#' detected by a simple, graphical test. BMJ, 315(7109):629-634.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' egger_result <- egger_test(example_meta)
#' print(egger_result)
#' }
egger_test <- function(data, alpha = 0.05) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for Egger's test")
  }

  # Standardized effect sizes
  yi <- data$effect
  sei <- data$se
  zi <- yi / sei

  # Precision (1/se)
  prec <- 1 / sei

  # Regression of z on precision
  fit <- lm(zi ~ prec)
  coef_summary <- summary(fit)$coefficients

  intercept <- coef_summary[1, "Estimate"]
  se_intercept <- coef_summary[1, "Std. Error"]
  t_stat <- coef_summary[1, "t value"]
  p_value <- coef_summary[1, "Pr(>|t|)"]

  # Confidence interval
  df <- k - 2
  t_crit <- qt(1 - alpha/2, df)
  ci_lower <- intercept - t_crit * se_intercept
  ci_upper <- intercept + t_crit * se_intercept

  result <- list(
    test = "Egger's regression test",
    intercept = intercept,
    se = se_intercept,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    significant = p_value < alpha,
    k = k,
    interpretation = if (p_value < alpha) {
      "Significant funnel plot asymmetry detected (p < 0.05)"
    } else {
      "No significant funnel plot asymmetry detected"
    }
  )

  class(result) <- "egger_test"
  return(result)
}


#' PET-PEESE Publication Bias Correction
#'
#' Precision-Effect Test (PET) and Precision-Effect Estimate with Standard Error
#' (PEESE) methods for detecting and correcting publication bias (Stanley & Doucouliagos, 2014).
#' PET works best when true effect is zero; PEESE when true effect is non-zero.
#'
#' @param data Data frame with effect and se columns
#' @param method Method to use: "conditional" (default, recommended), "PET", or "PEESE"
#' @param alpha Significance level for PET test (default: 0.10)
#'
#' @return List containing:
#'   \itemize{
#'     \item method_used: Which method was applied
#'     \item estimate: Bias-corrected effect estimate
#'     \item se: Standard error of corrected estimate
#'     \item ci_lower: Lower 95% CI
#'     \item ci_upper: Upper 95% CI
#'     \item p_value: P-value for corrected estimate
#'     \item pet_result: Results from PET regression
#'     \item peese_result: Results from PEESE regression (if applicable)
#'   }
#'
#' @references
#' Stanley TD, Doucouliagos H (2014). Meta-regression approximations to reduce
#' publication selection bias. Research Synthesis Methods, 5(1):60-78.
#'
#' Bartoš F, Maier M, Quintana DS, Wagenmakers EJ (2022). Adjusting for
#' publication bias in JASP and R. Advances in Methods and Practices in
#' Psychological Science, 5(3).
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' pet_peese <- pet_peese(example_meta)
#' print(pet_peese)
#' }
pet_peese <- function(data, method = "conditional", alpha = 0.10) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  if (!method %in% c("conditional", "PET", "PEESE")) {
    stop("method must be 'conditional', 'PET', or 'PEESE'")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for PET-PEESE")
  }

  yi <- data$effect
  sei <- data$se
  vari <- sei^2

  # PET: Regress effect on standard error
  pet_fit <- lm(yi ~ sei)
  pet_summary <- summary(pet_fit)
  pet_intercept <- coef(pet_fit)[1]
  pet_se <- pet_summary$coefficients[1, "Std. Error"]
  pet_p <- pet_summary$coefficients[1, "Pr(>|t|)"]

  # PET confidence interval
  df <- k - 2
  t_crit <- qt(0.975, df)
  pet_ci_lower <- pet_intercept - t_crit * pet_se
  pet_ci_upper <- pet_intercept + t_crit * pet_se

  pet_result <- list(
    estimate = pet_intercept,
    se = pet_se,
    ci_lower = pet_ci_lower,
    ci_upper = pet_ci_upper,
    p_value = pet_p,
    t_stat = pet_summary$coefficients[1, "t value"]
  )

  # PEESE: Regress effect on variance
  peese_fit <- lm(yi ~ vari)
  peese_summary <- summary(peese_fit)
  peese_intercept <- coef(peese_fit)[1]
  peese_se <- peese_summary$coefficients[1, "Std. Error"]
  peese_p <- peese_summary$coefficients[1, "Pr(>|t|)"]

  # PEESE confidence interval
  peese_ci_lower <- peese_intercept - t_crit * peese_se
  peese_ci_upper <- peese_intercept + t_crit * peese_se

  peese_result <- list(
    estimate = peese_intercept,
    se = peese_se,
    ci_lower = peese_ci_lower,
    ci_upper = peese_ci_upper,
    p_value = peese_p,
    t_stat = peese_summary$coefficients[1, "t value"]
  )

  # Conditional approach (recommended by Stanley & Doucouliagos)
  if (method == "conditional") {
    # Use PET if not significant (suggests true effect near zero)
    # Use PEESE if significant (suggests true effect non-zero)
    if (pet_p > alpha) {
      method_used <- "PET"
      final_estimate <- pet_intercept
      final_se <- pet_se
      final_ci_lower <- pet_ci_lower
      final_ci_upper <- pet_ci_upper
      final_p <- pet_p
      reasoning <- sprintf("PET used (PET p-value = %.3f > %.2f)", pet_p, alpha)
    } else {
      method_used <- "PEESE"
      final_estimate <- peese_intercept
      final_se <- peese_se
      final_ci_lower <- peese_ci_lower
      final_ci_upper <- peese_ci_upper
      final_p <- peese_p
      reasoning <- sprintf("PEESE used (PET p-value = %.3f < %.2f)", pet_p, alpha)
    }
  } else if (method == "PET") {
    method_used <- "PET"
    final_estimate <- pet_intercept
    final_se <- pet_se
    final_ci_lower <- pet_ci_lower
    final_ci_upper <- pet_ci_upper
    final_p <- pet_p
    reasoning <- "PET specified by user"
  } else {
    method_used <- "PEESE"
    final_estimate <- peese_intercept
    final_se <- peese_se
    final_ci_lower <- peese_ci_lower
    final_ci_upper <- peese_ci_upper
    final_p <- peese_p
    reasoning <- "PEESE specified by user"
  }

  result <- list(
    method_used = method_used,
    estimate = final_estimate,
    se = final_se,
    ci_lower = final_ci_lower,
    ci_upper = final_ci_upper,
    p_value = final_p,
    pet_result = pet_result,
    peese_result = peese_result,
    k = k,
    reasoning = reasoning,
    note = "PET-PEESE adjusts for publication bias; interpret with caution"
  )

  class(result) <- "pet_peese"
  return(result)
}


#' Trim and Fill Method for Publication Bias
#'
#' Implements the trim and fill method (Duval & Tweedie, 2000) to estimate
#' and adjust for the number of missing studies due to publication bias.
#'
#' @param data Data frame with effect and se columns
#' @param side Side of funnel plot to examine: "auto" (default), "left", or "right"
#' @param estimator Estimator for number of missing studies: "L0" (default) or "R0"
#'
#' @return List containing:
#'   \itemize{
#'     \item k0: Estimated number of missing studies
#'     \item side: Side where studies are missing
#'     \item filled_estimate: Pooled estimate after filling
#'     \item filled_se: Standard error after filling
#'     \item filled_ci_lower: Lower CI after filling
#'     \item filled_ci_upper: Upper CI after filling
#'     \item original_estimate: Original pooled estimate
#'     \item filled_data: Data frame with imputed studies added
#'   }
#'
#' @references
#' Duval S, Tweedie R (2000). Trim and fill: A simple funnel-plot-based method
#' of testing and adjusting for publication bias in meta-analysis. Biometrics,
#' 56(2):455-463.
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' tf_result <- trim_fill(example_meta)
#' print(tf_result)
#' }
trim_fill <- function(data, side = "auto", estimator = "L0") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!"effect" %in% names(data) || !"se" %in% names(data)) {
    stop("data must contain 'effect' and 'se' columns")
  }

  k <- nrow(data)
  if (k < 3) {
    stop("Need at least 3 studies for trim and fill")
  }

  # Original meta-analysis
  original_ma <- cbamm_fast(data, verbose = FALSE)
  original_estimate <- original_ma$estimate

  yi <- data$effect
  sei <- data$se
  wi <- 1 / sei^2

  # Determine side
  if (side == "auto") {
    # Use sign of Egger's intercept to determine side
    tryCatch({
      egger_res <- egger_test(data)
      side <- if (egger_res$intercept < 0) "left" else "right"
    }, error = function(e) {
      # Fallback to checking asymmetry
      side <<- "right"
    })
  }

  # Rank studies by standardized effect size
  zi <- (yi - original_estimate) / sei
  ranks <- rank(abs(zi))

  # Estimate number of missing studies (simplified L0 estimator)
  if (estimator == "L0") {
    # Count studies on the opposite side
    if (side == "right") {
      n_extreme <- sum(yi > original_estimate)
      k0 <- max(0, (2 * n_extreme) - k)
    } else {
      n_extreme <- sum(yi < original_estimate)
      k0 <- max(0, (2 * n_extreme) - k)
    }
  } else {
    # R0 estimator (simplified)
    k0 <- max(0, floor((k - 1) / 2))
  }

  k0 <- round(k0)

  if (k0 == 0) {
    # No adjustment needed
    result <- list(
      k0 = 0,
      side = side,
      filled_estimate = original_estimate,
      filled_se = original_ma$se,
      filled_ci_lower = original_ma$ci_lower,
      filled_ci_upper = original_ma$ci_upper,
      original_estimate = original_estimate,
      original_ci = c(original_ma$ci_lower, original_ma$ci_upper),
      filled_data = data,
      note = "No missing studies detected"
    )
  } else {
    # Create imputed studies
    # Find the k0 most extreme studies
    extreme_indices <- order(abs(yi - original_estimate), decreasing = TRUE)[1:min(k0, k)]

    # Mirror these studies
    imputed_yi <- 2 * original_estimate - yi[extreme_indices]
    imputed_sei <- sei[extreme_indices]

    # Combine with original data
    filled_data <- rbind(
      data,
      data.frame(
        study = paste0("Imputed_", 1:k0),
        effect = imputed_yi,
        se = imputed_sei
      )
    )

    # Re-run meta-analysis
    filled_ma <- cbamm_fast(filled_data, verbose = FALSE)

    result <- list(
      k0 = k0,
      side = side,
      filled_estimate = filled_ma$estimate,
      filled_se = filled_ma$se,
      filled_ci_lower = filled_ma$ci_lower,
      filled_ci_upper = filled_ma$ci_upper,
      original_estimate = original_estimate,
      original_ci = c(original_ma$ci_lower, original_ma$ci_upper),
      filled_data = filled_data,
      imputed_studies = data.frame(
        effect = imputed_yi,
        se = imputed_sei
      ),
      note = sprintf("Added %d imputed studies on the %s side", k0, side)
    )
  }

  class(result) <- "trim_fill"
  return(result)
}


#' Comprehensive Publication Bias Assessment
#'
#' Runs multiple publication bias tests and provides a comprehensive assessment.
#' Based on 2024 methodological recommendations.
#'
#' @param data Data frame with effect and se columns
#' @param methods Character vector of methods to use. Options: "egger", "pet_peese",
#'   "trim_fill", "all" (default)
#'
#' @return List containing results from all requested methods
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' bias_assessment <- assess_publication_bias(example_meta)
#' print(bias_assessment)
#' }
assess_publication_bias <- function(data, methods = "all") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if ("all" %in% methods) {
    methods <- c("egger", "pet_peese", "trim_fill")
  }

  results <- list()

  # Original meta-analysis for comparison
  original_ma <- cbamm_fast(data, verbose = FALSE)
  results$original <- list(
    estimate = original_ma$estimate,
    ci_lower = original_ma$ci_lower,
    ci_upper = original_ma$ci_upper,
    p_value = original_ma$p_value
  )

  # Run requested methods
  if ("egger" %in% methods) {
    tryCatch({
      results$egger <- egger_test(data)
    }, error = function(e) {
      results$egger <<- list(error = e$message)
    })
  }

  if ("pet_peese" %in% methods) {
    tryCatch({
      results$pet_peese <- pet_peese(data, method = "conditional")
    }, error = function(e) {
      results$pet_peese <<- list(error = e$message)
    })
  }

  if ("trim_fill" %in% methods) {
    tryCatch({
      results$trim_fill <- trim_fill(data)
    }, error = function(e) {
      results$trim_fill <<- list(error = e$message)
    })
  }

  # Summary
  results$summary <- list(
    k = nrow(data),
    note = paste(
      "Multiple publication bias methods applied.",
      "Interpret cautiously - no method is perfect.",
      "PET-PEESE generally performs best according to 2024 research."
    )
  )

  class(results) <- "bias_assessment"
  return(results)
}
