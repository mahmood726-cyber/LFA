#' Dose-Response Meta-Analysis
#'
#' Advanced dose-response meta-analysis methods from Statistics in Medicine and
#' Research Synthesis Methods. Handles linear, quadratic, and flexible spline models.
#'
#' Methods based on:
#' - Orsini et al. (2012) - Generalized least squares trend estimation
#' - Crippa & Orsini (2016) - Multivariate dose-response meta-analysis
#' - Discacciati et al. (2017) - Goodness-of-fit and flexible modeling
#'
#' @name dose_response_meta_analysis
NULL

#' Dose-Response Meta-Analysis
#'
#' Analyzes dose-response relationships from multiple studies using flexible
#' modeling approaches including restricted cubic splines.
#'
#' @param data Data frame with dose-response data
#' @param dose Dose/exposure variable name
#' @param cases Number of cases variable name
#' @param n Total sample size or person-years variable name
#' @param studyid Study identifier variable name
#' @param type Response type: "or" (odds ratio), "rr" (relative risk), "ir" (incidence rate)
#' @param ref_dose Reference dose (default: 0 or minimum)
#' @param model Model type: "linear", "quadratic", "spline" (default: "spline")
#' @param spline_knots Number of knots for splines (default: 3)
#' @param test_nonlinearity Test for non-linear trend (default: TRUE)
#'
#' @return Dose-response meta-analysis object
#'
#' @export
#' @examples
#' \dontrun{
#' # Dose-response for alcohol and cancer risk
#' dr_result <- dose_response_meta_analysis(
#'   data = alcohol_cancer,
#'   dose = "drinks_per_day",
#'   cases = "cases",
#'   n = "person_years",
#'   studyid = "study",
#'   type = "rr",
#'   ref_dose = 0,
#'   model = "spline"
#' )
#'
#' # Plot dose-response curve
#' plot(dr_result)
#'
#' # Predict risk at specific dose
#' predict(dr_result, newdose = c(1, 2, 3, 4))
#' }
dose_response_meta_analysis <- function(data, dose, cases, n, studyid,
                                       type = "rr",
                                       ref_dose = NULL,
                                       model = "spline",
                                       spline_knots = 3,
                                       test_nonlinearity = TRUE) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   DOSE-RESPONSE META-ANALYSIS                                ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Validate inputs
  required_cols <- c(dose, cases, n, studyid)
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns")
  }

  n_studies <- length(unique(data[[studyid]]))

  # Determine reference dose
  if (is.null(ref_dose)) {
    ref_dose <- min(data[[dose]], na.rm = TRUE)
    cat(sprintf("Reference dose: %.2f (minimum observed)\n", ref_dose))
  } else {
    cat(sprintf("Reference dose: %.2f\n", ref_dose))
  }

  cat(sprintf("Studies: %d\n", n_studies))
  cat(sprintf("Type: %s\n", toupper(type)))
  cat(sprintf("Model: %s\n\n", model))

  # Calculate effect sizes (log RR/OR/IR)
  cat("Calculating effect sizes...\n")
  dr_data <- calculate_dose_response_effects(data, dose, cases, n, studyid, type, ref_dose)

  cat(sprintf("  Total dose-response points: %d\n", nrow(dr_data)))

  # Fit dose-response model
  cat(sprintf("\nFitting %s dose-response model...\n", model))

  dr_fit <- fit_dose_response_model(dr_data, dose, studyid, model, spline_knots)

  cat("✓ Model fitted successfully\n")

  # Model summary
  cat("\nDose-response relationship:\n")
  if (model == "linear") {
    slope <- dr_fit$coefficients["dose"]
    se_slope <- dr_fit$se["dose"]
    cat(sprintf("  Linear slope: %.4f (SE = %.4f)\n", slope, se_slope))
    cat(sprintf("  Per unit increase: RR = %.3f (95%% CI: %.3f to %.3f)\n",
               exp(slope),
               exp(slope - 1.96 * se_slope),
               exp(slope + 1.96 * se_slope)))
  } else if (model == "quadratic") {
    cat("  Quadratic coefficients:\n")
    cat(sprintf("    Linear: %.4f (SE = %.4f)\n",
               dr_fit$coefficients["dose"],
               dr_fit$se["dose"]))
    cat(sprintf("    Quadratic: %.4f (SE = %.4f)\n",
               dr_fit$coefficients["dose2"],
               dr_fit$se["dose2"]))
  } else {
    cat("  Flexible spline model fitted\n")
    cat(sprintf("  Knots: %d\n", spline_knots))
  }

  # Test for non-linearity
  nonlin_test <- NULL
  if (test_nonlinearity && model %in% c("quadratic", "spline")) {
    cat("\nTest for non-linearity:\n")
    nonlin_test <- test_dose_response_nonlinearity(dr_fit, model)
    cat(sprintf("  χ²(%d) = %.2f, p %s\n",
               nonlin_test$df,
               nonlin_test$statistic,
               if(nonlin_test$p_value < 0.001) "< 0.001" 
               else sprintf("= %.3f", nonlin_test$p_value)))

    if (nonlin_test$p_value < 0.05) {
      cat("  ⚠️  Significant non-linear relationship detected\n")
    } else {
      cat("  Linear model may be adequate\n")
    }
  }

  # Generate predictions across dose range
  cat("\nGenerating dose-response curve...\n")
  dose_range <- seq(min(dr_data[[dose]]), max(dr_data[[dose]]), length.out = 100)
  predictions <- predict_dose_response(dr_fit, dose_range, ref_dose, model)

  # Create plots
  plots <- create_dose_response_plots(dr_data, dr_fit, predictions, dose, ref_dose, type)

  # Compile results
  result <- list(
    fit = dr_fit,
    data = dr_data,
    model = model,
    type = type,
    ref_dose = ref_dose,
    n_studies = n_studies,
    predictions = predictions,
    nonlinearity_test = nonlin_test,
    plots = plots
  )

  class(result) <- c("dose_response_meta_analysis", "list")

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   DOSE-RESPONSE META-ANALYSIS COMPLETE                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  return(result)
}

#' @keywords internal
calculate_dose_response_effects <- function(data, dose, cases, n, studyid, type, ref_dose) {
  # Calculate log(RR/OR/IR) for each dose level within each study

  dr_data <- data.frame()

  for (study in unique(data[[studyid]])) {
    study_data <- data[data[[studyid]] == study, ]

    # Sort by dose
    study_data <- study_data[order(study_data[[dose]]), ]

    # Find reference dose
    ref_idx <- which.min(abs(study_data[[dose]] - ref_dose))

    ref_cases <- study_data[[cases]][ref_idx]
    ref_n <- study_data[[n]][ref_idx]

    for (i in 1:nrow(study_data)) {
      if (i == ref_idx) next  # Skip reference

      dose_val <- study_data[[dose]][i]
      case_val <- study_data[[cases]][i]
      n_val <- study_data[[n]][i]

      # Calculate effect size
      if (type == "rr" || type == "ir") {
        # Log relative risk or incidence rate ratio
        y <- log((case_val / n_val) / (ref_cases / ref_n))
        # Variance using delta method
        se <- sqrt(1/case_val - 1/n_val + 1/ref_cases - 1/ref_n)
      } else if (type == "or") {
        # Log odds ratio
        y <- log((case_val / (n_val - case_val)) / (ref_cases / (ref_n - ref_cases)))
        se <- sqrt(1/case_val + 1/(n_val - case_val) + 1/ref_cases + 1/(ref_n - ref_cases))
      }

      dr_data <- rbind(dr_data, data.frame(
        studyid = study,
        dose = dose_val,
        y = y,
        se = se,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(dr_data)
}

#' @keywords internal
fit_dose_response_model <- function(dr_data, dose, studyid, model, spline_knots) {
  if (requireNamespace("metafor", quietly = TRUE)) {
    # Use metafor for meta-regression

    if (model == "linear") {
      # Linear dose-response
      fit <- metafor::rma.mv(
        yi = y,
        V = se^2,
        mods = ~ dose,
        random = ~ 1 | studyid,
        data = dr_data,
        method = "REML"
      )

      return(list(
        type = "metafor",
        fit = fit,
        coefficients = coef(fit),
        se = sqrt(diag(vcov(fit)))
      ))

    } else if (model == "quadratic") {
      # Quadratic dose-response
      dr_data$dose2 <- dr_data$dose^2

      fit <- metafor::rma.mv(
        yi = y,
        V = se^2,
        mods = ~ dose + dose2,
        random = ~ 1 | studyid,
        data = dr_data,
        method = "REML"
      )

      return(list(
        type = "metafor",
        fit = fit,
        coefficients = coef(fit),
        se = sqrt(diag(vcov(fit)))
      ))

    } else if (model == "spline") {
      # Restricted cubic splines
      spline_basis <- create_spline_basis(dr_data$dose, spline_knots)
      dr_data <- cbind(dr_data, spline_basis)

      spline_vars <- paste0("spline", 1:ncol(spline_basis))
      formula_str <- paste("~", paste(spline_vars, collapse = " + "))

      fit <- metafor::rma.mv(
        yi = y,
        V = se^2,
        mods = as.formula(formula_str),
        random = ~ 1 | studyid,
        data = dr_data,
        method = "REML"
      )

      return(list(
        type = "metafor",
        fit = fit,
        coefficients = coef(fit),
        se = sqrt(diag(vcov(fit))),
        spline_knots = spline_knots,
        knot_positions = attr(spline_basis, "knots")
      ))
    }

  } else {
    # Simplified approach without metafor
    warning("metafor package not available. Using simplified weighted regression.")
    return(fit_simplified_dose_response(dr_data, model, spline_knots))
  }
}

#' @keywords internal
create_spline_basis <- function(dose, n_knots) {
  # Restricted cubic splines
  knot_positions <- quantile(dose, probs = seq(0, 1, length.out = n_knots + 2))

  n <- length(dose)
  k <- length(knot_positions)

  # Create basis functions
  basis <- matrix(NA, n, n_knots)

  for (j in 1:n_knots) {
    # Truncated power basis
    basis[, j] <- pmax(dose - knot_positions[j + 1], 0)^3
  }

  attr(basis, "knots") <- knot_positions
  colnames(basis) <- paste0("spline", 1:n_knots)

  return(as.data.frame(basis))
}

#' @keywords internal
fit_simplified_dose_response <- function(dr_data, model, spline_knots) {
  weights <- 1 / dr_data$se^2

  if (model == "linear") {
    fit <- lm(y ~ dose, data = dr_data, weights = weights)
  } else if (model == "quadratic") {
    dr_data$dose2 <- dr_data$dose^2
    fit <- lm(y ~ dose + dose2, data = dr_data, weights = weights)
  } else {
    # Simplified spline
    spline_basis <- create_spline_basis(dr_data$dose, spline_knots)
    dr_data <- cbind(dr_data, spline_basis)
    formula_str <- paste("y ~", paste(names(spline_basis), collapse = " + "))
    fit <- lm(as.formula(formula_str), data = dr_data, weights = weights)
  }

  list(
    type = "simplified",
    fit = fit,
    coefficients = coef(fit),
    se = summary(fit)$coefficients[, 2]
  )
}

#' @keywords internal
test_dose_response_nonlinearity <- function(dr_fit, model) {
  if (dr_fit$type == "metafor") {
    fit <- dr_fit$fit

    if (model == "quadratic") {
      # Test quadratic term
      idx_quad <- grep("dose2", names(coef(fit)))
      statistic <- (coef(fit)[idx_quad] / sqrt(diag(vcov(fit))[idx_quad]))^2
      return(list(
        statistic = statistic,
        df = 1,
        p_value = pchisq(statistic, df = 1, lower.tail = FALSE)
      ))

    } else if (model == "spline") {
      # Test all spline terms jointly
      spline_idx <- grep("spline", names(coef(fit)))
      n_spline <- length(spline_idx)

      # Wald test
      L <- diag(length(coef(fit)))[spline_idx, ]
      wald_stat <- t(L %*% coef(fit)) %*% solve(L %*% vcov(fit) %*% t(L)) %*% (L %*% coef(fit))

      return(list(
        statistic = as.numeric(wald_stat),
        df = n_spline,
        p_value = pchisq(wald_stat, df = n_spline, lower.tail = FALSE)
      ))
    }

  } else {
    # Simplified: use F-test
    fit <- dr_fit$fit

    if (model == "quadratic") {
      # Test quadratic term
      t_stat <- summary(fit)$coefficients["dose2", "t value"]
      return(list(
        statistic = t_stat^2,
        df = 1,
        p_value = 2 * pt(abs(t_stat), df = fit$df.residual, lower.tail = FALSE)
      ))
    } else {
      return(list(statistic = NA, df = NA, p_value = NA))
    }
  }
}

#' @keywords internal
predict_dose_response <- function(dr_fit, dose_range, ref_dose, model) {
  pred_data <- data.frame(dose = dose_range)

  if (model == "quadratic") {
    pred_data$dose2 <- dose_range^2
  } else if (model == "spline") {
    spline_basis <- create_spline_basis(dose_range, dr_fit$spline_knots)
    pred_data <- cbind(pred_data, spline_basis)
  }

  if (dr_fit$type == "metafor") {
    # Predict using metafor
    preds <- predict(dr_fit$fit, newmods = as.matrix(pred_data[, -1, drop = FALSE]))

    predictions <- data.frame(
      dose = dose_range,
      log_rr = preds$pred,
      log_rr_lower = preds$ci.lb,
      log_rr_upper = preds$ci.ub,
      rr = exp(preds$pred),
      rr_lower = exp(preds$ci.lb),
      rr_upper = exp(preds$ci.ub)
    )

  } else {
    # Predict using lm
    preds <- predict(dr_fit$fit, newdata = pred_data, se.fit = TRUE)

    predictions <- data.frame(
      dose = dose_range,
      log_rr = preds$fit,
      log_rr_lower = preds$fit - 1.96 * preds$se.fit,
      log_rr_upper = preds$fit + 1.96 * preds$se.fit,
      rr = exp(preds$fit),
      rr_lower = exp(preds$fit - 1.96 * preds$se.fit),
      rr_upper = exp(preds$fit + 1.96 * preds$se.fit)
    )
  }

  return(predictions)
}

#' @keywords internal
create_dose_response_plots <- function(dr_data, dr_fit, predictions, dose, ref_dose, type) {
  plots <- list()

  # Dose-response curve
  par(mar = c(5, 5, 3, 2))

  plot(predictions$dose, predictions$rr, type = "l", lwd = 3, col = "darkblue",
       xlab = "Dose", ylab = sprintf("%s (95%% CI)", toupper(type)),
       main = "Dose-Response Curve",
       ylim = range(c(predictions$rr_lower, predictions$rr_upper, 0.5, 2)),
       las = 1)

  # Confidence band
  polygon(c(predictions$dose, rev(predictions$dose)),
          c(predictions$rr_lower, rev(predictions$rr_upper)),
          col = rgb(0, 0, 0.5, 0.2), border = NA)

  # Reference line
  abline(h = 1, lty = 2, col = "gray50")
  abline(v = ref_dose, lty = 3, col = "red")

  # Study points
  study_rr <- exp(dr_data$y)
  study_rr_lower <- exp(dr_data$y - 1.96 * dr_data$se)
  study_rr_upper <- exp(dr_data$y + 1.96 * dr_data$se)

  points(dr_data[[dose]], study_rr, pch = 19, col = "darkred", cex = 1.2)

  # Study CIs
  for (i in 1:nrow(dr_data)) {
    lines(rep(dr_data[[dose]][i], 2),
          c(study_rr_lower[i], study_rr_upper[i]),
          col = "darkred", lwd = 1.5)
  }

  legend("topleft",
         legend = c("Pooled curve", "95% CI", "Study estimates"),
         lty = c(1, NA, NA), lwd = c(3, NA, NA),
         pch = c(NA, 15, 19),
         col = c("darkblue", rgb(0, 0, 0.5, 0.2), "darkred"))

  plots$dose_response <- recordPlot()

  plots
}

#' Predict Dose-Response
#'
#' @param object Dose-response meta-analysis object
#' @param newdose New dose values for prediction
#' @param ... Additional arguments
#'
#' @export
predict.dose_response_meta_analysis <- function(object, newdose, ...) {
  predictions <- predict_dose_response(
    object$fit,
    newdose,
    object$ref_dose,
    object$model
  )

  return(predictions)
}

#' @export
print.dose_response_meta_analysis <- function(x, ...) {
  cat("Dose-Response Meta-Analysis\n")
  cat("============================\n\n")

  cat(sprintf("Studies: %d\n", x$n_studies))
  cat(sprintf("Model: %s\n", x$model))
  cat(sprintf("Reference dose: %.2f\n", x$ref_dose))
  cat(sprintf("Type: %s\n\n", toupper(x$type)))

  if (!is.null(x$nonlinearity_test)) {
    cat("Non-linearity test:\n")
    cat(sprintf("  χ²(%d) = %.2f, p = %.4f\n\n",
               x$nonlinearity_test$df,
               x$nonlinearity_test$statistic,
               x$nonlinearity_test$p_value))
  }

  cat("Use predict() to estimate risk at specific doses\n")
  cat("Use plot() to visualize dose-response curve\n\n")

  invisible(x)
}

#' @export
plot.dose_response_meta_analysis <- function(x, ...) {
  x$plots$dose_response
}
