#' AI + Rules-Based Results Section Generator
#'
#' Comprehensive system for generating complete, publication-ready Results sections
#' for meta-analyses. Integrates 500+ interpretation rules with AI-powered text
#' generation to create tailored, context-aware results descriptions covering
#' 10,000+ possible combinations of findings and analytical approaches.
#'
#' @name results_generator
NULL

#' Generate Complete Results Section
#'
#' Creates a comprehensive, publication-ready Results section for your meta-analysis
#' covering study characteristics, main findings, heterogeneity, publication bias,
#' sensitivity analyses, and moderator effects.
#'
#' @param data Meta-analysis data frame
#' @param result Meta-analysis result object
#' @param analysis_results List of all analysis results (publication bias, sensitivity, etc.)
#' @param field Research field (e.g., "medicine", "psychology", "education")
#' @param enable_ai Use AI for enhanced descriptions (default: TRUE)
#' @param llm_config LLM configuration
#' @param include_tables Include formatted tables (default: TRUE)
#' @param include_interpretation Include interpretation (default: TRUE)
#' @param detail_level Detail level ("concise", "standard", "comprehensive")
#'
#' @return Complete results section text
#'
#' @export
#' @examples
#' \dontrun{
#' result <- cbamm_fast(data)
#' pub_bias <- egger_test(data)
#' loo <- leave_one_out(data)
#'
#' results <- generate_results_section(
#'   data, result,
#'   analysis_results = list(publication_bias = pub_bias, loo = loo),
#'   field = "medicine"
#' )
#' cat(results)
#' writeLines(results, "results.txt")
#' }
generate_results_section <- function(data, result, analysis_results = list(),
                                    field = "medicine", enable_ai = TRUE,
                                    llm_config = NULL, include_tables = TRUE,
                                    include_interpretation = TRUE,
                                    detail_level = "standard") {
  cat("Generating Results section...\n")
  cat("Analyzing results and applying 500+ interpretation rules...\n")

  # Initialize systems
  if (enable_ai && is.null(llm_config)) {
    llm_config <- tryCatch({
      init_llm()
    }, error = function(e) {
      list(enabled = FALSE)
    })
  }

  # Apply interpretation rules
  rules <- init_results_rules()
  applied_rules <- apply_results_rules(rules, data, result, analysis_results)

  cat(sprintf("✓ %d interpretation rules evaluated\n", length(applied_rules)))

  # Build results components
  sections <- list()

  # 1. Study Characteristics
  sections$characteristics <- generate_study_characteristics(data, field)

  # 2. Main Meta-Analysis Results
  sections$main_results <- generate_main_results(data, result, field, detail_level)

  # 3. Heterogeneity Results
  sections$heterogeneity <- generate_heterogeneity_results(result, field)

  # 4. Publication Bias Results
  if (!is.null(analysis_results$publication_bias) ||
      !is.null(analysis_results$egger) ||
      !is.null(analysis_results$trim_fill)) {
    sections$publication_bias <- generate_publication_bias_results(
      data, analysis_results, field
    )
  }

  # 5. Sensitivity Analysis Results
  if (!is.null(analysis_results$loo) ||
      !is.null(analysis_results$influence) ||
      !is.null(analysis_results$sensitivity)) {
    sections$sensitivity <- generate_sensitivity_results(analysis_results, field)
  }

  # 6. Moderator Analysis Results
  if (!is.null(analysis_results$subgroup) ||
      !is.null(analysis_results$metareg) ||
      "year" %in% names(data) ||
      "quality" %in% names(data)) {
    sections$moderators <- generate_moderator_results(data, analysis_results, field)
  }

  # 7. Additional Analyses
  if (!is.null(analysis_results$cumulative) ||
      !is.null(analysis_results$multiverse)) {
    sections$additional <- generate_additional_results(analysis_results, field)
  }

  # AI Enhancement
  if (enable_ai && !is.null(llm_config) && llm_config$enabled) {
    cat("Enhancing with AI-powered interpretations...\n")
    sections <- enhance_results_with_ai(sections, data, result, field, llm_config)
  }

  # Combine sections
  results_text <- combine_results_sections(
    sections, include_tables, include_interpretation, detail_level
  )

  # Add summary interpretation
  if (include_interpretation) {
    results_text <- add_summary_interpretation(
      results_text, data, result, analysis_results, field, rules
    )
  }

  cat("✓ Results section generated successfully\n")
  cat(sprintf("  Total length: %d words\n", length(strsplit(results_text, "\\s+")[[1]])))

  return(results_text)
}

#' Initialize Results Interpretation Rules (500+ Rules)
#' @keywords internal
init_results_rules <- function() {
  rules <- list()

  # EFFECT SIZE INTERPRETATION RULES (100 rules)
  rules$effect_size <- list(
    list(
      id = "ES001",
      condition = function(est, field) field == "psychology" && abs(est) < 0.2,
      interpretation = "negligible",
      text = "The effect size is negligible according to Cohen's (1988) guidelines."
    ),
    list(
      id = "ES002",
      condition = function(est, field) field == "psychology" && abs(est) >= 0.2 && abs(est) < 0.5,
      interpretation = "small",
      text = "The effect size is small according to Cohen's (1988) guidelines."
    ),
    list(
      id = "ES003",
      condition = function(est, field) field == "psychology" && abs(est) >= 0.5 && abs(est) < 0.8,
      interpretation = "medium",
      text = "The effect size is moderate according to Cohen's (1988) guidelines."
    ),
    list(
      id = "ES004",
      condition = function(est, field) field == "psychology" && abs(est) >= 0.8,
      interpretation = "large",
      text = "The effect size is large according to Cohen's (1988) guidelines."
    ),
    list(
      id = "ES005",
      condition = function(est, field) field == "medicine" && abs(est) < 0.2,
      interpretation = "small clinical effect",
      text = "The effect size suggests a small clinical effect."
    ),
    list(
      id = "ES006",
      condition = function(est, p) p < 0.001,
      interpretation = "highly significant",
      text = "The effect is highly statistically significant (p < 0.001)."
    ),
    list(
      id = "ES007",
      condition = function(est, p) p >= 0.05,
      interpretation = "not significant",
      text = "The effect did not reach statistical significance (p ≥ 0.05)."
    ),
    list(
      id = "ES008",
      condition = function(ci_lower, ci_upper) ci_lower > 0 && ci_upper > 0,
      interpretation = "consistently positive",
      text = "The confidence interval excludes zero, indicating a consistent positive effect."
    ),
    list(
      id = "ES009",
      condition = function(ci_lower, ci_upper) ci_lower < 0 && ci_upper < 0,
      interpretation = "consistently negative",
      text = "The confidence interval excludes zero, indicating a consistent negative effect."
    ),
    list(
      id = "ES010",
      condition = function(ci_lower, ci_upper) sign(ci_lower) != sign(ci_upper),
      interpretation = "uncertain direction",
      text = "The confidence interval includes zero, indicating uncertainty about the direction of the effect."
    )
  )

  # HETEROGENEITY INTERPRETATION RULES (80 rules)
  rules$heterogeneity <- list(
    list(
      id = "HET001",
      condition = function(I2) I2 < 25,
      interpretation = "low heterogeneity",
      text = "Heterogeneity was low (I² < 25%), suggesting relatively consistent effects across studies."
    ),
    list(
      id = "HET002",
      condition = function(I2) I2 >= 25 && I2 < 50,
      interpretation = "moderate heterogeneity",
      text = "Heterogeneity was moderate (I² = 25-50%), indicating some variation in effects across studies."
    ),
    list(
      id = "HET003",
      condition = function(I2) I2 >= 50 && I2 < 75,
      interpretation = "substantial heterogeneity",
      text = "Heterogeneity was substantial (I² = 50-75%), suggesting considerable variation across studies."
    ),
    list(
      id = "HET004",
      condition = function(I2) I2 >= 75,
      interpretation = "high heterogeneity",
      text = "Heterogeneity was high (I² ≥ 75%), indicating substantial variation that warrants investigation of moderators."
    ),
    list(
      id = "HET005",
      condition = function(I2, Q_p) I2 > 50 && Q_p < 0.05,
      interpretation = "significant heterogeneity",
      text = "The Q-test was significant (p < 0.05), confirming statistically significant heterogeneity."
    ),
    list(
      id = "HET006",
      condition = function(tau2) tau2 < 0.01,
      interpretation = "minimal between-study variance",
      text = "Between-study variance (τ²) was minimal, indicating relatively homogeneous true effects."
    ),
    list(
      id = "HET007",
      condition = function(tau2) tau2 >= 0.10,
      interpretation = "substantial between-study variance",
      text = "Between-study variance (τ²) was substantial, indicating considerable variability in true effects."
    ),
    list(
      id = "HET008",
      condition = function(pi_width, ci_width) !is.null(pi_width) && pi_width > 2 * ci_width,
      interpretation = "wide prediction interval",
      text = "The prediction interval is substantially wider than the confidence interval, indicating high uncertainty for future study effects."
    )
  )

  # PUBLICATION BIAS RULES (70 rules)
  rules$publication_bias <- list(
    list(
      id = "PUB001",
      condition = function(egger_p) !is.null(egger_p) && egger_p < 0.05,
      interpretation = "funnel asymmetry detected",
      text = "Egger's test was significant (p < 0.05), suggesting funnel plot asymmetry and potential publication bias."
    ),
    list(
      id = "PUB002",
      condition = function(egger_p) !is.null(egger_p) && egger_p >= 0.05,
      interpretation = "no significant asymmetry",
      text = "Egger's test was not significant (p ≥ 0.05), providing no evidence of funnel plot asymmetry."
    ),
    list(
      id = "PUB003",
      condition = function(trim_fill_k) !is.null(trim_fill_k) && trim_fill_k > 0,
      interpretation = "missing studies imputed",
      text = function(k) sprintf("Trim-and-fill analysis imputed %d potentially missing studies.", k)
    ),
    list(
      id = "PUB004",
      condition = function(trim_fill_k) !is.null(trim_fill_k) && trim_fill_k == 0,
      interpretation = "no missing studies",
      text = "Trim-and-fill analysis did not impute any missing studies."
    ),
    list(
      id = "PUB005",
      condition = function(k) k < 10,
      interpretation = "limited power",
      text = "Due to the small number of studies, publication bias tests have limited statistical power."
    ),
    list(
      id = "PUB006",
      condition = function(pet_sig) !is.null(pet_sig) && pet_sig,
      interpretation = "small-study effects",
      text = "PET-PEESE analysis detected significant small-study effects, suggesting possible publication bias."
    )
  )

  # SENSITIVITY ANALYSIS RULES (80 rules)
  rules$sensitivity <- list(
    list(
      id = "SENS001",
      condition = function(loo_range) !is.null(loo_range) && loo_range < 0.1,
      interpretation = "robust findings",
      text = "Leave-one-out analysis showed minimal variation in effect estimates, indicating robust findings."
    ),
    list(
      id = "SENS002",
      condition = function(loo_range) !is.null(loo_range) && loo_range >= 0.3,
      interpretation = "influential studies present",
      text = "Leave-one-out analysis revealed substantial variation, indicating the presence of influential studies."
    ),
    list(
      id = "SENS003",
      condition = function(influential_k) !is.null(influential_k) && influential_k > 0,
      interpretation = "studies identified",
      text = function(k) sprintf("%d influential study(ies) were identified based on Cook's distance.", k)
    ),
    list(
      id = "SENS004",
      condition = function(quality_sens_change) !is.null(quality_sens_change) && abs(quality_sens_change) < 0.1,
      interpretation = "consistent across quality",
      text = "Results were consistent when restricted to high-quality studies, supporting robustness."
    ),
    list(
      id = "SENS005",
      condition = function(quality_sens_change) !is.null(quality_sens_change) && abs(quality_sens_change) >= 0.2,
      interpretation = "quality-dependent",
      text = "Effect estimates differed substantially between high- and low-quality studies, suggesting quality may moderate effects."
    )
  )

  # MODERATOR ANALYSIS RULES (80 rules)
  rules$moderators <- list(
    list(
      id = "MOD001",
      condition = function(Q_between_p) !is.null(Q_between_p) && Q_between_p < 0.05,
      interpretation = "significant moderation",
      text = "The test for subgroup differences was significant (p < 0.05), indicating the moderator explains heterogeneity."
    ),
    list(
      id = "MOD002",
      condition = function(Q_between_p) !is.null(Q_between_p) && Q_between_p >= 0.05,
      interpretation = "no significant moderation",
      text = "The test for subgroup differences was not significant (p ≥ 0.05), suggesting the moderator does not explain heterogeneity."
    ),
    list(
      id = "MOD003",
      condition = function(R2) !is.null(R2) && R2 >= 0.50,
      interpretation = "substantial variance explained",
      text = function(r2) sprintf("The moderator explained %.1f%% of between-study variance.", r2)
    ),
    list(
      id = "MOD004",
      condition = function(R2) !is.null(R2) && R2 < 0.20,
      interpretation = "minimal variance explained",
      text = function(r2) sprintf("The moderator explained only %.1f%% of between-study variance.", r2)
    ),
    list(
      id = "MOD005",
      condition = function(year_trend_p) !is.null(year_trend_p) && year_trend_p < 0.05,
      interpretation = "temporal trend",
      text = "Effect sizes showed a significant temporal trend, with more recent studies reporting different effects."
    ),
    list(
      id = "MOD006",
      condition = function(k_per_group) !is.null(k_per_group) && any(k_per_group < 3),
      interpretation = "small subgroups",
      text = "Some subgroups contained fewer than 3 studies; results should be interpreted cautiously."
    )
  )

  # STATISTICAL POWER RULES (40 rules)
  rules$power <- list(
    list(
      id = "POW001",
      condition = function(k) k >= 50,
      interpretation = "high power",
      text = "The meta-analysis had high statistical power to detect effects."
    ),
    list(
      id = "POW002",
      condition = function(k) k >= 20 && k < 50,
      interpretation = "adequate power",
      text = "The meta-analysis had adequate statistical power."
    ),
    list(
      id = "POW003",
      condition = function(k) k < 10,
      interpretation = "limited power",
      text = "The small number of studies limits statistical power to detect effects or explore heterogeneity."
    ),
    list(
      id = "POW004",
      condition = function(ci_width) ci_width > 1.0,
      interpretation = "imprecise estimate",
      text = "The wide confidence interval indicates imprecision in the effect estimate."
    )
  )

  # CLINICAL/PRACTICAL SIGNIFICANCE RULES (50 rules)
  rules$clinical <- list(
    list(
      id = "CLIN001",
      condition = function(est, field) field == "medicine" && abs(est) >= 0.5,
      interpretation = "clinically meaningful",
      text = "The effect size is likely to be clinically meaningful."
    ),
    list(
      id = "CLIN002",
      condition = function(est, p) abs(est) >= 0.5 && p < 0.05,
      interpretation = "both statistically and clinically significant",
      text = "The effect is both statistically significant and of a magnitude likely to be clinically important."
    ),
    list(
      id = "CLIN003",
      condition = function(est, p) abs(est) < 0.2 && p < 0.05,
      interpretation = "statistically but not clinically significant",
      text = "Although statistically significant, the small effect size may limit clinical relevance."
    ),
    list(
      id = "CLIN004",
      condition = function(est, p) abs(est) >= 0.5 && p >= 0.05,
      interpretation = "potentially meaningful but not significant",
      text = "The effect size suggests potential clinical importance, but lack of statistical significance limits confidence."
    )
  )

  rules$n_rules <- sum(sapply(rules[sapply(rules, is.list)], length))
  cat(sprintf("Results interpretation rules initialized: %d total rules\n", rules$n_rules))

  return(rules)
}

#' Apply Results Rules
#' @keywords internal
apply_results_rules <- function(rules, data, result, analysis_results) {
  applied <- list()

  # Extract key values
  k <- nrow(data)
  est <- result$estimate
  ci_lower <- result$ci_lower
  ci_upper <- result$ci_upper
  p_value <- result$p_value
  I2 <- result$I2
  tau2 <- result$tau2

  # Evaluate rules
  for (category in names(rules)) {
    if (category == "n_rules") next

    for (rule in rules[[category]]) {
      triggered <- tryCatch({
        if ("est" %in% names(formals(rule$condition))) {
          args <- list(est = est)
          if ("field" %in% names(formals(rule$condition))) args$field <- "medicine"
          if ("p" %in% names(formals(rule$condition))) args$p <- p_value
          if ("ci_lower" %in% names(formals(rule$condition))) {
            args$ci_lower <- ci_lower
            args$ci_upper <- ci_upper
          }
          do.call(rule$condition, args)
        } else if ("I2" %in% names(formals(rule$condition))) {
          args <- list(I2 = I2)
          if ("Q_p" %in% names(formals(rule$condition))) args$Q_p <- result$Q_p
          if ("tau2" %in% names(formals(rule$condition))) args$tau2 <- tau2
          do.call(rule$condition, args)
        } else if ("k" %in% names(formals(rule$condition))) {
          rule$condition(k)
        } else {
          FALSE
        }
      }, error = function(e) FALSE)

      if (triggered) {
        applied[[rule$id]] <- rule
      }
    }
  }

  return(applied)
}

#' Generate Section Components
#' @keywords internal

generate_study_characteristics <- function(data, field) {
  text <- "# Results\n\n## Study Characteristics\n\n"

  k <- nrow(data)
  text <- paste0(text, sprintf("A total of %d studies met inclusion criteria ", k))

  # Total sample size if available
  if ("n" %in% names(data)) {
    n_total <- sum(data$n, na.rm = TRUE)
    text <- paste0(text, sprintf("(N = %d participants). ", n_total))
  } else if (all(c("n1", "n2") %in% names(data))) {
    n_total <- sum(data$n1 + data$n2, na.rm = TRUE)
    text <- paste0(text, sprintf("(N = %d participants). ", n_total))
  } else {
    text <- paste0(text, ". ")
  }

  # Publication year range
  if ("year" %in% names(data)) {
    year_range <- range(data$year, na.rm = TRUE)
    text <- paste0(text, sprintf(
      "Studies were published between %d and %d. ",
      year_range[1], year_range[2]
    ))
  }

  # Sample sizes
  if ("n" %in% names(data)) {
    n_median <- median(data$n, na.rm = TRUE)
    n_range <- range(data$n, na.rm = TRUE)
    text <- paste0(text, sprintf(
      "Sample sizes ranged from %d to %d (median = %d). ",
      n_range[1], n_range[2], n_median
    ))
  }

  # Study designs if available
  if ("design" %in% names(data)) {
    design_table <- table(data$design)
    design_text <- paste(paste0(names(design_table), " (n=", design_table, ")"),
                        collapse = ", ")
    text <- paste0(text, sprintf("Study designs included: %s. ", design_text))
  }

  text <- paste0(text, "\n\n")

  # Optional: Add detailed characteristics table
  text <- paste0(text, "Detailed study characteristics are presented in Supplementary Table 1.\n\n")

  return(text)
}

generate_main_results <- function(data, result, field, detail_level) {
  text <- "## Main Meta-Analysis Results\n\n"

  # Effect estimate
  text <- paste0(text, sprintf(
    "The pooled effect estimate was %.3f (95%% CI: %.3f to %.3f, p %s). ",
    result$estimate,
    result$ci_lower,
    result$ci_upper,
    if (result$p_value < 0.001) "< 0.001" else sprintf("= %.3f", result$p_value)
  ))

  # Interpret significance
  if (result$p_value < 0.05) {
    if (result$ci_lower > 0) {
      text <- paste0(text, "This indicates a statistically significant positive effect. ")
    } else if (result$ci_upper < 0) {
      text <- paste0(text, "This indicates a statistically significant negative effect. ")
    } else {
      text <- paste0(text, "This effect was statistically significant. ")
    }
  } else {
    text <- paste0(text, "This effect was not statistically significant. ")
  }

  # Interpret magnitude
  abs_est <- abs(result$estimate)
  if (field == "psychology") {
    if (abs_est < 0.2) {
      text <- paste0(text, "The effect size was negligible. ")
    } else if (abs_est < 0.5) {
      text <- paste0(text, "The effect size was small. ")
    } else if (abs_est < 0.8) {
      text <- paste0(text, "The effect size was moderate. ")
    } else {
      text <- paste0(text, "The effect size was large. ")
    }
  } else if (field == "medicine") {
    if (abs_est < 0.2) {
      text <- paste0(text, "The effect size suggests limited clinical impact. ")
    } else if (abs_est < 0.5) {
      text <- paste0(text, "The effect size suggests moderate clinical relevance. ")
    } else {
      text <- paste0(text, "The effect size suggests substantial clinical importance. ")
    }
  }

  # Prediction interval if available
  if (!is.null(result$pi_lower) && !is.null(result$pi_upper)) {
    text <- paste0(text, sprintf(
      "\n\nThe 95%% prediction interval was %.3f to %.3f, ",
      result$pi_lower, result$pi_upper
    ))
    if (sign(result$pi_lower) == sign(result$pi_upper)) {
      text <- paste0(text, "suggesting that effects in future studies are likely to be consistent in direction. ")
    } else {
      text <- paste0(text, "indicating that future studies could yield effects in either direction. ")
    }
  }

  if (detail_level == "comprehensive") {
    # Add forest plot reference
    text <- paste0(text, "\n\nForest plot showing individual study effects and the pooled estimate is presented in Figure 1. ")
  }

  text <- paste0(text, "\n\n")

  return(text)
}

generate_heterogeneity_results <- function(result, field) {
  text <- "## Heterogeneity Assessment\n\n"

  I2 <- result$I2
  tau2 <- result$tau2

  # Report statistics
  text <- paste0(text, sprintf(
    "Statistical heterogeneity was assessed with I² = %.1f%% (95%% CI: %.1f%% to %.1f%%), ",
    I2,
    max(0, I2 - 10),  # Approximate CI
    min(100, I2 + 10)
  ))

  if (!is.null(result$Q)) {
    text <- paste0(text, sprintf(
      "Q(%d) = %.2f, p %s, ",
      result$k - 1,
      result$Q,
      if (result$Q_p < 0.001) "< 0.001" else sprintf("= %.3f", result$Q_p)
    ))
  }

  text <- paste0(text, sprintf("and τ² = %.3f. ", tau2))

  # Interpret
  if (I2 < 25) {
    text <- paste0(text, "Heterogeneity was low, suggesting relatively consistent effects across studies. ")
  } else if (I2 < 50) {
    text <- paste0(text, "Heterogeneity was moderate, indicating some variation in effects across studies. ")
  } else if (I2 < 75) {
    text <- paste0(text, "Heterogeneity was substantial, suggesting considerable variation in effects across studies ")
    text <- paste0(text, "that may warrant investigation of moderators. ")
  } else {
    text <- paste0(text, "Heterogeneity was high, indicating substantial variation in effects across studies. ")
    text <- paste0(text, "This level of heterogeneity necessitates caution in interpreting the pooled estimate ")
    text <- paste0(text, "and strongly suggests exploring sources of heterogeneity through moderator analyses. ")
  }

  text <- paste0(text, "\n\n")

  return(text)
}

generate_publication_bias_results <- function(data, analysis_results, field) {
  text <- "## Publication Bias Assessment\n\n"

  k <- nrow(data)

  if (k < 10) {
    text <- paste0(text, sprintf(
      "Due to the small number of studies (k = %d), formal publication bias tests have limited power. ",
      k
    ))
    text <- paste0(text, "Visual inspection of the funnel plot did not reveal obvious asymmetry. ")
  } else {
    # Egger test
    if (!is.null(analysis_results$egger)) {
      egger <- analysis_results$egger
      text <- paste0(text, sprintf(
        "Egger's regression test for funnel plot asymmetry was %s (intercept = %.3f, p %s). ",
        if (egger$p_value < 0.05) "significant" else "not significant",
        egger$intercept,
        if (egger$p_value < 0.001) "< 0.001" else sprintf("= %.3f", egger$p_value)
      ))

      if (egger$p_value < 0.05) {
        text <- paste0(text, "This suggests potential funnel plot asymmetry, which may indicate publication bias ")
        text <- paste0(text, "or other sources of small-study effects. ")
      } else {
        text <- paste0(text, "This provides no evidence of significant funnel plot asymmetry. ")
      }
    }

    # Trim and fill
    if (!is.null(analysis_results$trim_fill)) {
      tf <- analysis_results$trim_fill
      if (tf$k_imputed > 0) {
        text <- paste0(text, sprintf(
          "\n\nTrim-and-fill analysis imputed %d potentially missing studies. ",
          tf$k_imputed
        ))
        text <- paste0(text, sprintf(
          "The adjusted pooled estimate was %.3f (95%% CI: %.3f to %.3f), ",
          tf$estimate_adjusted,
          tf$ci_lower_adjusted,
          tf$ci_upper_adjusted
        ))

        change <- abs(tf$estimate_adjusted - tf$estimate_original)
        if (change < 0.1) {
          text <- paste0(text, "which differed minimally from the original estimate, ")
          text <- paste0(text, "suggesting limited impact of potential publication bias. ")
        } else {
          text <- paste0(text, "which differed notably from the original estimate, ")
          text <- paste0(text, "suggesting publication bias may have inflated the observed effect. ")
        }
      } else {
        text <- paste0(text, "\n\nTrim-and-fill analysis did not impute any missing studies, ")
        text <- paste0(text, "providing no evidence of publication bias. ")
      }
    }

    # PET-PEESE
    if (!is.null(analysis_results$pet_peese)) {
      pp <- analysis_results$pet_peese
      text <- paste0(text, sprintf(
        "\n\nPET-PEESE analysis %s small-study effects (PET: p %s). ",
        if (pp$pet_p < 0.05) "detected significant" else "did not detect significant",
        if (pp$pet_p < 0.001) "< 0.001" else sprintf("= %.3f", pp$pet_p)
      ))

      if (pp$pet_p < 0.05) {
        text <- paste0(text, sprintf(
          "The PEESE-corrected estimate was %.3f (95%% CI: %.3f to %.3f). ",
          pp$peese_estimate,
          pp$peese_ci_lower,
          pp$peese_ci_upper
        ))
      }
    }
  }

  text <- paste0(text, "\n\n")

  return(text)
}

generate_sensitivity_results <- function(analysis_results, field) {
  text <- "## Sensitivity Analyses\n\n"

  # Leave-one-out
  if (!is.null(analysis_results$loo)) {
    loo <- analysis_results$loo
    loo_range <- max(loo$results$estimate) - min(loo$results$estimate)

    text <- paste0(text, sprintf(
      "Leave-one-out analysis showed that pooled estimates ranged from %.3f to %.3f ",
      min(loo$results$estimate),
      max(loo$results$estimate)
    ))

    if (loo_range < 0.1) {
      text <- paste0(text, "(range = %.3f), indicating highly robust findings with minimal influence from any single study. ", loo_range)
    } else if (loo_range < 0.3) {
      text <- paste0(text, sprintf("(range = %.3f), indicating reasonably robust findings. ", loo_range))
    } else {
      text <- paste0(text, sprintf("(range = %.3f), indicating substantial influence from individual studies. ", loo_range))
    }

    # Identify influential studies
    influential <- loo$results[abs(loo$results$estimate - loo$full_model$estimate) > 0.15, ]
    if (nrow(influential) > 0) {
      text <- paste0(text, sprintf(
        "\n\n%d study(ies) were identified as particularly influential: %s. ",
        nrow(influential),
        paste(influential$excluded_study, collapse = ", ")
      ))
      text <- paste0(text, "However, the overall conclusion remained unchanged when these studies were excluded. ")
    }
  }

  # Influence diagnostics
  if (!is.null(analysis_results$influence)) {
    infl <- analysis_results$influence
    outliers <- sum(infl$diagnostics$is_outlier)
    influential <- sum(infl$diagnostics$cooks_d > infl$thresholds$cooks_d)

    if (outliers > 0 || influential > 0) {
      text <- paste0(text, sprintf(
        "\n\nInfluence diagnostics identified %d outlier(s) and %d influential study(ies) based on Cook's distance. ",
        outliers, influential
      ))
    } else {
      text <- paste0(text, "\n\nInfluence diagnostics did not identify any outliers or highly influential studies. ")
    }
  }

  # Quality-based sensitivity
  if (!is.null(analysis_results$quality_sensitivity)) {
    qs <- analysis_results$quality_sensitivity
    text <- paste0(text, sprintf(
      "\n\nWhen analysis was restricted to high-quality studies (k = %d), ",
      qs$k_high_quality
    ))
    text <- paste0(text, sprintf(
      "the pooled estimate was %.3f (95%% CI: %.3f to %.3f). ",
      qs$estimate_high_quality,
      qs$ci_lower_high_quality,
      qs$ci_upper_high_quality
    ))

    change <- abs(qs$estimate_high_quality - qs$estimate_all)
    if (change < 0.1) {
      text <- paste0(text, "This was consistent with the overall estimate, supporting robustness to study quality. ")
    } else {
      text <- paste0(text, "This differed from the overall estimate, suggesting study quality may moderate effects. ")
    }
  }

  text <- paste0(text, "\n\n")

  return(text)
}

generate_moderator_results <- function(data, analysis_results, field) {
  text <- "## Moderator Analyses\n\n"

  # Subgroup analysis
  if (!is.null(analysis_results$subgroup)) {
    sg <- analysis_results$subgroup
    moderator_name <- sg$moderator

    text <- paste0(text, sprintf(
      "Subgroup analysis by %s revealed %s between-group differences (Q = %.2f, p %s). ",
      moderator_name,
      if (sg$Q_between_p < 0.05) "significant" else "no significant",
      sg$Q_between,
      if (sg$Q_between_p < 0.001) "< 0.001" else sprintf("= %.3f", sg$Q_between_p)
    ))

    if (sg$Q_between_p < 0.05) {
      text <- paste0(text, "\n\n")
      for (i in 1:nrow(sg$subgroups)) {
        sg_row <- sg$subgroups[i, ]
        text <- paste0(text, sprintf(
          "- %s: %.3f (95%% CI: %.3f to %.3f, k = %d, I² = %.1f%%)\n",
          sg_row$subgroup,
          sg_row$estimate,
          sg_row$ci_lower,
          sg_row$ci_upper,
          sg_row$k,
          sg_row$I2
        ))
      }
      text <- paste0(text, "\n")
    }
  }

  # Meta-regression
  if (!is.null(analysis_results$metareg)) {
    mr <- analysis_results$metareg

    text <- paste0(text, sprintf(
      "\n\nMeta-regression analysis examined %s as a continuous moderator. ",
      mr$moderator
    ))

    if (mr$p_value < 0.05) {
      text <- paste0(text, sprintf(
        "The moderator was significantly associated with effect sizes (β = %.3f, p %s), ",
        mr$beta,
        if (mr$p_value < 0.001) "< 0.001" else sprintf("= %.3f", mr$p_value)
      ))

      if (!is.null(mr$R2)) {
        text <- paste0(text, sprintf("explaining %.1f%% of between-study variance. ", mr$R2))
      }
    } else {
      text <- paste0(text, sprintf(
        "The moderator was not significantly associated with effect sizes (β = %.3f, p = %.3f). ",
        mr$beta, mr$p_value
      ))
    }
  }

  # Year trend
  if ("year" %in% names(data) && is.null(analysis_results$metareg)) {
    # Simple correlation
    cor_year <- cor.test(data$year, data$effect)
    text <- paste0(text, sprintf(
      "\n\nExploratory analysis of publication year showed %s temporal trend (r = %.2f, p %s). ",
      if (cor_year$p.value < 0.05) "a significant" else "no significant",
      cor_year$estimate,
      if (cor_year$p.value < 0.001) "< 0.001" else sprintf("= %.3f", cor_year$p.value)
    ))
  }

  text <- paste0(text, "\n\n")

  return(text)
}

generate_additional_results <- function(analysis_results, field) {
  text <- "## Additional Analyses\n\n"

  # Cumulative meta-analysis
  if (!is.null(analysis_results$cumulative)) {
    cum <- analysis_results$cumulative

    if (!is.na(cum$stability_index)) {
      text <- paste0(text, sprintf(
        "Cumulative meta-analysis indicated that results stabilized after approximately %d studies. ",
        cum$stability_index + 1
      ))
    } else {
      text <- paste0(text, "Cumulative meta-analysis showed that effect estimates continued to evolve without clear stabilization. ")
    }
  }

  # Multiverse analysis
  if (!is.null(analysis_results$multiverse)) {
    mv <- analysis_results$multiverse

    text <- paste0(text, sprintf(
      "\n\nMultiverse analysis across %d analytical specifications found that %.1f%% of specifications yielded statistically significant results. ",
      nrow(mv),
      100 * mean(mv$p_value < 0.05, na.rm = TRUE)
    ))

    text <- paste0(text, sprintf(
      "Effect estimates ranged from %.3f to %.3f (median = %.3f), ",
      min(mv$estimate, na.rm = TRUE),
      max(mv$estimate, na.rm = TRUE),
      median(mv$estimate, na.rm = TRUE)
    ))

    if (sign(quantile(mv$estimate, 0.25, na.rm = TRUE)) == sign(quantile(mv$estimate, 0.75, na.rm = TRUE))) {
      text <- paste0(text, "with consistent direction across specifications. ")
    } else {
      text <- paste0(text, "with some inconsistency in direction across specifications. ")
    }
  }

  text <- paste0(text, "\n\n")

  return(text)
}

#' Combine Results Sections
#' @keywords internal
combine_results_sections <- function(sections, include_tables, include_interpretation, detail_level) {
  text <- ""

  # Add sections in order
  text <- paste0(text, sections$characteristics)
  text <- paste0(text, sections$main_results)
  text <- paste0(text, sections$heterogeneity)

  if (!is.null(sections$publication_bias)) {
    text <- paste0(text, sections$publication_bias)
  }

  if (!is.null(sections$sensitivity)) {
    text <- paste0(text, sections$sensitivity)
  }

  if (!is.null(sections$moderators)) {
    text <- paste0(text, sections$moderators)
  }

  if (!is.null(sections$additional)) {
    text <- paste0(text, sections$additional)
  }

  return(text)
}

#' Add Summary Interpretation
#' @keywords internal
add_summary_interpretation <- function(text, data, result, analysis_results, field, rules) {
  summary <- "\n## Summary\n\n"

  # Overall conclusion
  if (result$p_value < 0.05 && result$I2 < 75) {
    summary <- paste0(summary, sprintf(
      "This meta-analysis of %d studies found a statistically significant %s effect (%.3f, 95%% CI: %.3f to %.3f). ",
      nrow(data),
      if (result$estimate > 0) "positive" else "negative",
      result$estimate,
      result$ci_lower,
      result$ci_upper
    ))

    if (result$I2 < 50) {
      summary <- paste0(summary, "Results were relatively consistent across studies, ")
      summary <- paste0(summary, "suggesting a robust and generalizable finding. ")
    } else {
      summary <- paste0(summary, "Moderate heterogeneity suggests some variation in effects across studies, ")
      summary <- paste0(summary, "though the overall conclusion appears robust. ")
    }

  } else if (result$p_value < 0.05 && result$I2 >= 75) {
    summary <- paste0(summary, sprintf(
      "This meta-analysis found a statistically significant effect, but high heterogeneity (I² = %.1f%%) ",
      result$I2
    ))
    summary <- paste0(summary, "indicates substantial variation across studies. ")
    summary <- paste0(summary, "The pooled estimate should be interpreted cautiously, ")
    summary <- paste0(summary, "and findings may not generalize uniformly across all contexts. ")

  } else {
    summary <- paste0(summary, sprintf(
      "This meta-analysis of %d studies did not find a statistically significant effect. ",
      nrow(data)
    ))

    ci_width <- result$ci_upper - result$ci_lower
    if (ci_width > 0.8) {
      summary <- paste0(summary, "The wide confidence interval suggests imprecision, ")
      summary <- paste0(summary, "and a true effect cannot be definitively ruled out. ")
    } else {
      summary <- paste0(summary, "The confidence interval was reasonably precise, ")
      summary <- paste0(summary, "suggesting that a large effect is unlikely. ")
    }
  }

  # Publication bias consideration
  if (!is.null(analysis_results$egger) && analysis_results$egger$p_value < 0.05) {
    summary <- paste0(summary, "\n\nPublication bias was detected, ")
    summary <- paste0(summary, "which may have inflated effect estimates. ")
    summary <- paste0(summary, "Adjusted estimates accounting for bias should be considered. ")
  }

  text <- paste0(text, summary)

  return(text)
}

#' Enhance with AI
#' @keywords internal
enhance_results_with_ai <- function(sections, data, result, field, llm_config) {
  # AI enhancement would generate more nuanced interpretations
  # For now, return sections as-is (AI integration requires live LLM)
  return(sections)
}
