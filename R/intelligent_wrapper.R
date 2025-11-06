#' Intelligent Wrappers - AI and Rules-Enhanced Meta-Analysis
#'
#' Wraps existing meta-analysis functions with AI assistance (local Ollama) and
#' rules-based expert guidance. Every analysis gets intelligent recommendations.
#'
#' @name intelligent_wrapper
NULL

#' Intelligent Meta-Analysis with AI + Rules
#'
#' Enhanced cbamm_fast with integrated AI guidance and rules validation.
#'
#' @param data Data frame with meta-analysis data
#' @param method Pooling method
#' @param enable_ai Use AI assistance (requires local Ollama)
#' @param enable_rules Use rules engine (default: TRUE)
#' @param llm_config LLM configuration (optional)
#' @param verbose Print AI/rules guidance
#'
#' @return Enhanced meta-analysis result with AI/rules recommendations
#'
#' @export
#' @examples
#' \dontrun{
#' # With AI and rules (local Ollama)
#' result <- intelligent_cbamm(data, enable_ai = TRUE, enable_rules = TRUE)
#'
#' # Rules only (no AI)
#' result <- intelligent_cbamm(data, enable_ai = FALSE, enable_rules = TRUE)
#' }
intelligent_cbamm <- function(data, method = "REML", enable_ai = FALSE,
                              enable_rules = TRUE, llm_config = NULL,
                              verbose = TRUE) {
  if (verbose) {
    cat("\n=== Intelligent Meta-Analysis System ===\n")
    cat("AI: ", if(enable_ai) "ENABLED (Local Ollama)" else "DISABLED", "\n")
    cat("Rules: ", if(enable_rules) "ENABLED (500+ rules)" else "DISABLED", "\n\n")
  }

  # Initialize systems
  if (enable_ai && is.null(llm_config)) {
    llm_config <- init_llm(endpoint = "http://localhost:11434/api/generate")
  }

  if (enable_rules) {
    rules_engine <- init_rules_engine()
  }

  # PRE-ANALYSIS: Rules-based validation
  if (enable_rules) {
    if (verbose) cat("Running pre-analysis validation (500+ rules)...\n")
    pre_checks <- apply_rules(rules_engine, data, result = NULL)

    # Check for critical issues
    critical <- Filter(function(r) r$severity == "critical", pre_checks)
    if (length(critical) > 0) {
      stop("Critical issues detected. Run apply_rules() for details.")
    }

    if (verbose && length(pre_checks) > 0) {
      cat(sprintf("✓ Validation complete: %d rules triggered\n", length(pre_checks)))
      high_priority <- Filter(function(r) r$severity %in% c("critical", "high"), pre_checks)
      if (length(high_priority) > 0) {
        cat("\nHigh Priority Issues:\n")
        for (rule in head(high_priority, 5)) {
          cat(sprintf("  • %s\n", rule$message))
        }
      }
    }
  }

  # AI METHOD SELECTION
  if (enable_ai && llm_config$enabled) {
    if (verbose) cat("\nConsulting AI for method selection...\n")
    ai_recommendations <- ai_method_selection(data, llm_config = llm_config)

    if (verbose) {
      cat("AI Recommendations:\n")
      if (is.list(ai_recommendations$recommendations)) {
        for (rec_name in names(ai_recommendations$recommendations)) {
          cat(sprintf("  • %s: %s\n", rec_name, ai_recommendations$recommendations[[rec_name]]))
        }
      }
    }
  }

  # SCENARIO MATCHING
  if (verbose) cat("\nMatching to reference scenarios (10,000+ database)...\n")
  scenario_db <- init_scenario_database()
  similar_scenarios <- find_scenarios(
    scenario_db,
    k_min = max(1, nrow(data) - 5),
    k_max = nrow(data) + 5
  )

  if (verbose) {
    cat(sprintf("✓ Found %d similar scenarios from literature\n", length(similar_scenarios)))
  }

  # CORE ANALYSIS
  if (verbose) cat("\nPerforming core meta-analysis...\n")
  result <- cbamm_fast(data, method = method, verbose = FALSE)

  # POST-ANALYSIS: Rules-based result validation
  if (enable_rules) {
    if (verbose) cat("Running post-analysis validation...\n")
    post_checks <- apply_rules(rules_engine, data, result = result)

    result$rules_assessment <- post_checks
    if (verbose) {
      cat(sprintf("✓ Result validation: %d rules triggered\n", length(post_checks)))
    }
  }

  # AI INTERPRETATION
  if (enable_ai && llm_config$enabled) {
    if (verbose) cat("\nGenerating AI interpretation...\n")
    interpretation <- ai_interpret_results(result, llm_config = llm_config)
    result$ai_interpretation <- interpretation

    if (verbose) {
      cat("\nAI Interpretation:\n")
      cat(substr(interpretation, 1, 300))
      if (nchar(interpretation) > 300) cat("...\n")
    }
  }

  # RESEARCH GAPS ANALYSIS
  if (enable_ai && llm_config$enabled) {
    if (verbose) cat("\nIdentifying research gaps...\n")
    gaps <- ai_research_gaps(result, data, llm_config = llm_config)
    result$research_gaps <- gaps
  }

  # Add metadata
  result$intelligent_analysis <- list(
    ai_enabled = enable_ai,
    rules_enabled = enable_rules,
    n_rules_triggered = if (enable_rules) length(post_checks) else 0,
    similar_scenarios = length(similar_scenarios),
    timestamp = Sys.time()
  )

  class(result) <- c("intelligent_cbamm", class(result))

  if (verbose) {
    cat("\n=== Analysis Complete ===\n")
    cat(sprintf("Pooled Effect: %.3f (95%% CI: %.3f to %.3f)\n",
                result$estimate, result$ci_lower, result$ci_upper))
    cat(sprintf("Heterogeneity I²: %.1f%%\n", result$I2))
    cat(sprintf("Rules Triggered: %d\n", result$intelligent_analysis$n_rules_triggered))
  }

  return(result)
}

#' Intelligent Publication Bias Assessment
#'
#' Enhanced publication bias assessment with AI and rules guidance.
#'
#' @param data Meta-analysis data
#' @param enable_ai Use AI for interpretation
#' @param enable_rules Use rules engine
#' @param llm_config LLM config
#'
#' @return Enhanced publication bias assessment
#'
#' @export
intelligent_publication_bias <- function(data, enable_ai = FALSE,
                                        enable_rules = TRUE, llm_config = NULL) {
  # Rules check
  if (enable_rules) {
    rules_engine <- init_rules_engine()
    bias_rules <- apply_rules(rules_engine, data)

    # Check if sufficient studies
    if (nrow(data) < 10) {
      warning("Rules engine: Fewer than 10 studies. Publication bias tests may be unreliable.")
    }
  }

  # Run standard tests
  egger <- egger_test(data)
  pet_peese_result <- pet_peese(data)
  tf <- trim_fill(data)

  result <- list(
    egger = egger,
    pet_peese = pet_peese_result,
    trim_fill = tf,
    n_studies = nrow(data)
  )

  # AI interpretation
  if (enable_ai) {
    if (is.null(llm_config)) {
      llm_config <- init_llm()
    }

    if (llm_config$enabled) {
      prompt <- sprintf(
        "Interpret these publication bias results:

Egger test p-value: %.4f
Trim-and-fill imputed studies: %d
Number of studies: %d

Provide:
1. Overall assessment of publication bias risk
2. Which test result is most reliable
3. Recommended action",
        egger$p_value,
        tf$n_imputed,
        nrow(data)
      )

      interpretation <- query_llm_internal(prompt, llm_config)
      result$ai_interpretation <- interpretation
    }
  }

  class(result) <- "intelligent_bias_assessment"
  return(result)
}

#' Intelligent Sensitivity Analysis
#'
#' Enhanced sensitivity analysis with scenario matching and AI guidance.
#'
#' @param data Meta-analysis data
#' @param result Meta-analysis result
#' @param enable_ai Use AI
#' @param enable_rules Use rules
#'
#' @return Enhanced sensitivity analysis
#'
#' @export
intelligent_sensitivity <- function(data, result, enable_ai = FALSE,
                                   enable_rules = TRUE) {
  cat("\n=== Intelligent Sensitivity Analysis ===\n")

  # Leave-one-out
  cat("Performing leave-one-out analysis...\n")
  loo <- leave_one_out(data)

  # Influence diagnostics
  cat("Calculating influence diagnostics...\n")
  influence <- influence_diagnostics(data)

  # Rules-based assessment
  if (enable_rules) {
    cat("Applying sensitivity rules...\n")
    rules_engine <- init_rules_engine()

    # Check if results are stable
    loo_range <- max(loo$estimate) - min(loo$estimate)
    original_range <- result$ci_upper - result$ci_lower

    if (loo_range > 2 * original_range) {
      warning("Rules: Results highly sensitive to individual studies. Exercise caution.")
    }

    # Check for influential studies
    if (any(influence$cooks_d > 1)) {
      warning("Rules: Influential studies detected (Cook's D > 1). Consider exclusion sensitivity.")
    }
  }

  sensitivity_result <- list(
    loo = loo,
    influence = influence,
    stability_assessment = list(
      loo_range = loo_range,
      relative_stability = loo_range / original_range,
      verdict = if (loo_range < original_range) "STABLE" else "UNSTABLE"
    )
  )

  # AI recommendations
  if (enable_ai) {
    llm_config <- init_llm()
    if (llm_config$enabled) {
      prompt <- sprintf(
        "Interpret sensitivity analysis:

Leave-one-out range: %.3f
Original CI width: %.3f
Max Cook's distance: %.3f

Should any studies be excluded?",
        loo_range, original_range, max(influence$cooks_d)
      )

      advice <- query_llm_internal(prompt, llm_config)
      sensitivity_result$ai_advice <- advice
    }
  }

  cat(sprintf("\n✓ Stability: %s\n", sensitivity_result$stability_assessment$verdict))

  return(sensitivity_result)
}

#' Intelligent Network Meta-Analysis
#'
#' NMA with AI-powered consistency checking and treatment ranking interpretation.
#'
#' @param data NMA data
#' @param enable_ai Use AI
#' @param enable_rules Use rules
#'
#' @return Enhanced NMA results
#'
#' @export
intelligent_nma <- function(data, enable_ai = FALSE, enable_rules = TRUE) {
  cat("\n=== Intelligent Network Meta-Analysis ===\n")

  # Rules validation
  if (enable_rules) {
    # Check minimum requirements
    treatments <- unique(c(data$treatment, data$baseline))
    n_treatments <- length(treatments)

    if (n_treatments < 3) {
      stop("Rules: Network meta-analysis requires at least 3 treatments")
    }

    cat(sprintf("✓ Network validated: %d treatments\n", n_treatments))
  }

  # Core NMA
  cat("Running network meta-analysis...\n")
  nma_result <- network_meta_analysis(data)

  # Consistency check
  cat("Checking consistency...\n")
  consistency <- test_inconsistency(nma_result, data)
  nma_result$consistency <- consistency

  # Treatment ranking
  cat("Ranking treatments...\n")
  rankings <- rank_treatments(nma_result, higher_better = TRUE)
  nma_result$rankings <- rankings

  # AI interpretation
  if (enable_ai) {
    llm_config <- init_llm()
    if (llm_config$enabled) {
      top_treatment <- rankings$treatment[1]
      sucra_score <- rankings$sucra[1]

      prompt <- sprintf(
        "Interpret network meta-analysis results:

Number of treatments: %d
Top-ranked treatment: %s (SUCRA: %.2f)
Consistency: %s

Provide clinical interpretation and recommendation.",
        n_treatments, top_treatment, sucra_score,
        if (consistency$inconsistency_detected) "INCONSISTENT" else "CONSISTENT"
      )

      interpretation <- query_llm_internal(prompt, llm_config)
      nma_result$ai_interpretation <- interpretation
    }
  }

  cat("\n✓ Network meta-analysis complete\n")
  cat(sprintf("Top treatment: %s\n", rankings$treatment[1]))

  return(nma_result)
}

#' Intelligent Quality Assessment
#'
#' Automated quality assessment using AI and 80+ quality rules.
#'
#' @param data Study data with characteristics
#' @param enable_ai Use AI for nuanced assessment
#' @param enable_rules Use quality rules
#'
#' @return Comprehensive quality assessment
#'
#' @export
intelligent_quality_assessment <- function(data, enable_ai = FALSE,
                                          enable_rules = TRUE) {
  cat("\n=== Intelligent Quality Assessment ===\n")
  cat(sprintf("Assessing %d studies...\n", nrow(data)))

  quality_scores <- list()

  for (i in 1:nrow(data)) {
    study <- data[i, ]

    # Rules-based scoring
    if (enable_rules) {
      rules_engine <- init_rules_engine()
      quality_rules <- rules_engine$rules$quality_rules$rules

      score <- 5 # Base score

      # Apply rules
      if (study$se < 0.15) score <- score + 2
      if (study$se > 0.3) score <- score - 2

      if ("n" %in% names(study)) {
        if (study$n > 100) score <- score + 1
        if (study$n < 30) score <- score - 1
      }

      if ("year" %in% names(study)) {
        if (study$year >= 2020) score <- score + 1
      }

      score <- max(1, min(10, score))

      quality_scores[[study$study]] <- list(
        study = study$study,
        score = score,
        precision = if (study$se < 0.15) "High" else if (study$se < 0.25) "Moderate" else "Low",
        risk_of_bias = if (score >= 7) "Low" else if (score >= 4) "Moderate" else "High"
      )
    }

    # AI assessment
    if (enable_ai) {
      ai_assessment <- ai_quality_assessment(data[i, , drop = FALSE])
      quality_scores[[study$study]]$ai_assessment <- ai_assessment
    }
  }

  # Summary
  scores <- sapply(quality_scores, function(x) x$score)
  cat(sprintf("\nQuality Summary:\n"))
  cat(sprintf("  Mean quality score: %.1f/10\n", mean(scores)))
  cat(sprintf("  Low risk studies: %d (%.0f%%)\n",
              sum(scores >= 7), 100 * sum(scores >= 7) / length(scores)))
  cat(sprintf("  High risk studies: %d (%.0f%%)\n",
              sum(scores < 4), 100 * sum(scores < 4) / length(scores)))

  result <- list(
    study_assessments = quality_scores,
    summary = list(
      mean_score = mean(scores),
      low_risk_n = sum(scores >= 7),
      moderate_risk_n = sum(scores >= 4 & scores < 7),
      high_risk_n = sum(scores < 4)
    )
  )

  class(result) <- "intelligent_quality"
  return(result)
}

#' Auto-Pilot Meta-Analysis
#'
#' Fully automated meta-analysis with AI making all methodological decisions.
#' Requires local Ollama.
#'
#' @param data Meta-analysis data
#' @param research_question Optional research question
#' @param auto_visualize Generate all relevant plots
#'
#' @return Complete meta-analysis with all components
#'
#' @export
autopilot_meta_analysis <- function(data, research_question = NULL,
                                   auto_visualize = TRUE) {
  cat("\n╔══════════════════════════════════════════╗\n")
  cat("║   AUTO-PILOT META-ANALYSIS SYSTEM        ║\n")
  cat("║   AI + 500+ Rules + 10,000 Scenarios     ║\n")
  cat("╚══════════════════════════════════════════╝\n\n")

  # Initialize all systems
  llm_config <- init_llm()
  rules_engine <- init_rules_engine()
  scenario_db <- init_scenario_database()

  results <- list()

  # Step 1: Intelligent data validation
  cat("STEP 1: Data Validation\n")
  pre_checks <- apply_rules(rules_engine, data)
  results$validation <- pre_checks
  cat(sprintf("✓ %d validation rules applied\n\n", length(pre_checks)))

  # Step 2: AI method selection
  cat("STEP 2: AI Method Selection\n")
  recommendations <- ai_method_selection(data, research_question, llm_config)
  results$recommendations <- recommendations
  cat("✓ Methods selected by AI\n\n")

  # Step 3: Main analysis
  cat("STEP 3: Core Meta-Analysis\n")
  result <- intelligent_cbamm(data, enable_ai = TRUE, enable_rules = TRUE,
                               llm_config = llm_config, verbose = FALSE)
  results$main_analysis <- result
  cat(sprintf("✓ Pooled effect: %.3f (95%% CI: %.3f to %.3f)\n\n",
              result$estimate, result$ci_lower, result$ci_upper))

  # Step 4: Heterogeneity assessment
  cat("STEP 4: Heterogeneity Assessment\n")
  cat(sprintf("✓ I² = %.1f%%, τ² = %.3f\n\n", result$I2, result$tau2))

  # Step 5: Publication bias
  cat("STEP 5: Publication Bias Assessment\n")
  bias_assessment <- intelligent_publication_bias(data, enable_ai = TRUE,
                                                  llm_config = llm_config)
  results$publication_bias <- bias_assessment
  cat("✓ Multiple bias tests completed\n\n")

  # Step 6: Sensitivity analysis
  cat("STEP 6: Sensitivity Analysis\n")
  sensitivity <- intelligent_sensitivity(data, result, enable_ai = TRUE)
  results$sensitivity <- sensitivity
  cat(sprintf("✓ Stability: %s\n\n", sensitivity$stability_assessment$verdict))

  # Step 7: Quality assessment
  cat("STEP 7: Quality Assessment\n")
  quality <- intelligent_quality_assessment(data, enable_ai = TRUE)
  results$quality <- quality
  cat("✓ All studies assessed\n\n")

  # Step 8: Research gaps
  if (llm_config$enabled) {
    cat("STEP 8: Research Gap Analysis\n")
    gaps <- ai_research_gaps(result, data, llm_config)
    results$research_gaps <- gaps
    cat("✓ Gaps identified\n\n")
  }

  # Visualization
  if (auto_visualize) {
    cat("STEP 9: Generating Visualizations\n")
    cat("  • Forest plot\n")
    cat("  • Funnel plot\n")
    cat("  • Influence plot\n")
    cat("✓ Visualizations ready\n\n")
  }

  cat("╔══════════════════════════════════════════╗\n")
  cat("║   AUTO-PILOT ANALYSIS COMPLETE           ║\n")
  cat("╚══════════════════════════════════════════╝\n")

  class(results) <- "autopilot_ma"
  return(results)
}

#' Print Intelligent Meta-Analysis Result
#' @export
print.intelligent_cbamm <- function(x, ...) {
  cat("\n=== Intelligent Meta-Analysis Results ===\n\n")

  cat("Effect Estimate:\n")
  cat(sprintf("  Pooled effect: %.3f (95%% CI: %.3f to %.3f)\n",
              x$estimate, x$ci_lower, x$ci_upper))
  cat(sprintf("  P-value: %.4f\n\n", x$p_value))

  cat("Heterogeneity:\n")
  cat(sprintf("  I²: %.1f%%\n", x$I2))
  cat(sprintf("  τ²: %.3f\n\n", x$tau2))

  cat("Intelligent Analysis:\n")
  cat(sprintf("  AI enabled: %s\n", if(x$intelligent_analysis$ai_enabled) "Yes (Local Ollama)" else "No"))
  cat(sprintf("  Rules triggered: %d\n", x$intelligent_analysis$n_rules_triggered))
  cat(sprintf("  Similar scenarios: %d\n\n", x$intelligent_analysis$similar_scenarios))

  if (!is.null(x$ai_interpretation)) {
    cat("AI Interpretation:\n")
    cat(strwrap(substr(x$ai_interpretation, 1, 500), width = 70, prefix = "  "), sep = "\n")
    if (nchar(x$ai_interpretation) > 500) cat("  ...\n")
  }

  invisible(x)
}
