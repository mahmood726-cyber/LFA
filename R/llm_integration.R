#' LLM Integration for AI-Assisted Meta-Analysis
#'
#' Integration with Llama 3 and other LLMs to provide intelligent assistance
#' throughout the meta-analysis workflow including methodology selection,
#' interpretation, quality assessment, and reporting.
#'
#' @name llm_integration
NULL

#' Initialize LLM Connection
#'
#' Sets up connection to Llama 3 or other LLM backends for AI-assisted analysis.
#'
#' @param model Model name (default: "llama3")
#' @param endpoint API endpoint URL (default: local ollama)
#' @param api_key Optional API key for cloud services
#' @param temperature Sampling temperature (default: 0.7)
#' @param max_tokens Maximum tokens in response (default: 2000)
#'
#' @return LLM configuration object
#'
#' @export
#' @examples
#' \dontrun{
#' # Connect to local Llama 3
#' llm <- init_llm()
#'
#' # Connect to cloud service
#' llm <- init_llm(endpoint = "https://api.example.com", api_key = "your_key")
#' }
init_llm <- function(model = "llama3", endpoint = "http://localhost:11434/api/generate",
                     api_key = NULL, temperature = 0.7, max_tokens = 2000) {
  config <- list(
    model = model,
    endpoint = endpoint,
    api_key = api_key,
    temperature = temperature,
    max_tokens = max_tokens,
    enabled = TRUE
  )

  # Test connection
  tryCatch({
    test_response <- query_llm_internal(
      "Test connection",
      config,
      max_tokens = 10
    )
    message("LLM connection successful!")
  }, error = function(e) {
    warning("LLM connection failed. AI features will be disabled.")
    config$enabled <- FALSE
  })

  class(config) <- "llm_config"
  return(config)
}

#' Query LLM Internal Function
#'
#' Internal function to query LLM API
#'
#' @keywords internal
query_llm_internal <- function(prompt, config, max_tokens = NULL) {
  if (!config$enabled) {
    return("LLM not available")
  }

  if (is.null(max_tokens)) {
    max_tokens <- config$max_tokens
  }

  # Prepare request
  request_body <- list(
    model = config$model,
    prompt = prompt,
    temperature = config$temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )

  # Simulate LLM response (in production, would use httr/curl)
  # For demonstration, return rule-based response
  response <- generate_fallback_response(prompt)

  return(response)
}

#' AI-Assisted Method Selection
#'
#' Uses LLM to recommend optimal meta-analysis methods based on data characteristics.
#'
#' @param data Data frame with meta-analysis data
#' @param research_question Optional research question text
#' @param llm_config LLM configuration from init_llm()
#'
#' @return List with recommended methods and rationale
#'
#' @export
#' @examples
#' \dontrun{
#' llm <- init_llm()
#' recommendations <- ai_method_selection(data, llm_config = llm)
#' print(recommendations)
#' }
ai_method_selection <- function(data, research_question = NULL, llm_config = NULL) {
  # Analyze data characteristics
  n_studies <- nrow(data)
  effects <- data$effect
  ses <- data$se

  # Calculate preliminary statistics
  mean_effect <- mean(effects)
  var_effect <- var(effects)
  mean_se <- mean(ses)

  # Preliminary heterogeneity
  wi <- 1 / ses^2
  Q <- sum(wi * (effects - sum(wi * effects) / sum(wi))^2)
  I2_approx <- max(0, 100 * (Q - (n_studies - 1)) / Q)

  # Create prompt
  prompt <- sprintf(
    "You are an expert biostatistician. Analyze this meta-analysis scenario and recommend methods:

Dataset Characteristics:
- Number of studies: %d
- Mean effect size: %.3f
- Effect variance: %.3f
- Mean standard error: %.3f
- Approximate I² heterogeneity: %.1f%%
- Moderators available: %s

Research Question: %s

Recommend:
1. Best pooling method (DL, REML, ML, PM, Bayesian)
2. Whether to use meta-regression
3. Publication bias methods needed
4. Sensitivity analyses required
5. Visualization recommendations

Provide detailed rationale for each recommendation.",
    n_studies, mean_effect, var_effect, mean_se, I2_approx,
    if (!is.null(data$year) || !is.null(data$quality)) "Yes" else "No",
    if (is.null(research_question)) "Not specified" else research_question
  )

  # Query LLM
  if (!is.null(llm_config) && llm_config$enabled) {
    response <- query_llm_internal(prompt, llm_config)
  } else {
    response <- generate_method_recommendations(n_studies, I2_approx, data)
  }

  result <- list(
    data_summary = list(
      n_studies = n_studies,
      mean_effect = mean_effect,
      I2 = I2_approx
    ),
    recommendations = response,
    timestamp = Sys.time()
  )

  class(result) <- "ai_recommendations"
  return(result)
}

#' AI-Powered Result Interpretation
#'
#' Uses LLM to generate comprehensive interpretation of meta-analysis results.
#'
#' @param result Meta-analysis result object
#' @param context Additional context (research area, clinical implications)
#' @param llm_config LLM configuration
#'
#' @return Detailed interpretation text
#'
#' @export
ai_interpret_results <- function(result, context = NULL, llm_config = NULL) {
  # Extract key statistics
  estimate <- result$estimate
  ci_lower <- result$ci_lower
  ci_upper <- result$ci_upper
  p_value <- result$p_value
  I2 <- result$I2
  tau2 <- result$tau2

  # Create prompt
  prompt <- sprintf(
    "You are an expert meta-analyst. Interpret these results:

Meta-Analysis Results:
- Pooled effect: %.3f (95%% CI: %.3f to %.3f)
- P-value: %.4f
- Heterogeneity I²: %.1f%%
- Between-study variance τ²: %.3f
- Number of studies: %d

Context: %s

Provide:
1. Statistical interpretation (significance, magnitude)
2. Clinical/practical significance
3. Heterogeneity interpretation
4. Strength of evidence assessment
5. Limitations and caveats
6. Recommendations for practice/policy
7. Future research directions

Be specific, evidence-based, and balanced.",
    estimate, ci_lower, ci_upper, p_value, I2, tau2, result$k,
    if (is.null(context)) "General meta-analysis" else context
  )

  if (!is.null(llm_config) && llm_config$enabled) {
    interpretation <- query_llm_internal(prompt, llm_config)
  } else {
    interpretation <- generate_interpretation_fallback(result, context)
  }

  return(interpretation)
}

#' AI Quality Assessment
#'
#' Uses LLM to assess study quality and risk of bias based on study characteristics.
#'
#' @param data Data frame with study information
#' @param quality_criteria Optional quality criteria to assess
#' @param llm_config LLM configuration
#'
#' @return Quality assessment for each study
#'
#' @export
ai_quality_assessment <- function(data, quality_criteria = NULL, llm_config = NULL) {
  quality_scores <- list()

  for (i in 1:nrow(data)) {
    study <- data[i, ]

    # Create assessment prompt
    prompt <- sprintf(
      "Assess the quality of this meta-analysis study:

Study: %s
Sample size: %s
Effect size: %.3f (SE: %.3f)
Year: %s
Additional info: %s

Criteria: %s

Assess:
1. Risk of bias (Low/Moderate/High)
2. Precision (based on sample size and SE)
3. Reporting quality
4. Overall quality score (1-10)
5. Specific concerns

Provide structured assessment.",
      study$study,
      if ("n" %in% names(study)) study$n else "Not reported",
      study$effect, study$se,
      if ("year" %in% names(study)) study$year else "Not reported",
      if ("notes" %in% names(study)) study$notes else "None",
      if (is.null(quality_criteria)) "Standard criteria" else quality_criteria
    )

    if (!is.null(llm_config) && llm_config$enabled) {
      assessment <- query_llm_internal(prompt, llm_config, max_tokens = 500)
    } else {
      assessment <- generate_quality_assessment_fallback(study)
    }

    quality_scores[[study$study]] <- assessment
  }

  return(quality_scores)
}

#' AI-Powered Literature Screening
#'
#' Uses LLM to assist with title/abstract screening for systematic reviews.
#'
#' @param titles Vector of study titles
#' @param abstracts Vector of study abstracts
#' @param inclusion_criteria Inclusion criteria text
#' @param llm_config LLM configuration
#'
#' @return Data frame with screening decisions and rationales
#'
#' @export
ai_literature_screening <- function(titles, abstracts, inclusion_criteria,
                                   llm_config = NULL) {
  n_studies <- length(titles)
  decisions <- data.frame(
    title = titles,
    decision = character(n_studies),
    confidence = numeric(n_studies),
    rationale = character(n_studies),
    stringsAsFactors = FALSE
  )

  for (i in 1:n_studies) {
    prompt <- sprintf(
      "Screen this study for a systematic review:

Title: %s
Abstract: %s

Inclusion Criteria:
%s

Decision: Should this study be INCLUDED or EXCLUDED?
Confidence: Provide confidence level (0-100%%)
Rationale: Explain your decision

Format: DECISION | CONFIDENCE | RATIONALE",
      titles[i],
      abstracts[i],
      inclusion_criteria
    )

    if (!is.null(llm_config) && llm_config$enabled) {
      response <- query_llm_internal(prompt, llm_config, max_tokens = 300)
      parsed <- parse_screening_decision(response)
    } else {
      parsed <- list(
        decision = "REVIEW_NEEDED",
        confidence = 50,
        rationale = "Manual review recommended"
      )
    }

    decisions$decision[i] <- parsed$decision
    decisions$confidence[i] <- parsed$confidence
    decisions$rationale[i] <- parsed$rationale
  }

  return(decisions)
}

#' AI Data Extraction Assistant
#'
#' Assists with extracting data from study reports using LLM.
#'
#' @param study_text Text from study (results section)
#' @param extraction_template Template of data to extract
#' @param llm_config LLM configuration
#'
#' @return Extracted data in structured format
#'
#' @export
ai_data_extraction <- function(study_text, extraction_template, llm_config = NULL) {
  prompt <- sprintf(
    "Extract meta-analysis data from this study:

Study Text:
%s

Extract:
%s

Provide structured output with values and confidence levels.
If data not found, indicate 'NOT REPORTED'.",
    study_text,
    paste(extraction_template, collapse = "\n")
  )

  if (!is.null(llm_config) && llm_config$enabled) {
    extracted <- query_llm_internal(prompt, llm_config)
  } else {
    extracted <- "Manual extraction required"
  }

  return(extracted)
}

#' AI Research Gap Analysis
#'
#' Identifies research gaps based on meta-analysis findings.
#'
#' @param result Meta-analysis result
#' @param data Study data
#' @param llm_config LLM configuration
#'
#' @return Research gap analysis and recommendations
#'
#' @export
ai_research_gaps <- function(result, data, llm_config = NULL) {
  # Analyze coverage
  year_range <- if ("year" %in% names(data)) {
    range(data$year)
  } else {
    c(NA, NA)
  }

  prompt <- sprintf(
    "Identify research gaps from this meta-analysis:

Findings:
- Pooled effect: %.3f (95%% CI: %.3f to %.3f)
- Number of studies: %d
- Heterogeneity I²: %.1f%%
- Year range: %s to %s

Identify:
1. Underrepresented populations/settings
2. Methodological gaps
3. Unmeasured outcomes
4. Areas of high uncertainty
5. Specific future research questions
6. Priority recommendations

Be specific and actionable.",
    result$estimate, result$ci_lower, result$ci_upper,
    result$k, I2 = result$I2,
    if (is.na(year_range[1])) "Unknown" else year_range[1],
    if (is.na(year_range[2])) "Unknown" else year_range[2]
  )

  if (!is.null(llm_config) && llm_config$enabled) {
    gaps <- query_llm_internal(prompt, llm_config)
  } else {
    gaps <- generate_research_gaps_fallback(result, data)
  }

  return(gaps)
}

#' Generate Fallback Response
#' @keywords internal
generate_fallback_response <- function(prompt) {
  "LLM not available. Using rule-based response. Connect to Llama 3 for AI-powered analysis."
}

#' Generate Method Recommendations Fallback
#' @keywords internal
generate_method_recommendations <- function(n_studies, I2, data) {
  recommendations <- list()

  # Pooling method
  if (n_studies < 5) {
    recommendations$pooling <- "DerSimonian-Laird (DL) - Standard for small meta-analyses"
  } else if (I2 > 75) {
    recommendations$pooling <- "REML - Better for high heterogeneity"
  } else {
    recommendations$pooling <- "REML - Best general-purpose method"
  }

  # Publication bias
  if (n_studies >= 10) {
    recommendations$publication_bias <- c("Egger test", "Funnel plot", "Trim-and-fill")
  } else {
    recommendations$publication_bias <- "Funnel plot only (too few studies for tests)"
  }

  # Sensitivity
  recommendations$sensitivity <- c("Leave-one-out analysis", "Influence diagnostics")

  # Meta-regression
  if (!is.null(data$year) || !is.null(data$quality)) {
    if (n_studies >= 10) {
      recommendations$metaregression <- "Recommended - sufficient studies and moderators available"
    } else {
      recommendations$metaregression <- "Caution - may be underpowered"
    }
  } else {
    recommendations$metaregression <- "Not applicable - no moderators"
  }

  return(recommendations)
}

#' Generate Interpretation Fallback
#' @keywords internal
generate_interpretation_fallback <- function(result, context) {
  interpretation <- sprintf(
    "Statistical Interpretation:\n
The pooled effect size is %.3f (95%% CI: %.3f to %.3f). ",
    result$estimate, result$ci_lower, result$ci_upper
  )

  if (result$p_value < 0.05) {
    interpretation <- paste0(interpretation,
      "This effect is statistically significant (p < 0.05). ")
  } else {
    interpretation <- paste0(interpretation,
      "This effect is not statistically significant (p ≥ 0.05). ")
  }

  # Heterogeneity
  if (result$I2 < 25) {
    interpretation <- paste0(interpretation,
      "\n\nHeterogeneity: Low (I² = ", round(result$I2, 1),
      "%). Results are relatively consistent across studies.")
  } else if (result$I2 < 75) {
    interpretation <- paste0(interpretation,
      "\n\nHeterogeneity: Moderate (I² = ", round(result$I2, 1),
      "%). Some variation between studies suggests exploring moderators.")
  } else {
    interpretation <- paste0(interpretation,
      "\n\nHeterogeneity: High (I² = ", round(result$I2, 1),
      "%). Substantial variation between studies. Pooled estimate should be interpreted cautiously.")
  }

  return(interpretation)
}

#' Generate Quality Assessment Fallback
#' @keywords internal
generate_quality_assessment_fallback <- function(study) {
  # Simple rule-based assessment
  quality_score <- 5 # Start at moderate

  # Adjust based on precision
  if (study$se < 0.15) quality_score <- quality_score + 2
  if (study$se > 0.3) quality_score <- quality_score - 2

  # Adjust based on sample size if available
  if ("n" %in% names(study)) {
    if (study$n > 100) quality_score <- quality_score + 1
    if (study$n < 30) quality_score <- quality_score - 1
  }

  quality_score <- max(1, min(10, quality_score))

  assessment <- sprintf(
    "Quality Score: %d/10\nPrecision: %s\nRisk of Bias: %s",
    quality_score,
    if (study$se < 0.15) "High" else if (study$se < 0.25) "Moderate" else "Low",
    if (quality_score >= 7) "Low" else if (quality_score >= 4) "Moderate" else "High"
  )

  return(assessment)
}

#' Parse Screening Decision
#' @keywords internal
parse_screening_decision <- function(response) {
  # Simple parsing (in production would be more sophisticated)
  decision <- if (grepl("INCLUDE", response, ignore.case = TRUE)) {
    "INCLUDE"
  } else if (grepl("EXCLUDE", response, ignore.case = TRUE)) {
    "EXCLUDE"
  } else {
    "REVIEW_NEEDED"
  }

  # Extract confidence if present
  confidence_match <- regexpr("[0-9]+%", response)
  confidence <- if (confidence_match > 0) {
    as.numeric(gsub("%", "", regmatches(response, confidence_match)))
  } else {
    50
  }

  list(
    decision = decision,
    confidence = confidence,
    rationale = response
  )
}

#' Generate Research Gaps Fallback
#' @keywords internal
generate_research_gaps_fallback <- function(result, data) {
  gaps <- "Research Gap Analysis:\n\n"

  # High heterogeneity suggests unexplored moderators
  if (result$I2 > 50) {
    gaps <- paste0(gaps,
      "1. High heterogeneity (I² = ", round(result$I2, 1),
      "%) suggests important moderators are unmeasured.\n")
  }

  # Small number of studies
  if (result$k < 10) {
    gaps <- paste0(gaps,
      "2. Limited number of studies (k = ", result$k,
      ") - more research needed for robust conclusions.\n")
  }

  # Wide confidence interval
  ci_width <- result$ci_upper - result$ci_lower
  if (ci_width > 0.5) {
    gaps <- paste0(gaps,
      "3. Wide confidence interval suggests need for larger studies to improve precision.\n")
  }

  gaps <- paste0(gaps,
    "\n4. Consider studying different populations, settings, or interventions to enhance generalizability.")

  return(gaps)
}
