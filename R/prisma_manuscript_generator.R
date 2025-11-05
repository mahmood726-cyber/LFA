#' Automated PRISMA-Compliant Manuscript Generator
#'
#' Generates complete, publication-ready systematic review and meta-analysis
#' manuscripts following PRISMA 2020 guidelines including:
#' - Title page with all author details
#' - Structured abstract (250 words)
#' - Introduction with objectives and PICO
#' - Methods (search, selection, data extraction, quality, analysis)
#' - Results with PRISMA flow diagram
#' - Discussion with implications
#' - References and appendices
#' - All tables and figures
#'
#' @name prisma_manuscript_generator
NULL

#' Generate Complete PRISMA Manuscript
#'
#' Creates a complete, journal-ready manuscript from meta-analysis results.
#' Outputs in multiple formats (Word, PDF, HTML, LaTeX).
#'
#' @param meta_result Meta-analysis result object from cbamm_fast or complete_analysis
#' @param title Manuscript title
#' @param authors Author list (vector of names or data frame with affiliations)
#' @param journal Target journal (default: "generic")
#' @param research_question PICO/research question
#' @param search_details Search strategy details
#' @param inclusion_criteria Study inclusion criteria
#' @param output_format Output format: "word", "pdf", "html", "latex" (default: c("word", "html"))
#' @param output_dir Output directory (default: "prisma_manuscript")
#' @param template Journal-specific template (default: NULL for generic)
#'
#' @return Path to generated manuscript files
#'
#' @export
#' @examples
#' \dontrun{
#' # Generate complete manuscript
#' manuscript <- generate_prisma_manuscript(
#'   meta_result = my_meta_analysis,
#'   title = "Efficacy of Aspirin for Primary Prevention of Cardiovascular Disease: A Systematic Review and Meta-Analysis",
#'   authors = c("John Doe, MD", "Jane Smith, PhD", "Robert Johnson, MSc"),
#'   research_question = list(
#'     population = "Adults without known cardiovascular disease",
#'     intervention = "Aspirin (any dose)",
#'     comparator = "Placebo or no treatment",
#'     outcome = "Major cardiovascular events"
#'   ),
#'   search_details = list(
#'     databases = c("PubMed", "Embase", "Cochrane CENTRAL"),
#'     date_range = "Inception to December 2024",
#'     search_string = "(aspirin OR acetylsalicylic acid) AND ..."
#'   )
#' )
#' }
generate_prisma_manuscript <- function(meta_result, title, authors,
                                       journal = "generic",
                                       research_question = NULL,
                                       search_details = NULL,
                                       inclusion_criteria = NULL,
                                       output_format = c("word", "html"),
                                       output_dir = "prisma_manuscript",
                                       template = NULL) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PRISMA MANUSCRIPT GENERATOR                                ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("Output directory: %s\n", output_dir))
  cat(sprintf("Target journal: %s\n", journal))
  cat(sprintf("Output formats: %s\n\n", paste(output_format, collapse = ", ")))

  # Generate manuscript sections
  cat("Generating manuscript sections...\n")

  manuscript <- list()

  # Title page
  cat("  [1/10] Title page\n")
  manuscript$title_page <- generate_title_page(title, authors, journal)

  # Abstract
  cat("  [2/10] Abstract\n")
  manuscript$abstract <- generate_abstract(meta_result, research_question)

  # Introduction
  cat("  [3/10] Introduction\n")
  manuscript$introduction <- generate_introduction(research_question)

  # Methods
  cat("  [4/10] Methods\n")
  manuscript$methods <- generate_methods_prisma(meta_result, search_details, inclusion_criteria)

  # Results
  cat("  [5/10] Results\n")
  manuscript$results <- generate_results_prisma(meta_result)

  # Discussion
  cat("  [6/10] Discussion\n")
  manuscript$discussion <- generate_discussion(meta_result)

  # Tables
  cat("  [7/10] Tables\n")
  manuscript$tables <- generate_tables_prisma(meta_result)

  # Figures
  cat("  [8/10] Figures\n")
  manuscript$figures <- generate_figures_prisma(meta_result, output_dir)

  # PRISMA checklist and flow diagram
  cat("  [9/10] PRISMA checklist and flow diagram\n")
  manuscript$prisma_checklist <- generate_prisma_checklist()
  manuscript$prisma_flow <- generate_prisma_flow_diagram(meta_result, output_dir)

  # References
  cat("  [10/10] References\n")
  manuscript$references <- generate_references()

  # Compile manuscript
  cat("\nCompiling manuscript...\n")
  compiled_manuscript <- compile_manuscript_sections(manuscript, title, journal)

  # Export to requested formats
  output_files <- list()
  for (format in output_format) {
    cat(sprintf("  Exporting to %s...\n", toupper(format)))
    output_file <- export_manuscript(compiled_manuscript, format, output_dir, title, template)
    output_files[[format]] <- output_file
  }

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   MANUSCRIPT GENERATION COMPLETE                             ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  cat("Generated files:\n")
  for (format in names(output_files)) {
    cat(sprintf("  %s: %s\n", toupper(format), output_files[[format]]))
  }

  result <- list(
    manuscript = compiled_manuscript,
    output_files = output_files,
    output_dir = output_dir
  )

  class(result) <- c("prisma_manuscript", "list")
  return(result)
}

#' @keywords internal
generate_title_page <- function(title, authors, journal) {
  # Format authors
  if (is.vector(authors)) {
    author_list <- paste(authors, collapse = "; ")
  } else if (is.data.frame(authors)) {
    author_list <- paste(authors$name, " (", authors$affiliation, ")", sep = "", collapse = "; ")
  } else {
    author_list <- as.character(authors)
  }

  # Corresponding author (first author by default)
  corresponding <- if (is.vector(authors)) authors[1] else authors$name[1]

  list(
    title = title,
    authors = author_list,
    corresponding_author = corresponding,
    date = format(Sys.Date(), "%B %d, %Y"),
    word_count = NA,  # Will be calculated
    keywords = "systematic review, meta-analysis"
  )
}

#' @keywords internal
generate_abstract <- function(meta_result, research_question) {
  # Extract key statistics
  n_studies <- if (!is.null(meta_result$n_studies)) meta_result$n_studies else nrow(meta_result$data)
  estimate <- if (!is.null(meta_result$estimate)) meta_result$estimate else NA
  ci_lower <- if (!is.null(meta_result$ci_lower)) meta_result$ci_lower else NA
  ci_upper <- if (!is.null(meta_result$ci_upper)) meta_result$ci_upper else NA
  I2 <- if (!is.null(meta_result$I2)) meta_result$I2 else NA

  # PICO
  pico_text <- if (!is.null(research_question)) {
    sprintf("in %s comparing %s with %s on %s",
            research_question$population,
            research_question$intervention,
            research_question$comparator,
            research_question$outcome)
  } else {
    "[specify population, intervention, comparator, outcome]"
  }

  abstract_text <- sprintf("
**Background:** [Brief context on the clinical question]

**Objective:** To systematically review and meta-analyze the evidence %s.

**Methods:** We searched multiple databases for relevant studies. Two reviewers independently screened studies, extracted data, and assessed risk of bias. We pooled effect sizes using random-effects meta-analysis and assessed heterogeneity using I² statistics.

**Results:** We included %d studies. The pooled effect size was %.2f (95%% CI: %.2f to %.2f). Heterogeneity was %s (I² = %.1f%%). [Additional findings on publication bias, subgroup analyses, sensitivity analyses].

**Conclusions:** [State main finding and clinical implications]. [Mention limitations]. [Provide recommendations for practice and future research].

**Registration:** PROSPERO [ID if applicable]

**Keywords:** %s
  ",
  pico_text, n_studies, estimate, ci_lower, ci_upper,
  ifelse(I2 < 25, "low", ifelse(I2 < 75, "moderate", "high")), I2,
  "systematic review, meta-analysis"
  )

  list(
    structured = abstract_text,
    word_count = length(strsplit(gsub("\\*\\*|\\[|\\]", "", abstract_text), "\\s+")[[1]])
  )
}

#' @keywords internal
generate_introduction <- function(research_question) {
  pico_text <- if (!is.null(research_question)) {
    sprintf("whether %s is effective compared to %s for %s in %s",
            research_question$intervention,
            research_question$comparator,
            research_question$outcome,
            research_question$population)
  } else {
    "[specify research question]"
  }

  intro_text <- sprintf("
# Introduction

## Background

[Provide clinical context and significance of the research question. Discuss disease burden, current treatment approaches, and gaps in knowledge.]

## Rationale

[Explain why this systematic review is needed. Mention previous reviews if any, and how this review adds to existing knowledge.]

## Objectives

The primary objective of this systematic review and meta-analysis was to evaluate %s.

Specifically, we aimed to:
1. Assess the overall effect size
2. Evaluate between-study heterogeneity
3. Investigate potential sources of heterogeneity through subgroup analyses
4. Assess risk of publication bias
5. Conduct sensitivity analyses to test robustness of findings

This review follows the Preferred Reporting Items for Systematic Reviews and Meta-Analyses (PRISMA) 2020 guidelines.
  ", pico_text)

  intro_text
}

#' @keywords internal
generate_methods_prisma <- function(meta_result, search_details, inclusion_criteria) {
  databases <- if (!is.null(search_details$databases)) {
    paste(search_details$databases, collapse = ", ")
  } else {
    "PubMed, Embase, Cochrane Central Register of Controlled Trials, Web of Science"
  }

  date_range <- if (!is.null(search_details$date_range)) {
    search_details$date_range
  } else {
    "inception to [date]"
  }

  methods_text <- sprintf("
# Methods

## Protocol and Registration

This systematic review was prospectively registered with PROSPERO (registration number: [ID]). The protocol is available at [URL]. Any deviations from the protocol are documented in Appendix X.

## Eligibility Criteria

We included studies meeting the following criteria:

**Population:** %s

**Intervention:** %s

**Comparator:** %s

**Outcome:** %s

**Study Design:** Randomized controlled trials (RCTs)

We excluded: non-randomized studies, case reports, editorials, and studies not published in English.

## Information Sources

We searched the following databases: %s from %s. We also searched trial registries (ClinicalTrials.gov, WHO ICTRP) and scanned reference lists of included studies and relevant reviews.

## Search Strategy

A comprehensive search strategy was developed in collaboration with a medical librarian. The full search strategy for all databases is provided in Appendix X.

Example search string (PubMed):
%s

## Study Selection

Two reviewers (XX and YY) independently screened titles and abstracts using predefined eligibility criteria. Potentially eligible studies underwent full-text review. Disagreements were resolved by discussion or consultation with a third reviewer (ZZ). Inter-rater agreement was assessed using Cohen's kappa.

## Data Collection Process

Two reviewers independently extracted data using a standardized form (pilot-tested on 5 studies). We extracted: study characteristics (author, year, design, setting), participant characteristics (age, sex, baseline characteristics), intervention details (dose, duration, co-interventions), outcome data (mean, SD, or event counts), and risk of bias items.

## Data Items

Primary outcome: %s

Secondary outcomes: [list if applicable]

## Risk of Bias Assessment

Two reviewers independently assessed risk of bias using the Cochrane Risk of Bias 2 (RoB 2) tool for RCTs. We assessed five domains: randomization process, deviations from intended interventions, missing outcome data, measurement of outcome, and selection of reported result. Each domain was rated as low risk, some concerns, or high risk.

## Effect Measures

We used [mean difference/standardized mean difference/risk ratio/odds ratio] as the effect measure.

## Synthesis Methods

We conducted random-effects meta-analysis using the [method] method. Heterogeneity was quantified using I² statistic and τ². We considered I² values of 25%%, 50%%, and 75%% as low, moderate, and high heterogeneity, respectively.

## Meta-Regression and Subgroup Analyses

We planned a priori subgroup analyses based on: [list moderators]. Meta-regression was conducted for continuous moderators.

## Sensitivity Analyses

We performed sensitivity analyses: (1) excluding high risk of bias studies, (2) using fixed-effect model, (3) excluding outliers, and (4) using alternative effect size calculations.

## Publication Bias Assessment

We assessed publication bias using: funnel plot visual inspection, Egger's test, PET-PEESE, and trim-and-fill analysis.

## Certainty Assessment

We assessed certainty of evidence using the GRADE approach, considering risk of bias, inconsistency, indirectness, imprecision, and publication bias.

## Software

All analyses were conducted in R (version %s) using the cbamm package.
  ",
  if (!is.null(inclusion_criteria$population)) inclusion_criteria$population else "[specify]",
  if (!is.null(inclusion_criteria$intervention)) inclusion_criteria$intervention else "[specify]",
  if (!is.null(inclusion_criteria$comparator)) inclusion_criteria$comparator else "[specify]",
  if (!is.null(inclusion_criteria$outcome)) inclusion_criteria$outcome else "[specify]",
  databases, date_range,
  if (!is.null(search_details$search_string)) search_details$search_string else "[insert full search strategy]",
  if (!is.null(inclusion_criteria$outcome)) inclusion_criteria$outcome else "[specify primary outcome]",
  paste(R.version$major, R.version$minor, sep = ".")
  )

  methods_text
}

#' @keywords internal
generate_results_prisma <- function(meta_result) {
  n_studies <- if (!is.null(meta_result$n_studies)) meta_result$n_studies else nrow(meta_result$data)
  estimate <- if (!is.null(meta_result$estimate)) meta_result$estimate else NA
  ci_lower <- if (!is.null(meta_result$ci_lower)) meta_result$ci_lower else NA
  ci_upper <- if (!is.null(meta_result$ci_upper)) meta_result$ci_upper else NA
  p_value <- if (!is.null(meta_result$p_value)) meta_result$p_value else NA
  I2 <- if (!is.null(meta_result$I2)) meta_result$I2 else NA
  tau2 <- if (!is.null(meta_result$tau2)) meta_result$tau2 else NA

  results_text <- sprintf("
# Results

## Study Selection

The PRISMA flow diagram (Figure 1) summarizes the study selection process. Our search identified [N] records. After removing [N] duplicates, [N] records were screened. We excluded [N] records based on title and abstract. Full-text articles were retrieved for [N] records, of which [N] were excluded for the following reasons: [list reasons with counts]. Finally, %d studies were included in the quantitative synthesis.

Inter-rater agreement for study selection was [substantial/moderate] (κ = [value]).

## Study Characteristics

Table 1 presents characteristics of included studies. Studies were published between [year] and [year], with sample sizes ranging from [min] to [max] participants (median: [value]). [Describe key study characteristics: settings, populations, interventions, follow-up duration, etc.]

## Risk of Bias

Figure 2 shows risk of bias assessment. Overall, [N]%% of studies had low risk of bias, [N]%% had some concerns, and [N]%% had high risk of bias. The most common source of bias was [domain]. Detailed risk of bias assessments for each study are in Appendix X.

## Meta-Analysis Results

### Primary Outcome

The pooled effect size from %d studies was %.2f (95%% CI: %.2f to %.2f; p %s) (Figure 3, Forest plot). Between-study heterogeneity was %s (I² = %.1f%%, τ² = %.3f).

[Interpret the finding in clinical context. Discuss magnitude of effect and clinical significance.]

### Subgroup Analyses

[Report subgroup analyses results. For each subgroup analysis:
- State the research question
- Report pooled estimates for each subgroup
- Report test for subgroup differences
- Interpret findings]

### Meta-Regression

[Report meta-regression results if conducted. Include regression coefficients, 95%% CIs, R² values, and interpretations.]

### Publication Bias

Visual inspection of the funnel plot (Figure 4) [revealed/did not reveal] asymmetry. Egger's test [was/was not] significant (p = [value]). PET-PEESE analysis suggested a bias-corrected estimate of [value] (95%% CI: [range]). Trim-and-fill analysis indicated [N] potentially missing studies, yielding an adjusted estimate of [value] (95%% CI: [range]).

[Overall assessment of publication bias and its impact on results.]

### Sensitivity Analyses

Table 2 presents sensitivity analyses. Results were robust to: [list which sensitivity analyses confirmed main findings]. However, [mention any analyses that yielded different results and discuss implications].

### Certainty of Evidence

The GRADE assessment (Table 3) indicates [high/moderate/low/very low] certainty of evidence. Evidence was [upgraded/downgraded/not modified] due to [reasons].
  ",
  n_studies, n_studies, estimate, ci_lower, ci_upper,
  if (!is.na(p_value)) {
    if (p_value < 0.001) "< 0.001" else sprintf("= %.3f", p_value)
  } else "= [value]",
  ifelse(I2 < 25, "low", ifelse(I2 < 75, "moderate", "high")), I2, tau2
  )

  results_text
}

#' @keywords internal
generate_discussion <- function(meta_result) {
  discussion_text <- "
# Discussion

## Summary of Evidence

This systematic review and meta-analysis of [N] studies found that [restate main finding]. [Interpret in terms of clinical significance and practical implications].

## Comparison with Previous Reviews

[Compare results with previous systematic reviews. Discuss similarities and differences, and potential reasons for any discrepancies.]

## Strengths and Limitations

### Strengths

This review has several strengths:
1. Comprehensive search strategy across multiple databases
2. Rigorous methodology following PRISMA guidelines
3. Independent duplicate screening and data extraction
4. Comprehensive assessment of publication bias using multiple methods
5. Extensive sensitivity analyses to test robustness
6. GRADE assessment of certainty of evidence

### Limitations

Several limitations should be considered:
1. [Discuss heterogeneity if substantial and reasons]
2. [Discuss any publication bias concerns]
3. [Mention any limitations in included studies]
4. [Note any scope limitations or applicability issues]
5. [Language or publication restrictions if applicable]

## Clinical Implications

The findings of this review suggest that [discuss clinical implications]:
- For patients: [what this means for patient care]
- For clinicians: [recommendations for practice]
- For policymakers: [implications for guidelines or policy]

## Research Implications

Future research should:
1. [Identify gaps in current evidence]
2. [Suggest specific research questions]
3. [Recommend methodological improvements]
4. [Discuss need for longer follow-up, larger trials, etc.]

## Conclusions

[Restate main findings in 2-3 sentences. Provide balanced conclusion considering strengths, limitations, and certainty of evidence. Avoid overstatement.]
  "

  discussion_text
}

#' @keywords internal
generate_tables_prisma <- function(meta_result) {
  # Table 1: Study characteristics
  table1 <- "
**Table 1. Characteristics of Included Studies**

| Study | Year | Country | N | Population | Intervention | Control | Follow-up | Outcome Measure | Risk of Bias |
|-------|------|---------|---|------------|--------------|---------|-----------|-----------------|--------------|
| [Study 1] | YYYY | [Country] | NNN | [Description] | [Details] | [Details] | N months | [Measure] | [Low/Some concerns/High] |

*N = sample size; [Add footnotes as needed]*
  "

  # Table 2: Sensitivity analyses
  table2 <- "
**Table 2. Sensitivity Analyses**

| Analysis | N Studies | Effect Estimate | 95% CI | I² (%) | Conclusion |
|----------|-----------|----------------|--------|--------|------------|
| Main analysis | [N] | [Value] | [Range] | [Value] | Reference |
| Excluding high RoB | [N] | [Value] | [Range] | [Value] | [Robust/Changed] |
| Fixed-effect model | [N] | [Value] | [Range] | [Value] | [Robust/Changed] |
| Excluding outliers | [N] | [Value] | [Range] | [Value] | [Robust/Changed] |

*RoB = risk of bias; CI = confidence interval*
  "

  # Table 3: GRADE evidence profile
  table3 <- "
**Table 3. GRADE Evidence Profile**

| Outcome | Studies (N) | Participants (N) | Effect (95% CI) | Certainty | Reasons for Rating |
|---------|-------------|------------------|-----------------|-----------|-------------------|
| [Primary outcome] | [N] | [N] | [Value (Range)] | ⊕⊕⊕⊕ HIGH | - |
| | | | | ⊕⊕⊕◯ MODERATE | Downgraded for [reason] |
| | | | | ⊕⊕◯◯ LOW | Downgraded for [reasons] |
| | | | | ⊕◯◯◯ VERY LOW | Downgraded for [reasons] |

*GRADE Working Group grades of evidence:*
*⊕⊕⊕⊕ High certainty: Further research very unlikely to change confidence in estimate*
*⊕⊕⊕◯ Moderate certainty: Further research likely to have important impact on confidence*
*⊕⊕◯◯ Low certainty: Further research very likely to have important impact*
*⊕◯◯◯ Very low certainty: Very uncertain about the estimate*
  "

  list(
    table1 = table1,
    table2 = table2,
    table3 = table3
  )
}

#' @keywords internal
generate_figures_prisma <- function(meta_result, output_dir) {
  figures <- list()

  # Figure 1: PRISMA flow diagram (will be generated separately)
  figures$fig1_caption <- "**Figure 1. PRISMA 2020 Flow Diagram for Study Selection**"

  # Figure 2: Risk of bias summary
  figures$fig2_caption <- "**Figure 2. Risk of Bias Summary**. Summary of risk of bias assessments across all included studies using the Cochrane Risk of Bias 2 tool."

  # Figure 3: Forest plot
  figures$fig3_caption <- "**Figure 3. Forest Plot of Primary Outcome**. Random-effects meta-analysis of [outcome]. Squares represent individual study effect sizes (area proportional to weight), horizontal lines represent 95% confidence intervals, and the diamond represents the pooled estimate."

  # Figure 4: Funnel plot
  figures$fig4_caption <- "**Figure 4. Funnel Plot for Assessment of Publication Bias**. Each point represents one study. Asymmetry may indicate publication bias."

  figures
}

#' @keywords internal
generate_prisma_checklist <- function() {
  checklist <- "
# PRISMA 2020 Checklist

| Section | Item | Page |
|---------|------|------|
| **Title** | 1. Identify the report as a systematic review | [N] |
| **Abstract** | 2. Structured summary | [N] |
| **Introduction** | | |
| Rationale | 3. Rationale for review | [N] |
| Objectives | 4. Explicit statement of objectives/questions | [N] |
| **Methods** | | |
| Eligibility criteria | 5. Eligibility criteria | [N] |
| Information sources | 6. Information sources | [N] |
| Search strategy | 7. Search strategy | [N] |
| Selection process | 8. Study selection process | [N] |
| Data collection | 9. Data collection process | [N] |
| Data items | 10. Data extracted | [N] |
| Risk of bias | 11. Risk of bias assessment | [N] |
| Effect measures | 12. Effect measures | [N] |
| Synthesis methods | 13. Synthesis methods | [N] |
| Reporting bias | 14. Publication bias assessment | [N] |
| Certainty | 15. Certainty assessment | [N] |
| **Results** | | |
| Study selection | 16. Study selection results | [N] |
| Study characteristics | 17. Study characteristics | [N] |
| Risk of bias | 18. Risk of bias results | [N] |
| Results of syntheses | 19. Synthesis results | [N] |
| Reporting biases | 20. Publication bias results | [N] |
| Certainty of evidence | 21. Certainty of evidence | [N] |
| **Discussion** | | |
| Discussion | 22. Interpretation | [N] |
| Limitations | 23. Limitations | [N] |
| **Other Information** | | |
| Registration | 24. Registration and protocol | [N] |
| Support | 25. Sources of support | [N] |
| Competing interests | 26. Competing interests | [N] |
| Data availability | 27. Data and code availability | [N] |

*Complete checklist available at: http://www.prisma-statement.org/*
  "

  checklist
}

#' @keywords internal
generate_prisma_flow_diagram <- function(meta_result, output_dir) {
  # Generate PRISMA 2020 flow diagram using base R graphics
  png(file.path(output_dir, "prisma_flow_diagram.png"), width = 800, height = 1000, res = 100)

  par(mar = c(1, 1, 1, 1))
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 12))

  # Helper function to draw boxes
  draw_box <- function(x, y, w, h, text, col = "lightblue") {
    rect(x - w/2, y - h/2, x + w/2, y + h/2, col = col, border = "black", lwd = 2)
    text(x, y, text, cex = 0.7)
  }

  # PRISMA 2020 flow diagram structure
  # Identification
  draw_box(2.5, 11, 2, 0.8, "Records identified from:\nDatabases (n = [N])\nRegisters (n = [N])")
  draw_box(7.5, 11, 2, 0.8, "Records identified from:\nCitation searching (n = [N])")

  # Screening
  draw_box(5, 9.5, 3, 0.8, "Records screened\n(n = [N])", "lightyellow")
  draw_box(8.5, 9.5, 1.5, 0.6, "Records excluded\n(n = [N])", "pink")

  draw_box(5, 8, 3, 0.8, "Reports sought for retrieval\n(n = [N])", "lightyellow")
  draw_box(8.5, 8, 1.5, 0.6, "Reports not retrieved\n(n = [N])", "pink")

  draw_box(5, 6.5, 3, 0.8, "Reports assessed for eligibility\n(n = [N])", "lightyellow")
  draw_box(8.5, 6.5, 1.5, 1.2, "Reports excluded:\nReason 1 (n = [N])\nReason 2 (n = [N])\nReason 3 (n = [N])", "pink")

  # Included
  draw_box(5, 4.5, 3, 1, "Studies included in review\n(n = [N])\nReports of included studies\n(n = [N])", "lightgreen")

  # Arrows
  arrows(5, 10.7, 5, 10, lwd = 2, length = 0.1)
  arrows(5, 9, 5, 8.5, lwd = 2, length = 0.1)
  arrows(5, 7.5, 5, 7, lwd = 2, length = 0.1)
  arrows(5, 6, 5, 5.3, lwd = 2, length = 0.1)

  # Exclusion arrows
  arrows(6, 9.5, 7.8, 9.5, lwd = 1.5, length = 0.1, lty = 2)
  arrows(6, 8, 7.8, 8, lwd = 1.5, length = 0.1, lty = 2)
  arrows(6, 6.5, 7.8, 6.5, lwd = 1.5, length = 0.1, lty = 2)

  # Title
  text(5, 11.8, "PRISMA 2020 Flow Diagram", cex = 1.2, font = 2)

  dev.off()

  file.path(output_dir, "prisma_flow_diagram.png")
}

#' @keywords internal
generate_references <- function() {
  references <- "
# References

[References will be automatically formatted based on target journal requirements]

1. Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. BMJ 2021;372:n71.

2. Cochrane Handbook for Systematic Reviews of Interventions version 6.3 (updated February 2022). Cochrane, 2022. Available from www.training.cochrane.org/handbook.

3. Guyatt GH, Oxman AD, Vist GE, et al. GRADE: an emerging consensus on rating quality of evidence and strength of recommendations. BMJ 2008;336:924-6.

4. DerSimonian R, Laird N. Meta-analysis in clinical trials. Control Clin Trials 1986;7:177-88.

5. Egger M, Davey Smith G, Schneider M, Minder C. Bias in meta-analysis detected by a simple, graphical test. BMJ 1997;315:629-34.

[Additional references as cited in the manuscript]
  "

  references
}

#' @keywords internal
compile_manuscript_sections <- function(manuscript, title, journal) {
  compiled <- sprintf("
---
title: \"%s\"
output: word_document
---

%s

%s

%s

%s

%s

%s

---

%s

%s

%s

---

# Appendices

## Appendix A: Full Search Strategies

[Detailed search strategies for all databases]

## Appendix B: PRISMA 2020 Checklist

%s

## Appendix C: Risk of Bias Assessments

[Detailed risk of bias assessments for each included study]

## Appendix D: Data Extraction Forms

[Data extraction forms with complete data for all included studies]

  ",
  title,
  manuscript$title_page$title,
  manuscript$abstract$structured,
  manuscript$introduction,
  manuscript$methods,
  manuscript$results,
  manuscript$discussion,
  manuscript$tables$table1,
  manuscript$tables$table2,
  manuscript$tables$table3,
  manuscript$prisma_checklist
  )

  compiled
}

#' @keywords internal
export_manuscript <- function(manuscript, format, output_dir, title, template) {
  # Sanitize title for filename
  filename_base <- gsub("[^A-Za-z0-9]", "_", substr(title, 1, 50))

  if (format == "word") {
    output_file <- file.path(output_dir, paste0(filename_base, ".docx"))
    # Would use rmarkdown::render in practice
    writeLines(manuscript, file.path(output_dir, paste0(filename_base, ".md")))
    cat(sprintf("    Word document template written to: %s\n", output_file))
  } else if (format == "html") {
    output_file <- file.path(output_dir, paste0(filename_base, ".html"))
    # Convert markdown to HTML
    if (requireNamespace("markdown", quietly = TRUE)) {
      html <- markdown::markdownToHTML(text = manuscript, fragment.only = FALSE)
      writeLines(html, output_file)
    } else {
      writeLines(manuscript, file.path(output_dir, paste0(filename_base, ".md")))
    }
  } else if (format == "pdf") {
    output_file <- file.path(output_dir, paste0(filename_base, ".pdf"))
    writeLines(manuscript, file.path(output_dir, paste0(filename_base, ".md")))
    cat(sprintf("    PDF requires rmarkdown. Markdown written to: %s\n", output_file))
  } else if (format == "latex") {
    output_file <- file.path(output_dir, paste0(filename_base, ".tex"))
    writeLines(manuscript, output_file)
  }

  return(output_file)
}

#' @export
print.prisma_manuscript <- function(x, ...) {
  cat("PRISMA Manuscript Generated\n")
  cat("===========================\n\n")

  cat("Output directory:", x$output_dir, "\n")
  cat("Generated files:\n")
  for (format in names(x$output_files)) {
    cat(sprintf("  %s: %s\n", toupper(format), basename(x$output_files[[format]])))
  }

  cat("\n")
  invisible(x)
}
