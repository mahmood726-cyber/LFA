# LFA/cbamm - Quick Reference Guide

## Quick Start Examples

### 1. Basic Meta-Analysis
```r
library(cbamm)

# Create meta-analysis data
data <- data.frame(
  study = c("Study1", "Study2", "Study3"),
  effect = c(0.5, 0.6, 0.4),
  se = c(0.1, 0.12, 0.15)
)

# Simple meta-analysis
result <- cbamm_fast(data)
print(result)
```

### 2. Intelligent Meta-Analysis with AI + Rules
```r
# With AI (requires local Ollama)
llm <- init_llm()
result <- intelligent_cbamm(
  data,
  enable_ai = TRUE,
  enable_rules = TRUE,
  llm_config = llm,
  verbose = TRUE
)
```

### 3. Autopilot Mode (Fully Automated)
```r
# One command does EVERYTHING
result <- autopilot_meta_analysis(
  data = my_data,
  research_question = "Does intervention X improve outcome Y?",
  auto_visualize = TRUE
)
# AI selects methods, runs analysis, generates report
```

### 4. Rules Engine Validation
```r
# Initialize rules engine
rules <- init_rules_engine()
print(rules$n_rules)  # Shows 500+

# Apply rules to data
triggered <- apply_rules(rules, data)
print(triggered)  # Summary of triggered rules
```

### 5. Scenario Matching
```r
# Initialize scenario database
scenario_db <- init_scenario_database()
print(scenario_statistics(scenario_db))

# Find similar scenarios
similar <- find_scenarios(
  scenario_db,
  type = "clinical_trial",
  k_min = 5,
  k_max = 20
)
```

### 6. Publication Bias Assessment
```r
# Traditional methods
egger_result <- egger_test(data)
pet_peese_result <- pet_peese(data)
trim_fill_result <- trim_fill(data)

# Intelligent version with AI + rules
bias_result <- intelligent_publication_bias(
  data,
  enable_ai = TRUE,
  enable_rules = TRUE
)
```

### 7. Sensitivity Analysis
```r
# Intelligent sensitivity with stability assessment
sensitivity <- intelligent_sensitivity(
  data,
  result,
  enable_ai = TRUE,
  enable_rules = TRUE
)
print(sensitivity$stability_assessment)
```

### 8. Network Meta-Analysis
```r
# Create network data (3+ treatments)
nma_data <- data.frame(
  study = c(1, 1, 2, 2),
  treatment = c("A", "B", "A", "C"),
  effect = c(0, 0.5, 0, 0.8),
  se = c(0.1, 0.15, 0.1, 0.2)
)

# Run intelligent NMA
nma_result <- intelligent_nma(nma_data, enable_ai = TRUE)
```

### 9. Bayesian Meta-Analysis
```r
# Bayesian with MCMC
bayes_result <- bayesian_ma(
  data,
  n_iter = 10000,
  n_burn = 2000,
  prior_mean = 0,
  prior_sd = 10
)
print(bayes_result)
plot(bayes_result)
```

### 10. Modern ggplot2 Visualizations
```r
library(ggplot2)

# Publication-quality forest plot
p <- gg_forest_plot(
  data,
  result = result,
  show_weights = TRUE,
  color_by = "year",
  title = "Meta-Analysis Forest Plot"
)
print(p)
ggsave("forest_plot.pdf", p, width = 10, height = 8)

# Modern funnel plot
funnel <- gg_funnel_plot(data)
print(funnel)

# Cumulative evidence
cumulative <- gg_cumulative_plot(cum_result)
print(cumulative)
```

### 11. Meta-Regression
```r
# Simple meta-regression
reg_result <- meta_regression(data, ~ year)
print(reg_result)

# Multiple predictors
reg_multi <- meta_regression(data, ~ year + quality + sample_size)
print(reg_multi)
```

### 12. Effect Size Conversions
```r
# Cohen's d to Hedges' g
g <- d_to_g(d = 0.5, n1 = 30, n2 = 30)

# Calculate from raw data
d <- cohens_d_from_means(m1 = 10, m2 = 8, sd1 = 2, sd2 = 2, n1 = 30, n2 = 30)

# Odds ratio from 2x2 table
or <- or_from_2x2(a = 50, b = 25, c = 30, d = 45)

# Fisher's Z for correlations
z <- fisher_z(r = 0.5)
r_back <- inv_fisher_z(z)
```

### 13. IPD Meta-Analysis
```r
# One-stage IPD analysis
ipd_1stage <- ipd_onestage(ipd_data, method = "glm")
print(ipd_1stage)

# Two-stage IPD analysis
ipd_2stage <- ipd_twostage(ipd_data)
print(ipd_2stage)

# Test interactions
interaction <- ipd_interaction_analysis(ipd_data, "treatment")
```

### 14. Living Systematic Review
```r
# Initialize living review
living_ma <- init_living_ma(
  data,
  method = "REML",
  monitoring = TRUE
)

# Update with new studies
updated <- update_living_ma(living_ma, new_studies)

# Check stopping rules
stop_rules <- check_stopping_rules(updated)
print(stop_rules$verdict)
```

### 15. Reporting
```r
# Generate comprehensive report
generate_report(
  result,
  file = "meta_analysis_report.txt",
  include_studies = TRUE
)

# Create summary table
summary_table(result, format = "markdown")

# Export study data
export_study_data(result, file = "study_data.csv")

# PRISMA checklist
prisma_checklist(interactive = TRUE)
```

---

## Key Function Quick Lookup

### Data Preparation
- `validate_cbamm_data()` - Validate data structure
- `standardize_cbamm_data()` - Standardize data format
- `detect_outliers_iqr()` - Find outliers

### Core Meta-Analysis
- `cbamm_fast()` - Fast random-effects MA
- `cumulative_meta_analysis()` - Cumulative with stability detection
- `meta_regression()` - Study-level covariates
- `subgroup_analysis()` - Subgroup comparisons

### AI Integration
- `init_llm()` - Initialize Llama 3 connection
- `ai_method_selection()` - AI recommends methods
- `ai_interpret_results()` - AI generates interpretation
- `ai_quality_assessment()` - AI evaluates quality
- `ai_literature_screening()` - AI screens studies
- `ai_research_gaps()` - AI identifies gaps

### Rules Engine
- `init_rules_engine()` - Initialize 500+ rules
- `apply_rules()` - Apply rules to data

### Scenarios
- `init_scenario_database()` - Initialize 10K+ scenarios
- `find_scenarios()` - Find matching scenarios
- `scenario_statistics()` - Get database statistics

### Intelligent Wrappers
- `intelligent_cbamm()` - Meta-analysis with AI + rules
- `intelligent_publication_bias()` - Publication bias with AI
- `intelligent_sensitivity()` - Sensitivity with AI
- `intelligent_nma()` - Network MA with AI
- `intelligent_quality_assessment()` - Quality with AI
- `autopilot_meta_analysis()` - FULLY AUTOMATED

### Publication Bias
- `egger_test()` - Egger's test
- `pet_peese()` - PET-PEESE method
- `trim_fill()` - Trim-and-fill
- `assess_publication_bias()` - Multiple methods

### Sensitivity Analysis
- `leave_one_out()` - LOO analysis
- `influence_diagnostics()` - Cook's D, DFBETAS
- `bootstrap_ma()` - Bootstrap confidence intervals
- `cumulative_influence()` - Cumulative influence

### Advanced Methods
- `bayesian_ma()` - Bayesian MCMC
- `network_meta_analysis()` - Network MA (3+ treatments)
- `ipd_onestage()` - One-stage IPD analysis
- `ipd_twostage()` - Two-stage IPD analysis
- `dta_bivariate()` - Diagnostic test accuracy
- `dose_response_ma()` - Dose-response analysis
- `multivariate_ma()` - Correlated outcomes
- `rf_meta_analysis()` - Random forest heterogeneity
- `nn_meta_analysis()` - Neural network prediction
- `prediction_interval()` - Future study effect

### Visualization - ggplot2
- `gg_forest_plot()` - Publication-quality forest plot
- `gg_funnel_plot()` - Modern funnel plot
- `gg_labbe_plot()` - Diagnostic sensitivity/specificity
- `gg_cumulative_plot()` - Evidence accumulation
- `gg_gosh_plot()` - Heterogeneity patterns
- `gg_metareg_plot()` - Regression visualization

### Visualization - Traditional
- `forest_plot_enhanced()` - Enhanced forest plot
- `funnel_plot()` - Funnel plot
- `baujat_plot()` - Heterogeneity source plot
- `radial_plot()` - Galbraith plot
- `contour_funnel_plot()` - Contour funnel
- `labbe_plot()` - Diagnostic test plot

### Reporting
- `generate_report()` - Text report
- `summary_table()` - Summary tables
- `export_study_data()` - CSV/Excel export
- `prisma_checklist()` - PRISMA compliance
- `power_analysis_ma()` - Power analysis

### Effect Size Conversions
- `d_to_g()`, `g_to_d()` - Cohen's d ↔ Hedges' g
- `d_to_r()`, `r_to_d()` - d ↔ correlation
- `d_to_or()`, `or_to_d()` - d ↔ log OR
- `or_to_rr()`, `rr_to_or()` - OR ↔ RR
- `cohens_d_from_means()` - d from raw data
- `or_from_2x2()` - OR from 2x2 table
- `fisher_z()`, `inv_fisher_z()` - Fisher's Z

---

## Data Format

### Minimal Required Format
```r
data <- data.frame(
  study = c("Study1", "Study2", "Study3"),
  effect = c(0.5, 0.6, 0.4),
  se = c(0.1, 0.12, 0.15)
)
```

### Recommended Full Format
```r
data <- data.frame(
  study = c("Study1", "Study2", "Study3"),
  year = c(2020, 2021, 2022),
  effect = c(0.5, 0.6, 0.4),
  se = c(0.1, 0.12, 0.15),
  n = c(100, 150, 120),
  quality = c(7, 8, 6),
  country = c("USA", "UK", "Canada")
)
```

---

## Output Objects

### Meta-Analysis Result (`cbamm` object)
```
$estimate      # Pooled effect size
$ci_lower      # Lower 95% CI bound
$ci_upper      # Upper 95% CI bound
$se            # Standard error
$p_value       # P-value
$I2            # Heterogeneity I²
$tau2          # Between-study variance
$Q             # Heterogeneity Q statistic
$k             # Number of studies
$weights       # Study weights
$method        # Method used (DL, REML, etc)
$data          # Original data
```

### Rules Result (`rules_result` object)
```
$triggered[]   # List of triggered rules
$severity      # critical, high, medium, low, info
$message       # User-friendly guidance
$action        # ERROR, WARN, RECOMMEND, INTERPRET
Total count and severity breakdown printed
```

### Intelligent Result (`intelligent_cbamm` object)
```
# All cbamm components plus:
$rules_assessment        # Rules output
$ai_interpretation       # AI text interpretation
$research_gaps           # AI-identified gaps
$intelligent_analysis    # Metadata (AI enabled, rules triggered, etc)
```

---

## Rule Severity Levels

| Severity | Action | Description |
|----------|--------|-------------|
| critical | ERROR | Analysis stops, fix required |
| high | WARN | Serious issue, analysis proceeds with caution |
| medium | RECOMMEND | Issue identified, follow-up recommended |
| low | RECOMMEND | Minor issue, note for completeness |
| info | INFORM | Informational only, no action needed |

---

## Performance Tips

1. **Large Datasets**: Use `parallel_bootstrap_ma()` for parallel processing
2. **Caching**: Use `cached_meta_analysis()` to cache intermediate results
3. **Batch Processing**: Use `batch_meta_analysis()` for multiple analyses
4. **Vectorization**: Use `vectorized_heterogeneity()` for fast I² calculation

---

## System Requirements

**Required Packages** (automatically installed):
- stats, graphics, utils, methods, parallel, digest

**Optional Packages** (for full features):
- metafor (≥3.0.0) - interoperability
- ggplot2 (≥3.3.0) - modern visualizations
- dplyr (≥1.0.0) - data manipulation
- shiny (≥1.7.0) - interactive dashboard
- httr, jsonlite - for API calls

**For AI Features**:
- Ollama with Llama 3 model (https://ollama.ai)
- Run: `ollama pull llama3`
- Default endpoint: `http://localhost:11434/api/generate`

---

## See Also

- **Full documentation**: `CODEBASE_STRUCTURE.md`
- **Package documentation**: `?function_name`
- **Vignettes**: `browseVignettes("cbamm")`
- **GitHub**: https://github.com/mahmood726-cyber/LFA
- **Issues**: https://github.com/mahmood726-cyber/LFA/issues

