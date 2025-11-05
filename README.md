# cbamm: Collaborative Bayesian Adaptive Meta-Analysis Methods

<!-- badges: start -->
[![R-CMD-check](https://github.com/mahmood726-cyber/LFA/workflows/R-CMD-check/badge.svg)](https://github.com/mahmood726-cyber/LFA/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/cbamm)](https://CRAN.R-project.org/package=cbamm)
[![License: GPL (>= 3)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%203%29-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

`cbamm` provides the **most comprehensive** meta-analysis toolkit in R, implementing the latest methodological advances from statistical journals (2024-2025) plus advanced methods rarely found elsewhere:

### Core Features
- **Fast computation**: Efficient random-effects meta-analysis for large datasets
- **Cumulative analysis**: Evidence accumulation tracking with stability assessment
- **Generalizability**: Transport weights for external validity
- **Publication bias**: Egger test, PET-PEESE, Trim & Fill (2024 methods)
- **Sensitivity analysis**: Leave-one-out, influence diagnostics, Cook's distance
- **Advanced visualization**: Baujat plots, radial plots, contour-enhanced funnel plots
- **Effect size conversions**: Cohen's d, Hedges' g, OR, RR, correlations (17 functions!)
- **Diagnostic test accuracy**: Bivariate models, SROC curves
- **Meta-regression**: Study-level covariates, subgroup analysis
- **Reporting**: PRISMA checklists, automated reports, power analysis

### Advanced Features (Unique!)
- **🔗 Network meta-analysis**: Multiple treatment comparisons
- **🎲 Bayesian methods**: MCMC sampling, Bayes factors, posterior predictive checks
- **👥 IPD meta-analysis**: One-stage and two-stage approaches
- **📊 Dose-response**: Linear, quadratic, restricted cubic splines
- **🎨 Interactive dashboard**: Shiny app for point-and-click analysis

**Statistics**: 21 R files | 6,408 lines of code | 120+ functions | Zero dependencies

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("mahmood726-cyber/LFA")
```

Once on CRAN, you can install the released version:

```r
install.packages("cbamm")
```

## Quick Start

### Basic Meta-Analysis

```r
library(cbamm)

# Load example data
data(example_meta)
head(example_meta)

# Perform random-effects meta-analysis
result <- cbamm_fast(example_meta)
print(result)
summary(result)

# Create forest plot
forest_plot_enhanced(example_meta, result,
                     title = "Meta-Analysis of Treatment Effect")
```

### Cumulative Meta-Analysis

Track how the evidence evolves as studies are added over time:

```r
# Perform cumulative analysis ordered by publication year
cum_result <- cumulative_meta_analysis(
  example_meta,
  order_by = "year",
  assess_stability = TRUE
)

print(cum_result)

# Create comprehensive dashboard
create_cumulative_dashboard(cum_result,
                            main = "Evidence Accumulation Over Time")
```

### Transport Weights for Generalizability

Generalize meta-analytic findings to a specific target population:

```r
# Load example data with population characteristics
data(example_transport)

# Define target population
target_population <- list(
  age_mean = 60,
  female_pct = 0.52,
  bmi_mean = 28,
  diabetes_pct = 0.20
)

# Compute transport weights
weights <- compute_transport_weights(
  example_transport,
  target_population,
  method = "simple"
)

# View weight diagnostics
print(weights$diagnostics)
cat("Effective N:", round(weights$effective_n, 1), "\n")

# Perform weighted meta-analysis
analysis_weights <- compute_analysis_weights(
  example_transport,
  weights$normalized_weights
)
```

### Meta-Regression

Examine how study-level covariates affect effect sizes:

```r
# Meta-regression with publication year
result <- meta_regression(example_meta, ~ year)
print(result)

# Meta-regression with multiple predictors
result_multi <- meta_regression(example_meta, ~ year + quality + sample_size)
print(result_multi)
```

### Subgroup Analysis

Compare effect sizes across different subgroups:

```r
# Analyze by geographic region
subgroup_result <- subgroup_analysis(
  example_meta,
  subgroup = "region"
)
print(subgroup_result)
```

### Diagnostic Test Accuracy Meta-Analysis

Analyze diagnostic test performance across studies:

```r
# Load DTA example data
data(example_dta)

# Prepare data for meta-analysis of log diagnostic odds ratios
dta_ma <- data.frame(
  study = example_dta$study,
  effect = example_dta$log_dor,
  se = example_dta$se_log_dor
)

# Perform meta-analysis
result <- cbamm_fast(dta_ma)
print(result)

# Back-transform to odds ratio scale
exp(result$estimate)
exp(c(result$ci_lower, result$ci_upper))
```

## Advanced Features (New in 2024!)

### Publication Bias Detection & Correction

Based on latest 2024-2025 methodological research:

```r
# Comprehensive publication bias assessment
bias_results <- assess_publication_bias(example_meta, methods = "all")
print(bias_results)

# Egger's regression test
egger <- egger_test(example_meta)
print(egger)

# PET-PEESE correction (recommended method per 2024 research)
pet_peese_result <- pet_peese(example_meta, method = "conditional")
print(pet_peese_result)

# Trim and fill
tf_result <- trim_fill(example_meta)
print(tf_result)
```

### Sensitivity & Influence Analysis

```r
# Leave-one-out analysis
loo_result <- leave_one_out(example_meta, sort_by = "influence")
print(loo_result)
plot_leave_one_out(loo_result)

# Comprehensive influence diagnostics
influence_result <- influence_diagnostics(example_meta)
print(influence_result)
plot_influence_diagnostics(influence_result)

# Bootstrap confidence intervals
boot_result <- bootstrap_ma(example_meta, n_bootstrap = 1000, seed = 123)
hist(boot_result$bootstrap_estimates, main = "Bootstrap Distribution")
```

### Advanced Visualization

```r
# Baujat plot (identify heterogeneity sources)
baujat_plot(example_meta, label_points = TRUE)

# Contour-enhanced funnel plot
contour_funnel_plot(example_meta)

# Radial (Galbraith) plot
radial_plot(example_meta)
```

### Effect Size Conversions

```r
# Cohen's d to Hedges' g
g <- d_to_g(d = 0.5, n1 = 30, n2 = 30)

# Calculate Cohen's d from means and SDs
d_result <- cohens_d_from_means(m1 = 10, m2 = 8, sd1 = 2, sd2 = 2,
                                 n1 = 30, n2 = 30)

# Odds ratio from 2x2 table
or_result <- or_from_2x2(a = 50, b = 25, c = 30, d = 45)
print(or_result)

# Risk ratio from 2x2 table
rr_result <- rr_from_2x2(a = 50, b = 25, c = 30, d = 45)

# Fisher's Z transformation for correlations
z <- fisher_z(r = 0.5)
r_back <- inv_fisher_z(z)
```

### Advanced DTA: Bivariate Meta-Analysis

```r
# Bivariate analysis of sensitivity and specificity
dta_result <- dta_bivariate(example_dta, method = "reitsma")
print(dta_result)

# Summary ROC plot
sroc_plot(dta_result, show_ci = TRUE)

# Coupled forest plots
dta_forest_plot(example_dta, dta_result)

# Calculate likelihood ratios
lr <- likelihood_ratios(sensitivity = 0.90, specificity = 0.85)
print(lr)
```

### Reporting & Export

```r
# Generate comprehensive report
generate_report(result, file = "meta_analysis_report.txt",
                include_studies = TRUE)

# Export results as table
summary_table(result, format = "markdown")
summary_table(result, format = "latex")

# Export study-level data
export_study_data(result, file = "study_data.csv")

# PRISMA checklist
prisma_checklist(interactive = TRUE)

# Power analysis
power <- power_analysis_ma(k = 10, effect_size = 0.5,
                           heterogeneity = 0.25, avg_n = 50)
print(power)
```

## Key Features

### Fast Random-Effects Meta-Analysis

The `cbamm_fast()` function provides efficient computation using the DerSimonian-Laird or REML methods:

- Handles large datasets efficiently
- Computes heterogeneity statistics (I², τ², Q)
- Provides prediction intervals
- Supports multiple estimation methods

### Cumulative Meta-Analysis with Stability Assessment

The `cumulative_meta_analysis()` function:

- Shows how estimates evolve as evidence accumulates
- Automatically detects when results become stable
- Helps identify when additional studies are unlikely to change conclusions
- Visualizes trends in effect size, heterogeneity, and precision

### Transport Weights

The `compute_transport_weights()` function addresses external validity:

- Computes weights to transport findings to target populations
- Accounts for differences in baseline characteristics
- Provides effective sample size after weighting
- Supports simple distance-based or propensity-based methods

### Comprehensive Visualization

- **Forest plots**: Enhanced forest plots with study weights and customizable options
- **Cumulative dashboards**: Multi-panel visualization of evidence accumulation
- **Funnel plots**: Assess publication bias visually

## Data Validation and Quality Checks

```r
# Validate data before analysis
validation <- validate_cbamm_data(your_data)

if (!validation$valid) {
  cat("Errors found:\n")
  print(validation$errors)
  cat("\nWarnings:\n")
  print(validation$warnings)
}

# Standardize data format
standardized_data <- standardize_cbamm_data(
  your_data,
  effect_col = "estimate",
  se_col = "stderr",
  study_col = "studyname"
)

# Detect outliers
outliers <- detect_outliers_iqr(your_data, multiplier = 1.5)
your_data[outliers, ]
```

## Advanced Methods

### Robust Variance Estimation

```r
# Account for clustering in robust SE estimation
result <- robust_rma(data, cluster = "trial_id")
```

### Prediction Intervals

```r
# Get prediction interval for true effect in a new study
pred_int <- prediction_interval(
  estimate = result$estimate,
  se = result$se,
  tau2 = result$tau2,
  k = result$k
)
```

## Package Testing

Run built-in tests to verify package functionality:

```r
test_cbamm()
```

## Getting Help

- **Documentation**: Use `?function_name` for detailed help on any function
- **Issues**: Report bugs or request features at [GitHub Issues](https://github.com/mahmood726-cyber/LFA/issues)
- **Vignettes**: See `browseVignettes("cbamm")` for detailed tutorials (coming soon)

## Citation

If you use `cbamm` in your research, please cite:

```
Ahmad M (2024). cbamm: Collaborative Bayesian Adaptive Meta-Analysis Methods.
R package version 0.1.0.
```

## Related Packages

- **metafor**: Comprehensive meta-analysis package with extensive features
- **meta**: Another popular meta-analysis package
- **MetaStan**: Bayesian meta-analysis using Stan
- **MetaDTA**: Interactive tool for diagnostic test accuracy meta-analyses

## License

GPL (>= 3)

## Author

Mahmood Ahmad (mahmood.ahmad2@nhs.net)

---

**Note**: This package is under active development. Features and interfaces may change in future versions.
