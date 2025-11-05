# cbamm 0.1.0

## Initial Release - State-of-the-Art Meta-Analysis

This is the first release of the `cbamm` package, implementing **cutting-edge meta-analysis methods** based on the latest research from statistical journals (2024-2025).

📊 **Statistics**: 16 R source files, 4,459 lines of code, 90+ exported functions

### Core Features

* **Fast Random-Effects Meta-Analysis**
  - `cbamm_fast()`: Efficient meta-analysis using DL or REML methods
  - Support for large datasets with optimized computation
  - Automatic computation of heterogeneity statistics (I², τ², Q)

* **Cumulative Meta-Analysis**
  - `cumulative_meta_analysis()`: Track evidence accumulation over time
  - Automatic stability detection to identify when estimates stabilize
  - Customizable ordering by year, precision, or other variables
  - Comprehensive visualization via `create_cumulative_dashboard()`

* **Transport Weights for Generalizability**
  - `compute_transport_weights()`: Address external validity concerns
  - Weight studies based on similarity to target population
  - Support for simple distance-based and propensity-based methods
  - Effective sample size calculation after weighting
  - `compute_analysis_weights()`: Combine sampling variance and transport weights

* **Meta-Regression and Subgroup Analysis**
  - `meta_regression()`: Examine effects of study-level covariates
  - `subgroup_analysis()`: Compare effects across subgroups
  - `robust_rma()`: Robust variance estimation with clustering
  - Between-group heterogeneity testing

* **Diagnostic Test Accuracy Support**
  - Example DTA dataset with 2×2 table data
  - Support for sensitivity, specificity, and DOR meta-analyses
  - Specialized data handling for diagnostic studies

### Publication Bias Methods (2024-2025 Research)

**Based on latest methodological studies:**

* **`egger_test()`**: Egger's regression test for funnel plot asymmetry
* **`pet_peese()`**: PET-PEESE correction (recommended by 2024 comparative studies)
  - Conditional approach (PET when p > 0.10, PEESE when p < 0.10)
  - More robust than trim-and-fill in presence of heterogeneity
* **`trim_fill()`**: Trim and fill method for estimating missing studies
* **`assess_publication_bias()`**: Comprehensive assessment running all methods

**References**: Recent research shows PET-PEESE and Copas methods outperform traditional approaches

### Sensitivity & Influence Analysis

* **`leave_one_out()`**: Leave-one-out sensitivity analysis
  - Identifies studies with high influence on pooled estimate
  - Sortable by estimate, I², or influence
* **`influence_diagnostics()`**: Comprehensive influence diagnostics
  - Cook's distance for meta-analysis
  - DFBETAS (change in estimate)
  - Standardized residuals
  - Hat values (leverage)
* **`baujat_plot_data()`**: Baujat plot data (heterogeneity contribution vs influence)
* **`bootstrap_ma()`**: Bootstrap confidence intervals with resampling
* **`cumulative_influence()`**: Combined cumulative and influence analysis

**References**: Based on Viechtbauer & Cheung (2010) and Baujat et al. (2002)

### Advanced Visualization

**Basic Plots:**
* `forest_plot_enhanced()`: Enhanced forest plots with study weights
* `create_cumulative_dashboard()`: Multi-panel cumulative analysis visualization
* `funnel_plot()`: Publication bias assessment

**Advanced Plots (New!):**
* **`baujat_plot()`**: Identify sources of heterogeneity and influential studies
* **`contour_funnel_plot()`**: Contour-enhanced funnel plot showing significance regions
* **`radial_plot()`**: Radial (Galbraith) plot for heterogeneity assessment
* **`plot_leave_one_out()`**: Forest plot showing leave-one-out results
* **`plot_influence_diagnostics()`**: Multi-panel influence diagnostic plots
* **`labbe_plot()`**: L'Abbé plot for binary outcomes

All plots use base R graphics (no ggplot2 dependency)

**References**: Peters et al. (2008) for contour funnel plots, Galbraith (1988) for radial plots

### Effect Size Conversions (17 functions!)

**Comprehensive effect size conversion toolkit:**

* **Cohen's d / Hedges' g**: `d_to_g()`, `g_to_d()`, `cohens_d_from_means()`
* **Correlations**: `d_to_r()`, `r_to_d()`, `fisher_z()`, `inv_fisher_z()`, `se_fisher_z()`
* **Odds Ratios**: `or_to_log_or()`, `log_or_to_or()`, `or_to_rr()`, `rr_to_or()`, `or_from_2x2()`
* **Risk Ratios**: `rr_from_2x2()`, `rd_from_2x2()`
* **Cross-metric**: `d_to_or()`, `or_to_d()`
* **Utility**: `convert_effects()` for batch conversions

**References**: Borenstein et al. (2009), Chinn (2000), Zhang & Yu (1998)

### Advanced Diagnostic Test Accuracy

**Bivariate meta-analysis (state-of-the-art DTA methods):**

* **`dta_bivariate()`**: Bivariate random-effects model for sensitivity/specificity
  - Accounts for negative correlation between sens/spec
  - Simplified Reitsma model implementation
  - Summary operating points with confidence regions
* **`sroc_plot()`**: Summary receiver operating characteristic curves
* **`dta_forest_plot()`**: Coupled forest plots for sensitivity and specificity
* **`likelihood_ratios()`**: Calculate positive and negative likelihood ratios
* **`diagnostic_or()`**: Diagnostic odds ratio calculations

**References**: Reitsma et al. (2005), Rutter & Gatsonis (2001)

### Reporting & Export Utilities

**Professional reporting tools:**

* **`generate_report()`**: Comprehensive text reports with all statistics
* **`summary_table()`**: Export results as data.frame, markdown, or LaTeX
* **`export_study_data()`**: Export study-level data with weights to CSV
* **`prisma_checklist()`**: Interactive PRISMA reporting checklist
* **`power_analysis_ma()`**: Power calculations for meta-analysis planning

### Data Validation and Utilities

* `validate_cbamm_data()`: Comprehensive data validation with error/warning reporting
* `standardize_cbamm_data()`: Standardize data format for analysis
* `detect_outliers_iqr()`: Outlier detection using IQR method
* `prediction_interval()`: Prediction intervals for future studies
* Utility functions: `calculate_i2()`, `calculate_tau2()`, `format_ci()`, `format_p()`
* Conversion functions: `se_to_var()`, `var_to_se()`, `safe_divide()`

### S3 Methods (13 print methods!)

**Comprehensive print methods for all result classes:**

* `print.cbamm()`, `summary.cbamm()`: Core meta-analysis results
* `print.cbamm_cumulative()`: Cumulative analysis with stability info
* `print.cbamm_metareg()`, `print.cbamm_subgroup()`: Regression and subgroup results
* `print.egger_test()`, `print.pet_peese()`, `print.trim_fill()`: Publication bias results
* `print.bias_assessment()`: Comprehensive bias assessment
* `print.loo_analysis()`: Leave-one-out results with influential studies
* `print.influence_diagnostics()`: Influence diagnostics with thresholds
* `print.bootstrap_ma()`: Bootstrap results with CIs
* `print.dta_bivariate()`: DTA bivariate results with likelihood ratios

### Example Datasets

* `example_meta`: Standard meta-analysis dataset with 10 studies
* `example_transport`: Dataset with population characteristics for transport weights
* `example_dta`: Diagnostic test accuracy dataset with 2×2 table data

### Testing

* Comprehensive test suite using testthat (>= 3.0.0)
* Tests for all major functions
* Input validation tests
* Edge case handling tests

### Documentation

* Complete function documentation with examples
* Comprehensive README with quick start guide
* Example workflows for common meta-analysis tasks

### Package Infrastructure

* Minimal dependencies (only base R stats, graphics, utils, methods)
* Optional enhancements with metafor and ggplot2
* GPL (>= 3) license
* CRAN-ready package structure
* R version requirement: >= 3.5.0

## Known Limitations

* Some advanced features require R >= 4.0.0 for optimal performance
* Bayesian methods are planned for future releases
* Network meta-analysis support is planned for future releases

## Future Plans

For version 0.2.0, we plan to add:

* Bayesian meta-analysis methods
* Enhanced diagnostic test accuracy methods (bivariate models)
* Network meta-analysis capabilities
* Additional visualization options with ggplot2
* Vignettes with detailed tutorials
* Performance optimizations for very large meta-analyses (k > 1000)
* Publication bias tests (Egger's test, trim-and-fill)
* Sensitivity analysis tools

---

## Acknowledgments

We thank the R meta-analysis community, particularly the developers of `metafor`, `meta`, and `MetaDTA` packages, which provided inspiration for this work.

---

*For bug reports and feature requests, please visit: https://github.com/mahmood726-cyber/LFA/issues*
