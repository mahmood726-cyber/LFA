# cbamm 0.1.0

## Initial Release

This is the first release of the `cbamm` package (Collaborative Bayesian Adaptive Meta-Analysis Methods).

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

### Visualization Functions

* `forest_plot_enhanced()`: Enhanced forest plots with study weights
* `create_cumulative_dashboard()`: Multi-panel cumulative analysis visualization
* `funnel_plot()`: Publication bias assessment
* All plots use base R graphics (no dependencies on ggplot2)

### Data Validation and Utilities

* `validate_cbamm_data()`: Comprehensive data validation with error/warning reporting
* `standardize_cbamm_data()`: Standardize data format for analysis
* `detect_outliers_iqr()`: Outlier detection using IQR method
* `prediction_interval()`: Prediction intervals for future studies
* Utility functions: `calculate_i2()`, `calculate_tau2()`, `format_ci()`, `format_p()`
* Conversion functions: `se_to_var()`, `var_to_se()`, `safe_divide()`

### S3 Methods

* `print()` methods for all result objects (cbamm, cbamm_cumulative, cbamm_metareg, cbamm_subgroup)
* `summary()` method for detailed cbamm results with prediction intervals and study weights

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
