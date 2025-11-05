# LFA Codebase Documentation Index

## Overview

This is a comprehensive exploration of the **cbamm** (Collaborative Bayesian Adaptive Meta-Analysis Methods) package - an AI-powered meta-analysis toolkit with 500+ expert rules and 10,000+ validated scenarios.

**Package Version**: 2.0.0 (AI-Powered)
**Repository**: https://github.com/mahmood726-cyber/LFA
**Exploration Date**: 2025-11-05

---

## Three-Part Documentation System

### 1. QUICK_REFERENCE.md (11 KB | 436 lines)
**Best For**: Getting started quickly, practical usage examples

Contents:
- 15 working code examples
- Quick function lookup table (100+ functions)
- Data format specifications
- Output object structures
- System requirements and installation
- Performance tips and tricks

**Start Here If**: You want to use the package immediately and see code examples

---

### 2. CODEBASE_STRUCTURE.md (23 KB | 794 lines)
**Best For**: Deep technical understanding, architecture analysis

Contents:
- All 500+ rules in 10 categories with detailed specifications
- 15+ meta-analysis methods with implementation details
- 7 AI functions with complete specifications
- 20+ visualization types across 4 categories
- Complete workflow architecture
- File-by-file breakdown with line counts
- Integration patterns and performance features

**Start Here If**: You're developing or extending the package, need architecture details

---

### 3. README_EXPLORATION.md (11 KB | 333 lines)
**Best For**: Strategic overview, competitive analysis, implementation decisions

Contents:
- Executive summary of findings
- Architecture strengths and design patterns
- Unique competitive advantages
- Code quality metrics and statistics
- Usage patterns for different user types
- Recommended next steps for development
- Statistical methods coverage by year and domain

**Start Here If**: You're evaluating the package or planning improvements

---

## Quick Navigation by Use Case

### I want to USE the package
1. Read: **QUICK_REFERENCE.md** - Examples and function lookup
2. Install: Follow system requirements section
3. Try: Code examples from "Quick Start" section
4. Extend: Reference advanced examples and intelligent wrappers

### I want to UNDERSTAND the architecture
1. Read: **README_EXPLORATION.md** - Get overview of strengths
2. Deep dive: **CODEBASE_STRUCTURE.md** - Detailed architecture
3. Explore: Source code in `/home/user/LFA/R/`
4. Reference: QUICK_REFERENCE.md for function signatures

### I want to IMPROVE/EXTEND the package
1. Read: **README_EXPLORATION.md** - Recommended next steps
2. Study: **CODEBASE_STRUCTURE.md** - Architecture and patterns
3. Review: intelligent_wrapper.R - Integration pattern
4. Reference: rules_engine.R - How to add new rules
5. Check: QUICK_REFERENCE.md - Current function signatures

### I want to EVALUATE against other packages
1. Read: **README_EXPLORATION.md** - Unique advantages section
2. Check: CODEBASE_STRUCTURE.md - Method coverage table
3. Compare: Feature summary table
4. Benchmark: Code statistics section

---

## Key Numbers at a Glance

| Metric | Count | Where |
|--------|-------|-------|
| R Files | 32 | R/ directory |
| Total Lines of Code | 12,136+ | All files |
| Exported Functions | 210+ | NAMESPACE |
| S3 Methods | 33+ | methods.R, print_methods*.R |
| Expert Rules | 500+ | rules_engine.R (638 lines) |
| Validated Scenarios | 10,000+ | scenario_database.R (616 lines) |
| Meta-Analysis Methods | 15+ | Core + Advanced files |
| Visualization Types | 20+ | visualizations, advanced_plots, ggplot2 |
| Effect Size Conversions | 17 | effect_size_conversions.R |
| AI Functions | 7 | llm_integration.R (597 lines) |
| Intelligent Wrappers | 6 | intelligent_wrapper.R (576 lines) |

---

## Core Components Overview

### Meta-Analysis Implementation
**File Location**: `R/` directory (8-10 core files)

- **Basic**: cbamm_fast, cumulative_meta_analysis, meta_regression
- **Advanced**: bayesian_ma, network_meta_analysis, ipd_meta_analysis
- **Support**: publication_bias, sensitivity_analysis, validation
- **Modern**: ml_meta_analysis, living_systematic_review, dose_response

### AI Integration
**File Location**: `R/llm_integration.R` (597 lines)

- Local Llama 3 via Ollama (private, localhost:11434)
- 7 AI functions (method selection, interpretation, quality, screening, etc.)
- Rule-based fallback when AI unavailable
- Fully configurable temperature and token limits

### Rules Engine
**File Location**: `R/rules_engine.R` (638 lines)

- 500+ expert rules across 10 categories
- Pre/per-study/post-analysis execution
- Severity-based actions (ERROR, WARN, RECOMMEND, INTERPRET)
- Extensible rule framework

### Scenario Database
**File Location**: `R/scenario_database.R` (616 lines)

- 10,000+ validated scenarios
- 14 domain categories
- Reference matching for data analysis
- Use for validation and comparison

### Visualizations
**File Location**: `R/visualizations.R`, `R/advanced_plots.R`, `R/ggplot_visualizations.R`

- Traditional: forest, funnel, cumulative
- Advanced: baujat, radial, contour
- Modern ggplot2: 6 publication-quality functions
- Interactive: Shiny dashboard

### Intelligent Wrappers
**File Location**: `R/intelligent_wrapper.R` (576 lines)

- intelligent_cbamm() - 7-step pipeline
- intelligent_publication_bias() - Multi-test + AI
- intelligent_sensitivity() - Stability + AI
- intelligent_nma() - Network + AI
- intelligent_quality_assessment() - Quality + AI
- autopilot_meta_analysis() - Fully automated

---

## Statistical Methods by Category

### Core Methods
- Random-effects meta-analysis (DL, REML)
- Heterogeneity estimation (I², τ², Q)
- Prediction intervals
- Study weighting

### Advanced Methods
- **Bayesian**: MCMC sampling, posterior distributions, Bayes factors
- **Network**: 3+ treatment comparison, consistency testing, SUCRA ranking
- **IPD**: One-stage, two-stage, interaction testing
- **DTA**: Bivariate models, SROC, likelihood ratios
- **Dose-Response**: Linear, quadratic, spline models, ED50
- **Multivariate**: Correlated outcomes, joint analysis
- **Living**: Sequential updates, stopping rules

### Support Methods
- **Publication Bias**: Egger test, PET-PEESE, Trim-Fill
- **Sensitivity**: Leave-one-out, influence, bootstrap
- **Meta-Regression**: Study-level covariates, multiple predictors
- **Subgroup Analysis**: Between-group comparisons
- **ML Approaches**: Random forest, neural networks

---

## Integration Points

The package integrates with:
- **metafor** - Bidirectional conversion
- **meta** - Data format compatibility
- **RevMan** - Import functionality
- **CMA** - Export support
- **Ollama** - Local AI (optional)
- **ggplot2** - Modern visualizations
- **Shiny** - Interactive dashboard

---

## Rule Categories Summary

| Category | Rules | Purpose |
|----------|-------|---------|
| Methodology | 100+ | Sample size, method selection |
| Quality | 80+ | Precision, bias risk |
| Heterogeneity | 60+ | I² interpretation |
| Publication Bias | 50+ | Test selection |
| Interpretation | 80+ | Significance assessment |
| Reporting | 60+ | PRISMA compliance |
| Sensitivity | 40+ | Stability checking |
| Effect Size | 30+ | Magnitude interpretation |
| Sample Size | 20+ | Adequacy assessment |
| Data Validation | 20+ | Input validation |

---

## File Organization

### Core Analysis (8 files)
Essential meta-analysis functionality

### Advanced Methods (10 files)
Specialized approaches for complex scenarios

### AI & Rules (3 files)
Intelligent systems and decision support

### Visualization (4 files)
Plotting and interactive features

### Support (6 files)
Utilities, reporting, data handling

### Total: 32 files, 12,136+ lines

---

## Getting the Most Out of This Documentation

### For Reference
- **Bookmark**: All three .md files are in `/home/user/LFA/`
- **Search**: Use your editor's find function (Ctrl+F / Cmd+F)
- **Links**: Internal cross-references in CODEBASE_STRUCTURE.md
- **Tables**: Quick lookup tables in all documents

### For Learning
1. Start with QUICK_REFERENCE.md for overview
2. Move to README_EXPLORATION.md for context
3. Deep dive with CODEBASE_STRUCTURE.md for details
4. Explore actual code in `/home/user/LFA/R/`

### For Development
1. Review README_EXPLORATION.md "Recommended Next Steps"
2. Study intelligent_wrapper.R for integration patterns
3. Reference rules_engine.R for rule implementation
4. Follow S3 method patterns from methods.R

---

## Contact & Resources

- **GitHub**: https://github.com/mahmood726-cyber/LFA
- **Issues**: Report bugs or request features
- **Documentation**: Use `?function_name` in R for help
- **Vignettes**: Coming soon via `browseVignettes("cbamm")`

---

## Version Information

- **Package**: cbamm v2.0.0
- **Type**: AI-Powered Meta-Analysis Toolkit
- **Last Updated**: See git commits
- **Current Branch**: claude/repo-review-improvements-011CUpqXNxQuCtJqydP1bYvN
- **Documentation Date**: 2025-11-05

---

## Quick Start

### Minimal Code Example
```r
library(cbamm)

# Create data
data <- data.frame(
  study = c("A", "B", "C"),
  effect = c(0.5, 0.6, 0.4),
  se = c(0.1, 0.12, 0.15)
)

# Analyze
result <- cbamm_fast(data)
print(result)
```

### With AI & Rules
```r
# Intelligent analysis
result <- intelligent_cbamm(
  data,
  enable_ai = TRUE,
  enable_rules = TRUE
)
```

### Autopilot Mode
```r
# Fully automatic (no decisions needed)
result <- autopilot_meta_analysis(data)
```

See QUICK_REFERENCE.md for 15 more examples.

---

## Recommended Reading Order

1. **This file** (2 min) - Understanding the documentation structure
2. **README_EXPLORATION.md** (15 min) - Executive summary and findings
3. **QUICK_REFERENCE.md** (20 min) - Code examples and quick lookup
4. **CODEBASE_STRUCTURE.md** (45 min) - Detailed technical analysis
5. **Source code** (as needed) - Actual implementations

---

**Total Documentation**: 1,563 lines across 3 files + 12,136+ lines of source code

Happy exploring!
