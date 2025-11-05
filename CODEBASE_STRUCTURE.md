# LFA Codebase Structure - Comprehensive Analysis

**Package**: cbamm (Collaborative Bayesian Adaptive Meta-Analysis Methods)  
**Current Version**: 2.0.0 (AI-Powered with Local Llama 3)  
**Total Implementation**: 32 R files | 12,136+ lines of code | 210+ exported functions

---

## 1. META-ANALYSIS METHODS IMPLEMENTATION

### Core Methods (cbamm_fast)
**File**: `R/methods.R`
- **Random-effects meta-analysis**: DerSimonian-Laird (DL), Restricted Maximum Likelihood (REML)
- **Heterogeneity estimation**: I² calculation, τ² (tau-squared) estimation, Q-statistic
- **Prediction intervals**: Generate prediction ranges for future studies
- **Effect size standardization**: Support for multiple effect size metrics
- **Study weights**: Inverse-variance weighting with confidence intervals

### Advanced Meta-Analysis Methods (25+ specialized approaches)

#### 1. **Cumulative Meta-Analysis** (`R/cumulative_meta_analysis.R`)
- Temporal ordering by publication year
- Automated stability detection 
- Evidence accumulation tracking
- Stability threshold (default: 0.01 change)
- Cumulative results visualization

#### 2. **Meta-Regression** (`R/meta_regression.R`)
- Study-level covariate modeling
- Multiple predictor support
- Residual heterogeneity assessment
- R² variance explained calculation
- Robust variance estimation

#### 3. **Bayesian Meta-Analysis** (`R/bayesian_meta_analysis.R`)
- MCMC sampling (Metropolis-Hastings)
- Posterior distributions for effect size and τ
- Prior specification (normal for effects, half-Cauchy for τ)
- MCMC diagnostics (ESS, acceptance rates)
- Bayes Factor calculations
- Posterior/prior predictive checks

#### 4. **Network Meta-Analysis** (`R/network_meta_analysis.R`)
- Multi-treatment comparison (≥3 treatments)
- Contrast-based approach
- Consistency testing
- Treatment ranking with SUCRA scores
- Network structure visualization
- Indirect treatment comparison

#### 5. **Individual Patient Data (IPD) Meta-Analysis** (`R/ipd_meta_analysis.R`)
- One-stage approach (simultaneous analysis)
- Two-stage approach (pooled estimates)
- Patient-level covariate analysis
- Interaction testing across studies
- Missing data imputation strategies
- Comparison with aggregate data

#### 6. **Diagnostic Test Accuracy (DTA)** (`R/dta_advanced.R`)
- Bivariate models for sensitivity/specificity
- Summary ROC (SROC) curves
- Likelihood ratios calculation
- Diagnostic odds ratios
- Threshold effect assessment
- Multiple test evaluation

#### 7. **Dose-Response Meta-Analysis** (`R/dose_response.R`)
- Linear relationship modeling
- Quadratic polynomial fitting
- Restricted cubic spline models
- ED50 (50% effective dose) calculation
- Non-linearity testing
- Dose stratification analysis

#### 8. **Multivariate Meta-Analysis** (`R/multivariate_meta_analysis.R`)
- Joint analysis of multiple correlated outcomes
- Correlation structure modeling
- Multivariate regression
- Outcome difference testing
- Shared effect estimation

#### 9. **Living Systematic Reviews** (`R/living_systematic_review.R`)
- Continuous update mechanisms
- Sequential meta-analysis
- Stopping rules (trial sequential analysis boundaries)
- Version comparison functionality
- Dynamic monitoring setup
- Real-time evidence accumulation

#### 10. **Publication Bias Assessment** (`R/publication_bias.R`)
- **Egger's Regression Test**: Funnel plot asymmetry
- **PET-PEESE**: Precision Effect Test and Precision Effect Estimate with Standard Error
- **Trim-and-Fill**: Imputation of missing studies
- **Contour-Enhanced Funnel Plots**: Visual assessment
- Publication bias models (2024 latest methods)

#### 11. **Sensitivity Analysis** (`R/sensitivity_analysis.R`)
- Leave-one-out analysis
- Influence diagnostics (Cook's D, DFBETAS)
- Bootstrap meta-analysis (1000+ iterations)
- Cumulative influence assessment
- Outlier detection via IQR method
- Jackknife analysis

#### 12. **Machine Learning Methods** (`R/ml_meta_analysis.R`)
- **Random Forest**: Non-linear heterogeneity modeling, variable importance
- **Neural Networks**: Effect size prediction, heterogeneity detection
- **Ensemble Methods**: Combined predictions from multiple ML models
- **Automated Outlier Detection**: ML-based study filtering
- Out-of-bag error estimation
- Heterogeneity explanation quantification

#### 13. **Prediction Models** (`R/prediction_models.R`)
- Future study effect prediction
- Study characteristic-based forecasting
- Calibration plots
- Performance evaluation
- Confidence/prediction intervals

#### 14. **Transport Weights** (`R/transport_weights.R`)
- Generalizability assessment
- Population characteristic weighting
- Simple distance-based method
- Propensity-based weighting
- Effective sample size calculation

#### 15. **Performance & Parallel Processing** (`R/performance_parallel.R`)
- Parallel bootstrap meta-analysis
- Vectorized heterogeneity calculations
- Caching of meta-analysis results
- Batch processing
- Optimized leave-one-out analysis

#### 16. **Data Validation** (`R/validation.R`)
- Data structure validation
- Required column verification
- Missing data detection
- Value range checking
- Duplicate study detection

#### 17. **Effect Size Conversions** (`R/effect_size_conversions.R`)
- **17 conversion functions** including:
  - Cohen's d ↔ Hedges' g (bias correction)
  - d ↔ correlation r
  - d ↔ log odds ratio
  - OR ↔ Risk Ratio
  - Fisher's Z transformation
  - 2x2 table calculations (OR, RR, RD)
  - Bias-corrected conversions

#### 18. **Subgroup Analysis** (Integrated in methods.R)
- Between-group heterogeneity testing
- Within-group effect estimation
- Multiple subgroup support
- Interaction testing

---

## 2. AI/LLAMA INTEGRATION

### Architecture
**Primary File**: `R/llm_integration.R` (597 lines)

### Core LLM Functions

#### 1. **Initialize LLM** (`init_llm`)
- Model: Llama 3 (default)
- Local Ollama endpoint: `http://localhost:11434/api/generate`
- Configuration options:
  - Temperature (sampling): 0.7 (default)
  - Max tokens: 2000 (default)
  - Optional cloud API key support
- Connection testing at initialization
- Graceful degradation if unavailable

#### 2. **AI Method Selection** (`ai_method_selection`)
```
Input Analysis:
  - Number of studies (k)
  - Mean effect size
  - Effect variance
  - Mean standard error
  - I² heterogeneity (preliminary)
  - Available moderators
  
AI Recommendations:
  1. Best pooling method (DL, REML, ML, PM, Bayesian)
  2. Meta-regression necessity
  3. Publication bias methods
  4. Sensitivity analyses needed
  5. Visualization recommendations
```

#### 3. **AI Result Interpretation** (`ai_interpret_results`)
Analyzes meta-analysis results to provide:
- Statistical significance assessment
- Clinical/practical significance
- Heterogeneity interpretation
- Strength of evidence evaluation
- Limitations and caveats
- Practice/policy recommendations
- Future research directions

#### 4. **AI Quality Assessment** (`ai_quality_assessment`)
Evaluates studies on:
- Risk of bias (Low/Moderate/High)
- Precision assessment
- Reporting quality
- Overall quality score (1-10)
- Study-specific concerns

#### 5. **AI Literature Screening** (`ai_literature_screening`)
Title/abstract screening with:
- Inclusion/exclusion decisions
- Confidence levels (0-100%)
- Decision rationales
- High-throughput processing
- Manual review flagging

#### 6. **AI Data Extraction** (`ai_data_extraction`)
- Structured text parsing
- Template-based extraction
- Confidence scoring
- Manual extraction fallback
- Data validation integration

#### 7. **AI Research Gap Analysis** (`ai_research_gaps`)
Identifies:
- Underrepresented populations/settings
- Methodological gaps
- Unmeasured outcomes
- Areas of high uncertainty
- Priority recommendations

### Fallback System
When Llama not available, all functions default to:
- Rule-based recommendations
- Evidence-based defaults
- Fallback response generation
- Graceful degradation

---

## 3. VISUALIZATION CAPABILITIES

### Traditional Visualizations (`R/visualizations.R`)
- **Forest plots**: Enhanced design with study weights, CIs
- **Funnel plots**: Publication bias assessment
- **Cumulative dashboard**: Multi-panel evidence accumulation
- Basic R graphics output

### Advanced Traditional Plots (`R/advanced_plots.R`)
1. **Baujat Plot**: Identifies heterogeneity sources
   - X-axis: Study influence
   - Y-axis: Study outlierness
   - Bubble size: Study weight

2. **Radial (Galbraith) Plot**: Outlier detection
3. **Contour Funnel Plot**: Enhanced funnel with confidence regions
4. **L'Abbé Plot**: Diagnostic test visualization
5. **Leave-One-Out Plots**: Sensitivity visualization
6. **Influence Diagnostic Plots**: Cook's D visualization

### Modern ggplot2 Visualizations (`R/ggplot_visualizations.R` - 585 lines)

#### 1. **gg_forest_plot**
- Publication-quality forest plot
- Custom coloring by variables (year, quality, etc.)
- Optional study weights as point size
- Prediction intervals display
- Clean modern aesthetics
- ggplot2 native implementation

#### 2. **gg_funnel_plot**
- Modern funnel plot design
- Asymmetry visualization
- Contour regions optional
- Publication bias assessment

#### 3. **gg_labbe_plot**
- Diagnostic sensitivity/specificity
- Bivariate visualization
- Color-coded by study characteristics

#### 4. **gg_cumulative_plot**
- Evidence accumulation over time
- Effect size trajectory
- Heterogeneity evolution
- Stability indication

#### 5. **gg_gosh_plot**
- GOSH (Graphical Display of Study Heterogeneity)
- Study combination effects
- Pattern identification

#### 6. **gg_metareg_plot**
- Meta-regression scatter
- Line of best fit
- Confidence regions
- Residual diagnostics

### Shiny Dashboard (`R/package_integration.R`)
- Interactive point-and-click analysis
- Real-time updating
- Multiple visualizations
- Export functionality
- Live filtering and subsetting

### Export Formats
- PDF/PNG: High-resolution publication ready
- Data export: CSV, Excel
- Report generation: R Markdown, HTML
- PRISMA formats

---

## 4. RULES-BASED SYSTEMS

### Architecture
**Primary File**: `R/rules_engine.R` (638 lines)

### Rule Categories & Counts

#### 1. **Methodology Rules** (100+)
```
METH001-METH100 covering:
- Sample size requirements (METH001-010)
- Heterogeneity-based method selection (METH011-030)
- Effect size distribution validation (METH031-050)
- Standard error validation (METH051-070)
- Advanced methodology (METH071-100)

Examples:
METH001: k < 3 → ERROR: "Insufficient studies"
METH002: k < 5 → WARN: "Small MA, use DL with caution"
METH003: 5 ≤ k < 10 → RECOMMEND: "REML recommended"
METH004: k ≥ 10 → RECOMMEND: "Multiple methods suitable"
METH011: I² > 75% → RECOMMEND: "High heterogeneity, use RE"
METH072: Duplicate studies → ERROR: "Duplicate study IDs"
METH073: >10% missing → WARN: "Substantial missing data"
```

#### 2. **Quality Assessment Rules** (80+)
```
QUAL101-180 covering:
- Precision-based scoring (QUAL101-120)
- Sample size quality (QUAL121-140)
- Comprehensive quality criteria (QUAL141-180)

Examples:
QUAL101: SE < 0.1 → "High precision study"
QUAL102: SE > 0.5 → "Low precision, limited influence"
QUAL121: n < 30 → "Small sample, increased bias risk"
QUAL122: n ≥ 100 → "Large sample, good power"
```

#### 3. **Heterogeneity Rules** (60+)
```
HET201-260 covering:
- I² interpretation thresholds
- Heterogeneity source exploration

Examples:
HET201: I² < 25% → "Low, results consistent"
HET202: 25% ≤ I² < 50% → "Moderate, some variation"
HET203: 50% ≤ I² < 75% → "Substantial, explore moderators"
HET204: I² ≥ 75% → WARN: "Very high, pooling questionable"
```

#### 4. **Publication Bias Rules** (50+)
```
BIAS301-350 covering:
- Sample size requirements for tests
- Test selection guidance
- Funnel plot interpretation

Examples:
BIAS301: k < 10 → WARN: "Too few for bias tests"
BIAS302: k ≥ 10 → RECOMMEND: "Egger & funnel plot"
```

#### 5. **Interpretation Rules** (80+)
```
INTERP401-480 covering:
- Statistical significance interpretation
- Effect size interpretation

Examples:
INTERP401: p < 0.001 → "Highly significant"
INTERP402: 0.05 ≤ p < 0.10 → "Marginally significant"
INTERP403: p ≥ 0.10 → "Not statistically significant"
```

#### 6. **Reporting Rules** (60+)
```
REP501-560 covering:
- PRISMA compliance
- Standard reporting requirements

Examples:
REP501: "Report per PRISMA guidelines"
REP502: "Always report I², τ², Q statistics"
```

#### 7. **Sensitivity Rules** (40+)
```
SENS561-600 covering:
- Sensitivity analysis recommendations

Examples:
SENS561: k ≥ 5 → RECOMMEND: "Leave-one-out analysis"
SENS562: I² > 50% → RECOMMEND: "Explore heterogeneity sources"
```

#### 8. **Effect Size Rules** (30+)
```
ES601-630 covering:
- Effect size magnitude interpretation

Examples:
ES601: |d| < 0.2 → "Small effect"
ES602: 0.5 ≤ |d| < 0.8 → "Medium effect"
ES603: |d| ≥ 0.8 → "Large effect"
```

#### 9. **Sample Size Rules** (20+)
```
SS631-650 covering:
- Sample size adequacy assessment
```

#### 10. **Data Validation Rules** (20+)
```
VAL651-670 covering:
- Input data validation
```

### Rule Application System
**File**: `R/rules_engine.R`

#### Execution Flow:
1. **Pre-Analysis Rules** (validation before analysis)
   - Checks on data structure
   - Identifies critical issues
   - Stops analysis if critical errors
   - Warns about high-priority issues

2. **Per-Study Rules** (applied to each study)
   - Quality assessment
   - Precision evaluation
   - Bias risk assessment

3. **Post-Analysis Rules** (validation after analysis)
   - Result validation
   - Interpretation guidance
   - Heterogeneity assessment
   - Publication bias evaluation

#### Rule Structure:
```
Each rule contains:
- id: Unique identifier (METH001, QUAL101, etc.)
- condition: Function testing rule
- action: Response type (ERROR, WARN, RECOMMEND, INTERPRET, etc.)
- message: User-friendly guidance
- severity: critical, high, medium, low, info
```

#### Output Format:
```
Rules Result object with:
- Total rules triggered count
- Severity breakdown (critical, high, medium, low)
- Detailed messages for critical/high severity
- Grouped by rule category
```

---

## 5. METHODS & RESULTS GENERATION FEATURES

### Intelligent Wrappers (`R/intelligent_wrapper.R` - 576 lines)

#### 1. **Intelligent cbamm** (`intelligent_cbamm`)
Combines meta-analysis with AI and rules:
```
Process:
1. PRE-ANALYSIS: Apply 500+ rules for validation
2. AI SELECTION: LLM recommends best methods
3. SCENARIO MATCHING: Find similar studies (10K+ database)
4. CORE ANALYSIS: Run cbamm_fast
5. POST-ANALYSIS: Validate results with rules
6. AI INTERPRETATION: Generate insights
7. RESEARCH GAPS: Identify future directions

Output:
- Complete meta-analysis results
- Rules assessment
- AI interpretation text
- Research gaps analysis
- Scenario similarity count
- Metadata (timestamp, systems enabled)
```

#### 2. **Intelligent Publication Bias** (`intelligent_publication_bias`)
Enhanced bias assessment:
- Rules validation (≥10 studies for tests)
- Multiple bias tests (Egger, PET-PEESE, Trim-Fill)
- AI interpretation of results
- Integrated risk assessment

#### 3. **Intelligent Sensitivity Analysis** (`intelligent_sensitivity`)
Smart sensitivity evaluation:
- Leave-one-out analysis
- Influence diagnostics
- Stability verdict (STABLE/UNSTABLE)
- AI recommendations for exclusion
- Relative stability metric

#### 4. **Intelligent Network MA** (`intelligent_nma`)
Network analysis with AI:
- Rules validation (≥3 treatments)
- Network consistency checking
- Treatment ranking with SUCRA
- AI clinical interpretation
- Top treatment recommendation

#### 5. **Intelligent Quality Assessment** (`intelligent_quality_assessment`)
Comprehensive quality evaluation:
- Rules-based scoring (1-10 scale)
- Per-study precision assessment
- Risk of bias classification
- Summary statistics
- AI nuanced assessment option

#### 6. **Autopilot Meta-Analysis** (`autopilot_meta_analysis`)
**Fully automated analysis** (user provides data only):
```
STEP 1: Data Validation (500+ rules)
STEP 2: AI Method Selection
STEP 3: Core Meta-Analysis
STEP 4: Heterogeneity Assessment
STEP 5: Publication Bias
STEP 6: Sensitivity Analysis
STEP 7: Quality Assessment
STEP 8: Research Gap Analysis
STEP 9: Visualization Generation

Returns:
- Complete analysis package
- All components integrated
- Ready-to-publish results
- No user decisions required
```

### Scenario Database (`R/scenario_database.R` - 616 lines)

#### Database Size: 10,000+ scenarios

#### Coverage by Domain:
1. **Clinical Trials** (2,000 scenarios)
   - Drug efficacy RCTs
   - Surgical interventions
   - Behavioral therapies
   - Device trials
   - Vaccine efficacy

2. **Observational Studies** (1,500 scenarios)
   - Cohort designs
   - Case-control designs
   - Cross-sectional studies
   - 5 exposure types (dietary, environmental, occupational, lifestyle, genetic)

3. **Diagnostic Accuracy** (1,000 scenarios)
   - Imaging tests
   - Laboratory tests
   - Clinical exams
   - Questionnaires
   - Genetic tests

4. **Epidemiology** (1,000 scenarios)
   - Mortality studies
   - Incidence studies
   - Prevalence studies
   - Risk factor analysis
   - Survival studies

5. **Psychology** (1,000 scenarios)
   - Cognitive studies
   - Social psychology
   - Clinical psychology
   - Developmental studies
   - Neuropsychology

6. **Education** (800 scenarios)
   - Technology interventions
   - Curriculum studies
   - Teaching methods
   - Assessment innovations

7. **Economics** (700 scenarios)

8. **Environmental** (600 scenarios)

9. **Methodology** (500 scenarios)
   - Method comparisons
   - Simulations
   - Power analyses

10. **Edge Cases** (400 scenarios)
    - Extreme heterogeneity (I² > 90%)
    - Zero events
    - Single-arm studies
    - Very small studies
    - Extreme effect sizes
    - Opposite direction effects

11. **Complex Designs** (500 scenarios)
    - Cluster RCTs
    - Stepped wedge designs
    - Crossover trials
    - Factorial designs

12. **Rare Diseases** (300 scenarios)
    - Very few studies (3-8)
    - High uncertainty
    - Small sample sizes

13. **Implementation Science** (300 scenarios)
    - Hospital settings
    - Clinic settings
    - Community settings

14. **Prognostic** (400 scenarios)
    - Hazard ratio analysis
    - Survival modeling

#### Scenario Features:
Each scenario includes:
- Scenario ID (domain-specific prefix)
- Scenario type and subtype
- Number of studies (k)
- True/target effect size
- Target heterogeneity
- Simulated data with effect sizes and SEs
- Study characteristics (year, quality, design, blinding)
- Expected challenges
- Expected methods
- Domain-specific metadata

### Reporting & Export (`R/reporting.R`)

#### Report Generation:
- **Summary tables**: Markdown, LaTeX, HTML formats
- **Study data export**: CSV/Excel
- **Text reports**: Comprehensive analysis summaries
- **PRISMA checklists**: Automated compliance verification

#### Integration with Other Tools (`R/package_integration.R`)
```
Bidirectional conversion:
- metafor ↔ cbamm
- meta package ↔ cbamm
- RevMan imports
- CMA export
- Cross-method comparison
- Auto method selection
```

### Utilities (`R/utils.R`)

#### Calculation Functions:
- I² calculation (multiple methods)
- τ² estimation
- Safe division with NA handling
- SE ↔ Variance conversion
- Confidence interval formatting
- P-value formatting

#### Data Functions:
- Outlier detection (IQR-based)
- Data standardization
- Prediction interval calculation
- Effect size scaling

---

## FEATURE SUMMARY TABLE

| Category | Methods | Key Functions | Advanced Features |
|----------|---------|----------------|-------------------|
| **Meta-Analysis** | 15+ | cbamm_fast, cumulative_ma, subgroup_analysis | Parallel processing, caching |
| **AI Integration** | 7 | init_llm, ai_method_selection, ai_interpret_results | Fallback system, local Ollama |
| **Visualization** | 20+ | gg_forest_plot, forest_plot_enhanced, baujat_plot | ggplot2 + base, interactive Shiny |
| **Rules Engine** | 500+ | init_rules_engine, apply_rules | Pre/post-analysis validation |
| **Scenarios** | 10,000+ | init_scenario_database, find_scenarios | Domain-specific, edge cases |
| **Publication Bias** | 3 main | egger_test, pet_peese, trim_fill | Latest 2024 methods |
| **Sensitivity** | 5 main | leave_one_out, influence_diagnostics, bootstrap_ma | Comprehensive diagnostics |
| **Effect Sizes** | 17 | d_to_g, or_to_log_or, fisher_z, etc. | Bias-corrected, reversible |
| **DTA** | 5 main | dta_bivariate, sroc_plot, likelihood_ratios | Threshold effects |
| **Network MA** | 5 main | network_meta_analysis, rank_treatments | Consistency testing |
| **Bayesian** | 4 main | bayesian_ma, bayesian_metareg | MCMC, priors, Bayes factors |
| **IPD** | 4 main | ipd_onestage, ipd_twostage | Interaction analysis |
| **Machine Learning** | 4 main | rf_meta_analysis, nn_meta_analysis | Automated outlier detection |
| **Reporting** | 5 main | generate_report, prisma_checklist | Multiple formats |

---

## INTEGRATION & WORKFLOW

### Complete Workflow Chain:
```
DATA → VALIDATION (500+ rules)
    ↓
AI METHOD SELECTION
    ↓
SCENARIO MATCHING (10K+ database)
    ↓
META-ANALYSIS (15+ methods)
    ↓
PUBLICATION BIAS ASSESSMENT
    ↓
SENSITIVITY ANALYSIS
    ↓
QUALITY ASSESSMENT
    ↓
AI INTERPRETATION
    ↓
RESEARCH GAPS ANALYSIS
    ↓
VISUALIZATION (20+ plot types)
    ↓
REPORTING & EXPORT
```

### System States:
1. **Rules-only mode** (no AI dependency)
2. **AI-enabled mode** (with local Ollama)
3. **Autopilot mode** (fully automated)
4. **Expert mode** (manual method selection)

---

## TECHNICAL ARCHITECTURE

### Dependencies
- Core: stats, graphics, utils, methods, parallel, digest
- Optional: metafor, ggplot2, dplyr, gridExtra, shiny, DT, meta, httr, jsonlite

### Performance Features
- Parallel processing (parallel package)
- Result caching (digest-based)
- Vectorized operations
- Batch processing capability
- Bootstrap acceleration

### Code Quality
- S3 method dispatch (33+ methods)
- roxygen2 documentation
- roxygen note: 7.3.3
- testthat framework (Edition 3)
- Continuous integration (GitHub Actions)

---

## STATISTICAL METHODS BY PUBLICATION YEAR

### 2024-2025 Methods
- Publication bias (latest)
- Living reviews (sequential)
- ML heterogeneity modeling
- IPD interaction analysis
- Dose-response splines

### 2020-2023 Methods
- Bayesian MA
- Multivariate MA
- Network MA consistency
- Prediction models
- Shiny integration

### 2015-2019 Methods
- Random forest application
- Neural network heterogeneity
- DTA bivariate models
- Transport weights

### Classic Methods
- DerSimonian-Laird
- REML estimation
- Egger's test
- Trim and fill
- Meta-regression

