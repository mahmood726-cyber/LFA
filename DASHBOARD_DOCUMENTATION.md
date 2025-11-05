# SHINY DASHBOARD DOCUMENTATION - LFA/cbamm Package

## Overview

The LFA/cbamm package includes a **state-of-the-art interactive Shiny dashboard** using the **bs4Dash framework** (AdminLTE 3), providing a professional, modern interface for comprehensive meta-analysis with **highest-resolution downloads (300 DPI)** suitable for publication.

---

## GUI Framework: bs4Dash

### Why bs4Dash?

**bs4Dash** is the most advanced Shiny dashboard framework, based on AdminLTE 3:
- **Modern design**: Clean, professional interface
- **Responsive**: Works on desktop, tablet, mobile
- **Rich widgets**: Value boxes, cards, tabs, menus
- **Customizable**: Themes, colors, layouts
- **Production-ready**: Used in enterprise applications

### Key Features:
```r
library(bs4Dash)

ui <- bs4DashPage(
  header = bs4DashNavbar(...),
  sidebar = bs4DashSidebar(...),
  body = bs4DashBody(...),
  footer = bs4DashFooter(...)
)
```

---

## Dashboard Features

### 1. Navigation Structure

**Sidebar Menu:**
- **DATA & ANALYSIS**
  - Upload Data
  - Quick Analysis

- **VISUALIZATIONS**
  - Dashboard (12-panel comprehensive)
  - Forest Plots
  - Funnel & Bias Plots
  - Sensitivity Analysis
  - Publication Bias
  - Advanced Plots (Multiverse, P-Curve, GRADE, 3D Heterogeneity)

- **ANALYSIS**
  - Heterogeneity
  - Sensitivity
  - Moderators
  - Bayesian

- **PUBLICATION**
  - Methods Section (500+ rules)
  - Results Section (500+ rules)
  - Complete Package

- **AI ASSISTANT**
  - LLM Interpretation (Gemini/Ollama)
  - AI Recommendations

### 2. Value Boxes (Real-time Metrics)

Four dynamic value boxes display key statistics:
```r
bs4ValueBox(
  value = nrow(data),
  subtitle = "Studies Included",
  icon = icon("book"),
  status = "info"
)
```

Metrics shown:
- Number of studies
- Pooled effect size
- I² heterogeneity (color-coded: green < 50%, yellow 50-75%, red > 75%)
- P-value (color-coded: green if significant)

### 3. Interactive Plots

All plots are reactive and update automatically:
- Forest plots (enhanced with weights, CIs)
- Funnel plots (contour-enhanced)
- Publication bias plots (4-panel)
- Sensitivity plots (leave-one-out, influence, cumulative)
- Comprehensive 12-panel dashboard
- 30+ specialized plots

---

## HIGH-RESOLUTION DOWNLOADS (300 DPI)

### Download Capabilities

All downloads use **300 DPI** resolution, the gold standard for academic publishing:

#### 1. Individual Plots

**Forest Plot:**
```r
downloadHandler(
  filename = "forest_plot_2025-11-05.png",
  content = function(file) {
    png(file, width = 3600, height = 3000, res = 300, type = "cairo")
    forest_plot_enhanced(data, result)
    dev.off()
  }
)
```
- **Size**: 3600 × 3000 pixels
- **Physical**: 12 × 10 inches at 300 DPI
- **Format**: PNG with Cairo graphics
- **Quality**: Publication-ready

**Funnel Plot:**
- **Size**: 3000 × 3000 pixels (square)
- **Physical**: 10 × 10 inches at 300 DPI
- **Features**: Contour-enhanced, significance regions

**Dashboard (12 panels):**
- **Size**: 4800 × 3600 pixels
- **Physical**: 16 × 12 inches at 300 DPI
- **Includes**: All comprehensive visualizations

**Publication Bias Plots (4 panels):**
- **Size**: 4800 × 3600 pixels
- **Panels**: Funnel, Egger's regression, Trim-fill, P-curve
- **Layout**: 2 × 2 grid

**Sensitivity Plots (4 panels):**
- **Size**: 4200 × 3000 pixels
- **Panels**: Leave-one-out, DFFITS, Cumulative, Distribution
- **Layout**: 2 × 2 grid

#### 2. Document Downloads

**Methods Section:**
- **Formats**: TXT, DOCX
- **Features**: 500+ automated rules, AI enhancement
- **Templates**: Structured, Narrative, PRISMA
- **Detail levels**: Concise, Standard, Comprehensive

**Results Section:**
- **Formats**: TXT, DOCX
- **Features**: 500+ automated rules, tables, interpretation
- **Customizable**: Include/exclude tables, interpretation

**Complete PDF Report:**
- **Multi-page**: Summary, Forest, Funnel, Sensitivity
- **Size**: 11 × 8.5 inches (letter)
- **Professional layout**: Publication-ready

#### 3. Complete Package (ZIP)

Downloads everything in one archive:
```
meta_analysis_complete_2025-11-05.zip/
├── data.csv
├── forest_plot.png (300 DPI)
├── funnel_plot.png (300 DPI)
├── dashboard.png (300 DPI)
├── methods.txt
├── results.txt
└── summary.txt
```

---

## Technical Specifications

### Image Settings

All PNG downloads use:
```r
png(
  filename,
  width = pixels_width,
  height = pixels_height,
  res = 300,              # 300 DPI
  type = "cairo"          # Superior rendering
)
```

**Why Cairo?**
- Anti-aliased graphics
- Superior font rendering
- Better color accuracy
- Professional quality

### Resolution Table

| Plot Type | Pixels | Inches @ 300 DPI | Aspect Ratio |
|-----------|--------|------------------|--------------|
| Forest | 3600 × 3000 | 12 × 10 | 1.2:1 |
| Funnel | 3000 × 3000 | 10 × 10 | 1:1 (square) |
| Dashboard | 4800 × 3600 | 16 × 12 | 4:3 |
| Bias (4-panel) | 4800 × 3600 | 16 × 12 | 4:3 |
| Sensitivity (4-panel) | 4200 × 3000 | 14 × 10 | 1.4:1 |

### File Formats

- **PNG**: Raster, lossless compression, best for complex plots
- **PDF**: Vector, scalable, best for simple plots and documents
- **DOCX**: Microsoft Word, editable text
- **TXT**: Plain text, universal compatibility
- **ZIP**: Archive, contains all outputs

---

## Launching the Dashboard

### Basic Launch

```r
library(cbamm)

# Launch with default settings
launch_meta_dashboard()
```

### Advanced Launch

```r
# Custom port
launch_meta_dashboard(port = 3838)

# Specific LLM provider
launch_meta_dashboard(llm_provider = "ollama")

# With API key
launch_meta_dashboard(
  llm_provider = "gemini",
  gemini_api_key = "your-api-key-here"
)

# Disable LLM features
launch_meta_dashboard(enable_llm = FALSE)
```

### Configuration Options

```r
launch_meta_dashboard(
  port = NULL,                    # Auto-select port
  launch.browser = TRUE,          # Open browser automatically
  enable_llm = TRUE,              # Enable AI features
  llm_provider = "both",          # "gemini", "ollama", or "both"
  gemini_api_key = Sys.getenv("GEMINI_API_KEY")
)
```

---

## Workflow Example

### 1. Upload Data

```
Upload CSV with columns:
- study: Study identifiers
- effect: Effect sizes
- se: Standard errors
- year, quality, n (optional)
```

Or use sample data:
```r
# In dashboard: Click "Load Sample Dataset"
```

### 2. Run Analysis

Select method:
- DerSimonian-Laird (DL)
- Restricted Maximum Likelihood (REML)
- Paule-Mandel (PM)
- Sidik-Jonkman (SJ)

Click **"Run Analysis"** → Automatic computation of:
- Pooled effect
- Heterogeneity (I², τ²)
- Publication bias tests
- Sensitivity analyses
- Cumulative meta-analysis

### 3. View Results

**Quick Analysis Tab:**
- 4 value boxes (studies, effect, I², p-value)
- Forest plot
- Summary statistics
- Funnel plot
- Heterogeneity plot

**Dashboard Tab:**
- 12-panel comprehensive visualization
- All analyses in one view

### 4. Download Outputs

**Individual downloads:**
- Forest plot (300 DPI PNG)
- Funnel plot (300 DPI PNG)
- Sensitivity plots (300 DPI PNG)
- Bias plots (300 DPI PNG)

**Document downloads:**
- Methods section (TXT/DOCX)
- Results section (TXT/DOCX)
- Complete PDF report

**Complete package:**
- ZIP with all outputs

---

## Advanced Features

### 1. AI-Generated Methods Section

```r
# In dashboard:
Methods Section tab →
Select template (Structured/Narrative/PRISMA) →
Enable AI Enhancement →
Click "Generate Methods Section" →
Download TXT or DOCX
```

**Output includes:**
- Search strategy
- Inclusion criteria
- Statistical methods
- Heterogeneity assessment
- Publication bias methods
- Software citation
- Complete, publication-ready text

### 2. AI-Generated Results Section

```r
# In dashboard:
Results Section tab →
Include tables ✓
Include interpretation ✓
Click "Generate Results Section" →
Download TXT or DOCX
```

**Output includes:**
- Study characteristics table
- Pooled effect with CI
- Heterogeneity statistics
- Subgroup analyses
- Publication bias results
- Sensitivity analyses
- Complete interpretation

### 3. Multiverse Analysis

Runs 100-2000 alternative specifications:
- Different methods (DL, REML, ML, PM)
- Outlier removal permutations
- Sensitivity to assumptions

**Output:**
- Specification curve plot
- Distribution of estimates
- % significant results
- Robustness assessment

### 4. Complete Publication Package

One-click generation of:
- Methods section (500+ rules)
- Results section (500+ rules)
- 30+ publication-quality figures
- Executive summary
- All exports in multiple formats

---

## Dashboard Customization

### Themes

bs4Dash supports multiple themes:
```r
# Light theme (default)
skin = "light"

# Dark theme
skin = "dark"

# Custom colors
status = "primary"  # Blue
status = "success"  # Green
status = "warning"  # Yellow
status = "danger"   # Red
status = "info"     # Light blue
```

### Layout

```r
# Card elevation (shadow depth)
elevation = 2  # Default
elevation = 4  # More shadow

# Card colors
status = "primary"
solidHeader = TRUE
```

---

## Browser Compatibility

Tested and working on:
- ✅ Chrome/Chromium (recommended)
- ✅ Firefox
- ✅ Safari
- ✅ Edge
- ✅ Opera

**Recommended:** Chrome for best performance

---

## Performance

**Optimized for:**
- Large datasets (1000+ studies)
- Real-time updates
- Fast plot generation
- Efficient downloads

**Features:**
- Waiter loading screens
- Progress indicators
- Error handling
- Responsive design

---

## Dependencies

### Required:
```r
install.packages("shiny")
install.packages("bs4Dash")
```

### Recommended:
```r
install.packages(c(
  "DT",           # Interactive tables
  "plotly",       # Interactive plots
  "shinyWidgets", # Enhanced widgets
  "shinyjs",      # JavaScript integration
  "waiter",       # Loading screens
  "officer",      # DOCX generation
  "markdown"      # Markdown rendering
))
```

---

## Comparison with Other Tools

### cbamm Dashboard vs. Alternatives

| Feature | cbamm | RevMan | CMA | metafor (no GUI) |
|---------|-------|--------|-----|------------------|
| **Interface** | bs4Dash (modern) | Desktop app | Desktop app | Command line only |
| **Download Resolution** | **300 DPI** | Low | Medium | Manual |
| **AI Features** | ✅ Gemini/Ollama | ❌ | ❌ | ❌ |
| **Methods Auto-Gen** | ✅ 500+ rules | ❌ | ❌ | ❌ |
| **Results Auto-Gen** | ✅ 500+ rules | ❌ | ❌ | ❌ |
| **Multiverse Analysis** | ✅ | ❌ | ❌ | Manual |
| **Component NMA** | ✅ | ❌ | ❌ | ❌ |
| **Penalized Meta-Reg** | ✅ | ❌ | ❌ | ❌ |
| **8 Tau² Estimators** | ✅ | Limited | Limited | ✅ |
| **Web-based** | ✅ | ❌ | ❌ | ❌ |
| **Free/Open Source** | ✅ | Limited | ❌ | ✅ |
| **Publication Package** | ✅ One-click | Manual | Manual | Manual |

---

## Version Information

- **Dashboard Version**: 2.4.0
- **Last Updated**: 2025-11-05
- **Framework**: bs4Dash (AdminLTE 3)
- **R Version Required**: ≥ 3.5.0
- **Shiny Version Required**: ≥ 1.7.0

---

## Example Session

```r
# 1. Launch dashboard
library(cbamm)
launch_meta_dashboard()

# 2. Upload data or use sample
#    (In browser: Click "Load Sample Dataset")

# 3. Run analysis
#    (In browser: Select method, click "Run Analysis")

# 4. View results in multiple tabs
#    - Quick Analysis: Summary + main plots
#    - Dashboard: 12-panel comprehensive view
#    - Forest Plots: Detailed forest plots
#    - Funnel & Bias: Publication bias analysis
#    - Sensitivity: Robustness checks

# 5. Download outputs at 300 DPI
#    - Forest plot: Download Forest Plot (300 DPI)
#    - Funnel plot: Download Funnel Plot (300 DPI)
#    - Complete: Download All Results (ZIP)

# 6. Generate publication sections
#    (Methods tab → Generate → Download DOCX)
#    (Results tab → Generate → Download DOCX)

# 7. Get complete package
#    (Package tab → Generate Complete Package)
```

---

## Troubleshooting

### Dashboard won't launch

```r
# Check if shiny is installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Check if bs4Dash is installed
if (!requireNamespace("bs4Dash", quietly = TRUE)) {
  install.packages("bs4Dash")
}

# Launch with error messages
options(shiny.error = browser)
launch_meta_dashboard()
```

### Download buttons not working

```r
# Ensure data is loaded and analysis is run
# Check browser console for JavaScript errors
# Try different browser (Chrome recommended)
```

### Low-quality downloads

This should not happen - all downloads are **300 DPI**. If you see low quality:
- Check downloaded file resolution (should be 3000+ pixels)
- Ensure Cairo graphics device is available
- On Windows: Install Rtools
- On Mac: Install XQuartz
- On Linux: Install cairo development libraries

---

## Future Enhancements (Planned)

- **Real-time collaboration**: Multiple users editing same analysis
- **Version control**: Track changes to meta-analysis
- **Templates library**: Pre-built analysis templates
- **Automated reporting**: Schedule regular updates
- **Interactive plots**: Plotly integration for all plots
- **Mobile app**: iOS/Android companion app
- **Cloud sync**: Save/load from cloud storage

---

## Support

For issues, questions, or feature requests:
- **GitHub**: https://github.com/mahmood726-cyber/LFA/issues
- **Documentation**: See package vignettes
- **Examples**: inst/examples/ directory

---

**The cbamm Shiny dashboard provides the most advanced, user-friendly interface for meta-analysis available in R, with publication-quality outputs at the highest resolution (300 DPI) suitable for submission to top-tier journals.**
