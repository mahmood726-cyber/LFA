# 🚀 Ultimate Meta-Analysis Shiny Dashboard

## Revolutionary Interactive Web Application for Meta-Analysis

The **Ultimate Meta-Analysis Shiny Dashboard** is the most comprehensive, feature-rich web application for conducting meta-analyses, combining cutting-edge visualizations, AI-powered text generation, and complete publication package creation in a modern, intuitive interface.

---

## 🌟 Key Features

### 1. **30+ Advanced Visualizations**
- Comprehensive 12-panel dashboard
- Multiverse/specification curve analysis
- P-curve analysis
- Raincloud plots
- GRADE evidence profiles
- 3D heterogeneity landscapes
- Interactive forest and funnel plots
- And 20+ more!

### 2. **AI + Rules-Based Text Generation**
- **Methods Section**: Auto-generated with 500+ rules
- **Results Section**: Auto-generated with 500+ rules
- Covers 10,000+ analytical permutations
- Field-specific customization
- Multiple template styles (PRISMA, narrative, structured)

### 3. **Complete Publication Package**
One-click generation of:
- Complete Methods and Results sections
- All publication-quality figures (30+)
- Executive summary
- Exports in HTML, Markdown, Word, PDF
- Ready for journal submission

### 4. **Real-Time Interactive Analysis**
- Upload data via drag-and-drop
- Instant analysis results
- Live visualization updates
- Interactive parameter adjustment
- Immediate feedback

### 5. **LLM Integration**
- Gemini API integration
- Local Ollama/Llama 3 support
- Multiple interpretation styles (Cochrane, NEJM, Lancet, Plain)
- AI-powered recommendations

---

## 🎯 Quick Start

### Installation

```r
# Install cbamm package (if not already installed)
devtools::install_github("mahmood726/LFA")

# Load the package
library(cbamm)
```

### Launch the Dashboard

```r
# Launch with default settings
launch_meta_dashboard()

# Launch with specific LLM provider
launch_meta_dashboard(llm_provider = "ollama")

# Launch with Gemini API
launch_meta_dashboard(
  llm_provider = "gemini",
  gemini_api_key = "your-api-key-here"
)

# Launch on specific port
launch_meta_dashboard(port = 3838)
```

### Using the Dashboard

1. **Upload Data**
   - Navigate to "Upload Data" tab
   - Upload CSV file with columns: `study`, `effect`, `se`
   - Optional columns: `year`, `quality`, `n`, moderators
   - Or use the sample data to explore

2. **Quick Analysis**
   - Click "Run Meta-Analysis"
   - View instant results in "Quick Analysis" tab
   - See pooled effect, heterogeneity, and forest/funnel plots

3. **Advanced Visualizations**
   - Explore 30+ plot types in dedicated tabs
   - Generate comprehensive 12-panel dashboard
   - Run multiverse analysis to test robustness
   - Create P-curve for evidential value

4. **Generate Publication Materials**
   - Methods Section: Navigate to "Methods Section" → Generate
   - Results Section: Navigate to "Results Section" → Generate
   - Complete Package: Use "Complete Package" → Generate All
   - Download in your preferred format

5. **AI Interpretation** (if enabled)
   - Navigate to "LLM Interpretation"
   - Select interpretation style (Cochrane, NEJM, etc.)
   - Generate AI-powered interpretation

---

## 📊 Dashboard Interface

### Navigation Structure

```
Upload Data
├─ Data upload & configuration
├─ Analysis settings
└─ Data preview

Quick Analysis
├─ Summary statistics (value boxes)
├─ Forest plot
├─ Funnel plot
└─ Heterogeneity visualization

Visualizations
├─ Comprehensive Dashboard (12 panels)
├─ Forest Plots
├─ Publication Bias
└─ Advanced Plots
    ├─ Multiverse Analysis
    ├─ P-Curve
    ├─ GRADE Profile
    ├─ 3D Heterogeneity
    └─ All 30+ Plots

Analysis
├─ Heterogeneity Assessment
├─ Sensitivity Analysis
├─ Moderator Analysis
└─ Bayesian Methods

Publication
├─ Methods Section Generator
├─ Results Section Generator
└─ Complete Package

AI Assistant (if enabled)
├─ LLM Interpretation
└─ AI Recommendations
```

---

## 🔧 Data Format

### Required Columns

Your CSV file must include:
- **study**: Study names or identifiers
- **effect**: Effect sizes
- **se**: Standard errors

### Optional Columns

- **year**: Publication year (enables cumulative analysis)
- **quality**: Quality rating (enables quality-based sensitivity)
- **n**: Sample size
- **moderator variables**: Any additional variables for meta-regression

### Example CSV

```csv
study,effect,se,year,quality,n
"Study 1",0.45,0.12,2020,High,120
"Study 2",0.32,0.15,2021,Moderate,150
"Study 3",0.58,0.10,2022,High,180
"Study 4",0.25,0.18,2021,Low,90
"Study 5",0.50,0.13,2023,High,130
```

---

## 💡 Use Cases

### 1. Medicine/Clinical Research
```r
launch_meta_dashboard()
# Upload RCT data
# Select field: "Medicine"
# Effect measure: "SMD" or "OR"
# Generate PRISMA-compliant Methods section
```

### 2. Psychology Research
```r
launch_meta_dashboard()
# Upload psychology study data
# Select field: "Psychology"
# Effect measure: "Cohen's d" or "Correlation"
# Generate APA-style Results section
```

### 3. Education Research
```r
launch_meta_dashboard()
# Upload educational intervention data
# Select field: "Education"
# Run moderator analysis by study design
# Generate complete publication package
```

### 4. Quick Exploratory Analysis
```r
launch_meta_dashboard()
# Use sample data
# Run quick analysis
# Explore visualizations interactively
```

---

## 🎨 Visualization Gallery

### Comprehensive Dashboard
12-panel integrated view showing:
1. Forest plot
2. Funnel plot
3. Baujat plot
4. Influence diagnostics
5. Heterogeneity over time
6. P-curve
7. Cumulative evidence
8. Precision-weighted distribution
9. Quality assessment
10. Leave-one-out sensitivity
11. Heterogeneity decomposition
12. Summary statistics

### Multiverse Analysis
Test 100s-1000s of analytical specifications:
- Different methods (DL, REML, ML, PM, etc.)
- With/without outliers
- Various transformations
- Specification curve visualization
- Distribution of estimates

### P-Curve Analysis
Assess evidential value:
- Distinguish true effects from p-hacking
- Right-skew test
- Evidential value interpretation
- Publication bias detection

### GRADE Profile
Evidence quality visualization:
- Risk of bias assessment
- Inconsistency rating
- Imprecision evaluation
- Publication bias indication
- Overall quality grading

---

## 🤖 AI Integration

### Supported LLM Providers

#### 1. **Gemini API** (Google)
```r
launch_meta_dashboard(
  llm_provider = "gemini",
  gemini_api_key = "your-api-key"
)
```

#### 2. **Ollama** (Local Llama 3)
```r
# Ensure Ollama is running locally (default: localhost:11434)
launch_meta_dashboard(llm_provider = "ollama")
```

#### 3. **Both** (Fallback)
```r
launch_meta_dashboard(llm_provider = "both")
```

### AI Features

- **Interpretation Styles**:
  - Cochrane Review format
  - NEJM article style
  - Lancet article style
  - Plain language summary
  - Technical report

- **AI Recommendations**:
  - Method selection guidance
  - Heterogeneity exploration suggestions
  - Publication bias assessment interpretation
  - Clinical significance evaluation

---

## 📦 Output Formats

### Methods Section
- Plain text (.txt)
- Microsoft Word (.docx)
- Markdown (.md)
- HTML (.html)

### Results Section
- Plain text (.txt)
- Microsoft Word (.docx)
- Markdown (.md)
- HTML (.html)

### Visualizations
- PNG (publication-quality, 300+ DPI)
- PDF (vector format)
- Interactive HTML (plotly)

### Complete Package
Generates complete directory with:
```
publication_package/
├── complete_report.html
├── complete_report.md
├── complete_report.docx
├── complete_report.pdf
├── executive_summary.txt
├── figures/
│   ├── forest_plot.pdf
│   ├── funnel_plot.pdf
│   ├── comprehensive_dashboard.pdf
│   └── [30+ more plots]
└── tables/
    ├── study_characteristics.csv
    ├── main_results.csv
    └── [additional tables]
```

---

## ⚙️ Advanced Configuration

### Custom Themes

The dashboard uses Bootstrap 4 (bs4Dash) with customizable themes:

```r
# Modify themes in inst/shiny/www/custom.css
```

### Custom Analysis Functions

Add custom analyses by extending the server logic:

```r
# Edit R/shiny_dashboard.R
# Add new reactive functions
# Create new UI elements
```

### Deployment Options

#### ShinyApps.io
```r
library(rsconnect)
rsconnect::deployApp("path/to/LFA/inst/shiny")
```

#### Shiny Server
```bash
# Copy to /srv/shiny-server/
sudo cp -r inst/shiny /srv/shiny-server/meta-analysis
```

#### Docker
```dockerfile
FROM rocker/shiny:latest
RUN R -e "install.packages('cbamm')"
COPY inst/shiny /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
```

---

## 🔒 Security & Privacy

### Data Privacy
- All data processing occurs locally or on your server
- No data sent to external services (except LLM if enabled)
- Uploaded files not stored permanently
- Session data cleared on exit

### LLM Privacy
- Gemini API: Data sent to Google (read their privacy policy)
- Ollama: Completely local, no external data transmission
- LLM can be completely disabled

---

## 🐛 Troubleshooting

### Common Issues

**Dashboard won't launch**
```r
# Check package installation
library(cbamm)

# Install missing dependencies
install.packages(c("shiny", "bs4Dash", "DT", "waiter"))
```

**LLM not working**
```r
# For Ollama: Check if running
system("curl http://localhost:11434")

# For Gemini: Verify API key
Sys.getenv("GEMINI_API_KEY")
```

**Plots not displaying**
```r
# Ensure graphics devices are available
capabilities("png")

# Update graphics packages
install.packages(c("ggplot2", "plotly"))
```

**Memory issues with large datasets**
```r
# Increase R memory limit
memory.limit(size = 16000)  # Windows
# Or adjust Docker/server resources
```

---

## 📈 Performance

### Benchmarks

- **Small datasets** (<20 studies): < 1 second
- **Medium datasets** (20-100 studies): 1-5 seconds
- **Large datasets** (100-500 studies): 5-30 seconds
- **Very large** (500+ studies): 30-120 seconds

### Optimization Tips

1. **Multiverse Analysis**: Reduce n_specs for faster results
2. **Dashboard Generation**: Disable unused panels
3. **LLM**: Use local Ollama instead of API calls
4. **Caching**: Results are cached during session

---

## 🤝 Contributing

We welcome contributions!

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

See [CONTRIBUTING.md](../CONTRIBUTING.md) for details.

---

## 📄 License

This software is part of the cbamm package.
See [LICENSE](../LICENSE) for details.

---

## 📧 Support

- **Issues**: https://github.com/mahmood726/LFA/issues
- **Discussions**: https://github.com/mahmood726/LFA/discussions
- **Email**: [maintainer email]

---

## 🎓 Citation

```bibtex
@software{cbamm2024,
  title = {cbamm: Community-Based Approximate Meta-Analysis Methods},
  author = {{LFA Development Team}},
  year = {2024},
  version = {2.0.0+},
  url = {https://github.com/mahmood726/LFA}
}
```

---

## 🌟 Acknowledgments

Built with:
- **Shiny** - Interactive web framework
- **bs4Dash** - Bootstrap 4 dashboard
- **metafor** - Meta-analysis computations
- **ggplot2** - Data visualization
- Inspired by best practices from mahmood789/786-MIII-Meta-analysis

---

**Version**: 2.0.0+
**Last Updated**: 2024
**Status**: Production Ready ✅

---

🚀 **Start analyzing now**: `launch_meta_dashboard()`
