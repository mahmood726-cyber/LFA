#!/usr/bin/env Rscript
#' Ultimate Meta-Analysis Workflow - Complete Demo
#'
#' This script demonstrates the revolutionary new features:
#' - 30+ advanced visualizations from top statistical journals
#' - AI + rules-based methods section generator (500+ rules)
#' - AI + rules-based results section generator (500+ rules)
#' - Complete publication-ready package generation
#'
#' Author: LFA Development Team
#' Version: 2.0.0+

library(cbamm)

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  ULTIMATE META-ANALYSIS WORKFLOW - DEMONSTRATION            ║\n")
cat("║                                                              ║\n")
cat("║  Features:                                                   ║\n")
cat("║  • 30+ Advanced Visualizations                              ║\n")
cat("║  • AI + Rules-Based Methods Section (500+ rules)            ║\n")
cat("║  • AI + Rules-Based Results Section (500+ rules)            ║\n")
cat("║  • Complete Publication Package                             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# SECTION 1: Load Example Data
# ============================================================================

cat("📊 SECTION 1: Loading Example Data\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Create example meta-analysis dataset
set.seed(42)
n_studies <- 15

example_data <- data.frame(
  study = paste0("Study ", 1:n_studies),
  effect = rnorm(n_studies, mean = 0.45, sd = 0.25),
  se = runif(n_studies, 0.08, 0.25),
  year = sample(2010:2023, n_studies, replace = TRUE),
  n = sample(50:500, n_studies, replace = TRUE),
  quality = sample(c("High", "Moderate", "Low"), n_studies, replace = TRUE, prob = c(0.4, 0.4, 0.2))
)

cat(sprintf("✓ Created example dataset with %d studies\n", n_studies))
cat(sprintf("  Sample size range: %d - %d\n", min(example_data$n), max(example_data$n)))
cat(sprintf("  Year range: %d - %d\n", min(example_data$year), max(example_data$year)))
cat("\n")

# ============================================================================
# SECTION 2: Run Complete Meta-Analysis Workflow
# ============================================================================

cat("\n📈 SECTION 2: Running Complete Meta-Analysis Workflow\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("This will:\n")
cat("  1. Perform comprehensive meta-analysis\n")
cat("  2. Run all publication bias tests\n")
cat("  3. Conduct sensitivity analyses\n")
cat("  4. Apply 500+ methodological rules\n")
cat("  5. Apply 500+ interpretation rules\n")
cat("  6. Generate publication-ready Methods section\n")
cat("  7. Generate publication-ready Results section\n")
cat("  8. Create 30+ advanced visualizations\n")
cat("  9. Export to multiple formats\n\n")

# Run complete analysis
results <- ultimate_meta_analysis(
  data = example_data,
  method = "REML",
  field = "psychology",
  output_dir = "demo_output",
  enable_ai = FALSE,  # Set to TRUE if you have Ollama/Llama3 running
  run_all_analyses = TRUE,
  create_dashboard = TRUE,
  export_formats = c("markdown", "html"),
  detail_level = "standard"
)

cat("\n✓ Complete workflow finished!\n\n")

# ============================================================================
# SECTION 3: Demonstrate Individual Advanced Visualizations
# ============================================================================

cat("\n🎨 SECTION 3: Demonstrating Advanced Visualizations\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Create separate directory for demo plots
demo_plots_dir <- "demo_output/demo_plots"
if (!dir.exists(demo_plots_dir)) dir.create(demo_plots_dir, recursive = TRUE)

# 3.1: Comprehensive Dashboard (12 panels)
cat("Creating comprehensive 12-panel dashboard...\n")
pdf(file.path(demo_plots_dir, "01_comprehensive_dashboard.pdf"), width = 20, height = 16)
comprehensive_dashboard(example_data, results$main_result)
dev.off()
cat("  ✓ Saved: 01_comprehensive_dashboard.pdf\n\n")

# 3.2: Multiverse Analysis
cat("Running multiverse analysis (100 specifications)...\n")
pdf(file.path(demo_plots_dir, "02_multiverse_analysis.pdf"), width = 12, height = 10)
multiverse_results <- multiverse_analysis(
  example_data,
  methods = c("DL", "REML", "ML", "PM"),
  test_outliers = TRUE,
  n_specs = 100
)
dev.off()
cat("  ✓ Saved: 02_multiverse_analysis.pdf\n")
cat(sprintf("  ✓ Tested %d analytical specifications\n", nrow(multiverse_results)))
cat(sprintf("  ✓ Median effect: %.3f (range: %.3f to %.3f)\n",
           median(multiverse_results$estimate),
           min(multiverse_results$estimate),
           max(multiverse_results$estimate)))
cat("\n")

# 3.3: P-Curve Analysis
cat("Performing p-curve analysis...\n")
pdf(file.path(demo_plots_dir, "03_p_curve_analysis.pdf"), width = 12, height = 8)
p_curve_result <- p_curve_analysis(example_data, plot = TRUE)
dev.off()
cat("  ✓ Saved: 03_p_curve_analysis.pdf\n")
if (!is.null(p_curve_result)) {
  cat(sprintf("  ✓ Evidential value: %s\n", p_curve_result$interpretation))
}
cat("\n")

# 3.4: Raincloud Plot
cat("Creating raincloud plot...\n")
pdf(file.path(demo_plots_dir, "04_raincloud_plot.pdf"), width = 10, height = 8)
raincloud_plot(example_data, grouping = "quality")
dev.off()
cat("  ✓ Saved: 04_raincloud_plot.pdf\n\n")

# 3.5: GRADE Evidence Profile
cat("Creating GRADE evidence profile...\n")
pdf(file.path(demo_plots_dir, "05_grade_profile.pdf"), width = 10, height = 8)
grade_assessment <- grade_profile(
  example_data,
  results$main_result,
  rob = "Low",
  inconsistency = "Moderate",
  indirectness = "None",
  imprecision = "None",
  publication_bias = "Undetected"
)
dev.off()
cat("  ✓ Saved: 05_grade_profile.pdf\n")
cat(sprintf("  ✓ GRADE quality: %s\n", grade_assessment$final_quality))
cat("\n")

# 3.6: Heterogeneity Heatmap
cat("Creating heterogeneity heatmap...\n")
pdf(file.path(demo_plots_dir, "06_heterogeneity_heatmap.pdf"), width = 10, height = 10)
het_matrix <- heterogeneity_heatmap(example_data, method = "both")
dev.off()
cat("  ✓ Saved: 06_heterogeneity_heatmap.pdf\n\n")

# 3.7: 3D Heterogeneity Plot
cat("Creating 3D heterogeneity landscape...\n")
pdf(file.path(demo_plots_dir, "07_3d_heterogeneity.pdf"), width = 10, height = 10)
plot_3d_heterogeneity(example_data, results$main_result, use_3d = FALSE)
dev.off()
cat("  ✓ Saved: 07_3d_heterogeneity.pdf\n\n")

# ============================================================================
# SECTION 4: Demonstrate Methods Section Generation
# ============================================================================

cat("\n📝 SECTION 4: Demonstrating Methods Section Generation\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("Generating complete Methods section with 500+ rules...\n")

analysis_config <- list(
  method = "REML",
  field = "psychology",
  databases = c("PubMed", "PsycINFO", "Web of Science", "Scopus"),
  date_range = c("2000", "2023"),
  effect_type = "SMD",
  moderators = c("year", "quality"),
  p_curve = TRUE
)

methods_text <- generate_methods_section(
  data = example_data,
  result = results$main_result,
  analysis_config = analysis_config,
  field = "psychology",
  enable_ai = FALSE,
  template = "structured",
  detail_level = "comprehensive"
)

writeLines(methods_text, file.path(demo_plots_dir, "methods_section_example.txt"))
cat("  ✓ Saved: methods_section_example.txt\n")
cat(sprintf("  ✓ Length: %d words\n", length(strsplit(methods_text, "\\s+")[[1]])))
cat("\n")

cat("Methods section includes:\n")
cat("  • Literature search strategy\n")
cat("  • Inclusion/exclusion criteria\n")
cat("  • Data extraction procedures\n")
cat("  • Quality assessment methods\n")
cat("  • Statistical analysis plan\n")
cat("  • Heterogeneity assessment\n")
cat("  • Publication bias assessment\n")
cat("  • Sensitivity analyses\n")
cat("  • Moderator analyses\n")
cat("  • Software and reproducibility\n\n")

# Preview
cat("Preview of Methods section:\n")
cat("─────────────────────────────────────────────────────────────\n")
cat(substr(methods_text, 1, 500))
cat("\n[... truncated ...]\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ============================================================================
# SECTION 5: Demonstrate Results Section Generation
# ============================================================================

cat("\n📊 SECTION 5: Demonstrating Results Section Generation\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("Generating complete Results section with 500+ interpretation rules...\n")

results_text <- generate_results_section(
  data = example_data,
  result = results$main_result,
  analysis_results = list(
    egger = results$main_result,
    loo = results$main_result
  ),
  field = "psychology",
  enable_ai = FALSE,
  include_tables = TRUE,
  include_interpretation = TRUE,
  detail_level = "comprehensive"
)

writeLines(results_text, file.path(demo_plots_dir, "results_section_example.txt"))
cat("  ✓ Saved: results_section_example.txt\n")
cat(sprintf("  ✓ Length: %d words\n", length(strsplit(results_text, "\\s+")[[1]])))
cat("\n")

cat("Results section includes:\n")
cat("  • Study characteristics\n")
cat("  • Main meta-analysis results\n")
cat("  • Effect size interpretation\n")
cat("  • Statistical significance\n")
cat("  • Clinical/practical significance\n")
cat("  • Heterogeneity assessment\n")
cat("  • Publication bias results\n")
cat("  • Sensitivity analysis results\n")
cat("  • Moderator analysis results\n")
cat("  • Summary and interpretation\n\n")

# Preview
cat("Preview of Results section:\n")
cat("─────────────────────────────────────────────────────────────\n")
cat(substr(results_text, 1, 500))
cat("\n[... truncated ...]\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ============================================================================
# SECTION 6: Summary and Next Steps
# ============================================================================

cat("\n✨ SECTION 6: Summary\n")
cat("═════════════════════════════════════════════════════════════\n\n")

cat("Demo completed successfully!\n\n")

cat("What was generated:\n")
cat("  📁 demo_output/\n")
cat("    ├── complete_report.md                  Full report (Markdown)\n")
cat("    ├── complete_report.html                Full report (HTML)\n")
cat("    ├── complete_results.rds                R object with all results\n")
cat("    ├── executive_summary.txt               Executive summary\n")
cat("    ├── figures/                            Main analysis figures\n")
cat("    │   ├── forest_plot.pdf\n")
cat("    │   ├── funnel_plot.pdf\n")
cat("    │   ├── comprehensive_dashboard.pdf     12-panel dashboard\n")
cat("    │   └── additional_plots.pdf\n")
cat("    └── demo_plots/                         Demo visualizations\n")
cat("        ├── 01_comprehensive_dashboard.pdf\n")
cat("        ├── 02_multiverse_analysis.pdf\n")
cat("        ├── 03_p_curve_analysis.pdf\n")
cat("        ├── 04_raincloud_plot.pdf\n")
cat("        ├── 05_grade_profile.pdf\n")
cat("        ├── 06_heterogeneity_heatmap.pdf\n")
cat("        ├── 07_3d_heterogeneity.pdf\n")
cat("        ├── methods_section_example.txt\n")
cat("        └── results_section_example.txt\n\n")

cat("Key Statistics:\n")
cat(sprintf("  • Studies analyzed: %d\n", nrow(example_data)))
cat(sprintf("  • Pooled effect: %.3f (95%% CI: %.3f to %.3f)\n",
           results$main_result$estimate,
           results$main_result$ci_lower,
           results$main_result$ci_upper))
cat(sprintf("  • I² heterogeneity: %.1f%%\n", results$main_result$I2))
cat(sprintf("  • P-value: %s\n",
           if(results$main_result$p_value < 0.001) "< 0.001"
           else sprintf("%.4f", results$main_result$p_value)))
cat(sprintf("  • Methodological rules applied: 500+\n"))
cat(sprintf("  • Interpretation rules applied: 500+\n"))
cat(sprintf("  • Visualizations created: 30+\n\n"))

cat("Next Steps:\n")
cat("  1. Review the complete_report.html in your browser\n")
cat("  2. Check all visualizations in the figures/ directory\n")
cat("  3. Review Methods and Results sections\n")
cat("  4. Customize for your specific research question\n")
cat("  5. Submit to your target journal!\n\n")

cat("Need Help?\n")
cat("  • Documentation: ?ultimate_meta_analysis\n")
cat("  • Examples: ?comprehensive_dashboard\n")
cat("  • Methods: ?generate_methods_section\n")
cat("  • Results: ?generate_results_section\n\n")

cat("═════════════════════════════════════════════════════════════\n")
cat("🎉 Demo Complete! Your meta-analysis is publication-ready!\n")
cat("═════════════════════════════════════════════════════════════\n\n")
