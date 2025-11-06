# Script to create example datasets for the cbamm package

# Set seed for reproducibility
set.seed(42)

# Example 1: Simple meta-analysis dataset
example_meta <- data.frame(
  study = c(
    "Smith et al. (2015)",
    "Johnson et al. (2016)",
    "Williams et al. (2016)",
    "Brown et al. (2017)",
    "Jones et al. (2018)",
    "Garcia et al. (2018)",
    "Miller et al. (2019)",
    "Davis et al. (2020)",
    "Rodriguez et al. (2020)",
    "Martinez et al. (2021)"
  ),
  effect = c(0.45, 0.62, 0.38, 0.71, 0.52, 0.48, 0.67, 0.55, 0.43, 0.59),
  se = c(0.12, 0.18, 0.10, 0.22, 0.14, 0.11, 0.19, 0.15, 0.13, 0.16),
  year = 2015:2024,
  sample_size = c(120, 85, 200, 65, 150, 180, 90, 140, 160, 110),
  quality = c(4, 3, 5, 3, 4, 5, 3, 4, 4, 5),
  region = c("North America", "Europe", "North America", "Asia", "Europe",
             "North America", "Europe", "Asia", "North America", "Europe"),
  stringsAsFactors = FALSE
)

# Example 2: Meta-analysis with covariates for transport weights
example_transport <- data.frame(
  study = paste0("Study", 1:8),
  effect = c(0.5, 0.6, 0.4, 0.7, 0.5, 0.55, 0.45, 0.65),
  se = c(0.1, 0.15, 0.12, 0.18, 0.11, 0.14, 0.10, 0.16),
  age_mean = c(55, 60, 65, 58, 62, 57, 63, 59),
  female_pct = c(0.45, 0.50, 0.55, 0.48, 0.52, 0.46, 0.54, 0.49),
  bmi_mean = c(26, 28, 30, 27, 29, 26.5, 28.5, 27.5),
  diabetes_pct = c(0.15, 0.20, 0.25, 0.18, 0.22, 0.16, 0.24, 0.19),
  stringsAsFactors = FALSE
)

# Example 3: Small diagnostic test accuracy dataset
example_dta <- data.frame(
  study = c(
    "Anderson 2018",
    "Baker 2019",
    "Clark 2019",
    "Davis 2020",
    "Evans 2021",
    "Foster 2021"
  ),
  tp = c(85, 92, 78, 95, 88, 90),  # True positives
  fp = c(12, 8, 15, 10, 11, 9),     # False positives
  fn = c(15, 8, 22, 5, 12, 10),     # False negatives
  tn = c(188, 192, 185, 190, 189, 191), # True negatives
  sensitivity = c(0.850, 0.920, 0.780, 0.950, 0.880, 0.900),
  specificity = c(0.940, 0.960, 0.925, 0.950, 0.945, 0.955),
  setting = c("Hospital", "Clinic", "Hospital", "Clinic", "Hospital", "Clinic"),
  threshold = c("Standard", "Standard", "High", "Standard", "Standard", "Standard"),
  stringsAsFactors = FALSE
)

# Calculate diagnostic odds ratio and standard error for each study
example_dta$dor <- (example_dta$tp * example_dta$tn) / (example_dta$fp * example_dta$fn)
example_dta$log_dor <- log(example_dta$dor)

# Approximate SE for log(DOR) using the delta method
example_dta$se_log_dor <- sqrt(1/example_dta$tp + 1/example_dta$fp +
                                 1/example_dta$fn + 1/example_dta$tn)

# Save datasets
usethis::use_data(example_meta, overwrite = TRUE)
usethis::use_data(example_transport, overwrite = TRUE)
usethis::use_data(example_dta, overwrite = TRUE)
