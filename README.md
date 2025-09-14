library(cbamm)

# Load example data
data(example_meta)

# Run basic meta-analysis
result <- cbamm_fast(example_meta)
print(result)

# Cumulative analysis
cum_result <- cumulative_meta_analysis(
  example_meta,
  order_by = "year"
)
print(cum_result)

# Compute transport weights for generalizability
target_population <- list(
  age_mean = 60,
  female_pct = 0.52,
  bmi_mean = 28,
  charlson = 3
)

weights <- compute_transport_weights(
  example_meta, 
  target_population
)
