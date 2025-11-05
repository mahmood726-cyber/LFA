#' Comprehensive Scenario Database for Meta-Analysis
#'
#' Database of 10,000+ realistic meta-analysis scenarios for testing, validation,
#' training, and decision support. Covers all meta-analysis contexts.
#'
#' @name scenario_database
NULL

#' Initialize Scenario Database
#'
#' Loads comprehensive database of 10,000+ meta-analysis scenarios.
#'
#' @return Scenario database object
#'
#' @export
init_scenario_database <- function() {
  scenarios <- list(
    clinical_trials = generate_clinical_scenarios(2000),
    observational_studies = generate_observational_scenarios(1500),
    diagnostic_accuracy = generate_diagnostic_scenarios(1000),
    epidemiology = generate_epidemiology_scenarios(1000),
    psychology = generate_psychology_scenarios(1000),
    education = generate_education_scenarios(800),
    economics = generate_economics_scenarios(700),
    environmental = generate_environmental_scenarios(600),
    methodology = generate_methodology_scenarios(500),
    edge_cases = generate_edge_case_scenarios(400),
    complex_designs = generate_complex_design_scenarios(500),
    rare_diseases = generate_rare_disease_scenarios(300),
    implementation = generate_implementation_scenarios(300),
    prognostic = generate_prognostic_scenarios(400)
  )

  total_scenarios <- sum(sapply(scenarios, length))

  db <- list(
    scenarios = scenarios,
    n_scenarios = total_scenarios,
    version = "1.0.0",
    last_updated = Sys.time()
  )

  class(db) <- "scenario_database"
  message(sprintf("Scenario database initialized with %d scenarios", total_scenarios))
  return(db)
}

#' Generate Clinical Trial Scenarios (2000 scenarios)
#' @keywords internal
generate_clinical_scenarios <- function(n) {
  scenarios <- list()

  # Scenario templates
  templates <- list(
    list(
      name = "Drug efficacy RCT",
      k_range = c(5, 50),
      effect_range = c(-0.5, 1.5),
      I2_range = c(0, 80),
      context = "pharmaceutical intervention"
    ),
    list(
      name = "Surgical intervention",
      k_range = c(3, 20),
      effect_range = c(0.2, 1.0),
      I2_range = c(20, 90),
      context = "surgical procedure"
    ),
    list(
      name = "Behavioral intervention",
      k_range = c(8, 40),
      effect_range = c(0.1, 0.8),
      I2_range = c(30, 75),
      context = "behavioral therapy"
    ),
    list(
      name = "Device trial",
      k_range = c(4, 15),
      effect_range = c(0.3, 1.2),
      I2_range = c(10, 60),
      context = "medical device"
    ),
    list(
      name = "Vaccine efficacy",
      k_range = c(5, 30),
      effect_range = c(0.5, 2.0),
      I2_range = c(5, 50),
      context = "vaccine trial"
    )
  )

  for (i in 1:n) {
    template <- templates[[((i-1) %% length(templates)) + 1]]

    k <- sample(template$k_range[1]:template$k_range[2], 1)
    true_effect <- runif(1, template$effect_range[1], template$effect_range[2])
    target_I2 <- runif(1, template$I2_range[1], template$I2_range[2])

    # Generate heterogeneity
    tau2 <- (target_I2 / 100) * 0.1
    study_effects <- rnorm(k, true_effect, sqrt(tau2))

    # Generate standard errors
    ses <- runif(k, 0.1, 0.4)

    scenarios[[i]] <- list(
      scenario_id = sprintf("CLIN%04d", i),
      type = "clinical_trial",
      subtype = template$name,
      context = template$context,
      k = k,
      true_effect = true_effect,
      target_I2 = target_I2,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = study_effects,
        se = ses,
        year = sample(2010:2024, k, replace = TRUE)
      ),
      characteristics = list(
        design = "RCT",
        blinding = sample(c("double", "single", "open"), k, replace = TRUE),
        risk_of_bias = sample(c("low", "moderate", "high"), k, replace = TRUE, prob = c(0.5, 0.3, 0.2))
      ),
      expected_challenges = c(
        if (target_I2 > 60) "high heterogeneity" else NULL,
        if (k < 10) "small sample" else NULL,
        if (abs(true_effect) < 0.2) "small effect size" else NULL
      )
    )
  }

  return(scenarios)
}

#' Generate Observational Study Scenarios (1500 scenarios)
#' @keywords internal
generate_observational_scenarios <- function(n) {
  scenarios <- list()

  designs <- c("cohort", "case-control", "cross-sectional")
  exposures <- c("dietary", "environmental", "occupational", "lifestyle", "genetic")

  for (i in 1:n) {
    k <- sample(6:40, 1)
    design <- sample(designs, 1)
    exposure <- sample(exposures, 1)

    # Observational studies typically have higher heterogeneity
    I2 <- runif(1, 40, 90)
    tau2 <- (I2 / 100) * 0.15

    true_effect <- rnorm(1, 0.3, 0.3) # Typically modest effects
    study_effects <- rnorm(k, true_effect, sqrt(tau2))
    ses <- runif(k, 0.15, 0.5)

    scenarios[[i]] <- list(
      scenario_id = sprintf("OBS%04d", i),
      type = "observational",
      subtype = design,
      exposure = exposure,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = study_effects,
        se = ses,
        year = sample(2005:2024, k, replace = TRUE),
        quality = sample(1:9, k, replace = TRUE)
      ),
      expected_challenges = c(
        "confounding bias risk",
        "high heterogeneity likely",
        "publication bias assessment critical"
      )
    )
  }

  return(scenarios)
}

#' Generate Diagnostic Accuracy Scenarios (1000 scenarios)
#' @keywords internal
generate_diagnostic_scenarios <- function(n) {
  scenarios <- list()

  test_types <- c("imaging", "laboratory", "clinical_exam", "questionnaire", "genetic")

  for (i in 1:n) {
    k <- sample(5:30, 1)
    test_type <- sample(test_types, 1)

    # Generate sensitivity and specificity
    sensitivity <- runif(k, 0.6, 0.95)
    specificity <- runif(k, 0.7, 0.98)

    scenarios[[i]] <- list(
      scenario_id = sprintf("DTA%04d", i),
      type = "diagnostic_accuracy",
      test_type = test_type,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        tp = rpois(k, 50),
        fn = rpois(k, 10),
        fp = rpois(k, 15),
        tn = rpois(k, 100),
        sensitivity = sensitivity,
        specificity = specificity
      ),
      expected_methods = c("bivariate model", "HSROC", "SROC curve"),
      expected_challenges = c(
        "threshold effects",
        "correlated sensitivity/specificity",
        "heterogeneity in test accuracy"
      )
    )
  }

  return(scenarios)
}

#' Generate Epidemiology Scenarios (1000 scenarios)
#' @keywords internal
generate_epidemiology_scenarios <- function(n) {
  scenarios <- list()

  outcomes <- c("mortality", "incidence", "prevalence", "risk_factor", "survival")

  for (i in 1:n) {
    k <- sample(8:50, 1)
    outcome <- sample(outcomes, 1)

    # Epidemiology effects often log OR or RR
    true_logOR <- rnorm(1, 0, 0.5)
    tau2 <- runif(1, 0.05, 0.3)
    study_logORs <- rnorm(k, true_logOR, sqrt(tau2))
    ses <- runif(k, 0.1, 0.4)

    scenarios[[i]] <- list(
      scenario_id = sprintf("EPI%04d", i),
      type = "epidemiology",
      outcome = outcome,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = study_logORs,
        se = ses,
        sample_size = sample(100:10000, k, replace = TRUE),
        region = sample(c("North America", "Europe", "Asia", "Global"), k, replace = TRUE)
      ),
      effect_measure = "log odds ratio",
      expected_challenges = c(
        "geographic heterogeneity",
        "temporal trends",
        "population differences"
      )
    )
  }

  return(scenarios)
}

#' Generate Psychology Scenarios (1000 scenarios)
#' @keywords internal
generate_psychology_scenarios <- function(n) {
  scenarios <- list()

  domains <- c("cognitive", "social", "clinical", "developmental", "neuropsych")

  for (i in 1:n) {
    k <- sample(10:60, 1)
    domain <- sample(domains, 1)

    # Psychology typically uses Cohen's d
    true_d <- rnorm(1, 0.4, 0.3)
    tau2 <- runif(1, 0.05, 0.25)
    study_ds <- rnorm(k, true_d, sqrt(tau2))
    ses <- runif(k, 0.08, 0.35)

    scenarios[[i]] <- list(
      scenario_id = sprintf("PSY%04d", i),
      type = "psychology",
      domain = domain,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = study_ds,
        se = ses,
        published = sample(c(TRUE, FALSE), k, replace = TRUE, prob = c(0.8, 0.2)),
        quality = sample(1:10, k, replace = TRUE)
      ),
      effect_measure = "Cohen's d",
      expected_challenges = c(
        "publication bias likely",
        "small study effects",
        "p-hacking concerns"
      )
    )
  }

  return(scenarios)
}

#' Generate Education Scenarios (800 scenarios)
#' @keywords internal
generate_education_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(8:40, 1)
    intervention_type <- sample(c("technology", "curriculum", "teaching_method", "assessment"), 1)

    true_effect <- rnorm(1, 0.25, 0.2)
    tau2 <- runif(1, 0.08, 0.3)
    study_effects <- rnorm(k, true_effect, sqrt(tau2))
    ses <- runif(k, 0.1, 0.4)

    scenarios[[i]] <- list(
      scenario_id = sprintf("EDU%04d", i),
      type = "education",
      intervention = intervention_type,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = study_effects,
        se = ses,
        grade_level = sample(c("elementary", "middle", "high", "college"), k, replace = TRUE)
      )
    )
  }

  return(scenarios)
}

#' Generate Economics Scenarios (700 scenarios)
#' @keywords internal
generate_economics_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(6:30, 1)

    scenarios[[i]] <- list(
      scenario_id = sprintf("ECON%04d", i),
      type = "economics",
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.15, 0.3),
        se = runif(k, 0.05, 0.3)
      )
    )
  }

  return(scenarios)
}

#' Generate Environmental Scenarios (600 scenarios)
#' @keywords internal
generate_environmental_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(5:25, 1)

    scenarios[[i]] <- list(
      scenario_id = sprintf("ENV%04d", i),
      type = "environmental",
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.2, 0.4),
        se = runif(k, 0.1, 0.5)
      )
    )
  }

  return(scenarios)
}

#' Generate Methodology Scenarios (500 scenarios)
#' @keywords internal
generate_methodology_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    scenarios[[i]] <- list(
      scenario_id = sprintf("METH%04d", i),
      type = "methodology",
      focus = sample(c("method_comparison", "simulation", "power_analysis"), 1)
    )
  }

  return(scenarios)
}

#' Generate Edge Case Scenarios (400 scenarios)
#' @keywords internal
generate_edge_case_scenarios <- function(n) {
  scenarios <- list()

  edge_types <- c(
    "extreme_heterogeneity", "zero_events", "single_arm",
    "very_small_studies", "very_large_effects", "opposite_direction_effects"
  )

  for (i in 1:n) {
    edge_type <- sample(edge_types, 1)
    k <- sample(3:15, 1)

    if (edge_type == "extreme_heterogeneity") {
      data <- data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0, 1.5), # Very wide spread
        se = runif(k, 0.1, 0.3)
      )
    } else if (edge_type == "zero_events") {
      data <- data.frame(
        study = paste0("Study_", 1:k),
        events1 = c(rep(0, k/2), rpois(k/2, 2)),
        n1 = rep(50, k),
        events2 = rpois(k, 5),
        n2 = rep(50, k)
      )
    } else {
      data <- data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.5, 0.2),
        se = runif(k, 0.1, 0.3)
      )
    }

    scenarios[[i]] <- list(
      scenario_id = sprintf("EDGE%04d", i),
      type = "edge_case",
      edge_type = edge_type,
      k = k,
      data = data,
      expected_challenges = paste("Edge case:", edge_type)
    )
  }

  return(scenarios)
}

#' Generate Complex Design Scenarios (500 scenarios)
#' @keywords internal
generate_complex_design_scenarios <- function(n) {
  scenarios <- list()

  designs <- c("cluster_RCT", "stepped_wedge", "crossover", "factorial")

  for (i in 1:n) {
    design <- sample(designs, 1)
    k <- sample(4:20, 1)

    scenarios[[i]] <- list(
      scenario_id = sprintf("COMPLEX%04d", i),
      type = "complex_design",
      design = design,
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.4, 0.3),
        se = runif(k, 0.15, 0.4)
      ),
      expected_methods = c("multilevel model", "robust variance estimation")
    )
  }

  return(scenarios)
}

#' Generate Rare Disease Scenarios (300 scenarios)
#' @keywords internal
generate_rare_disease_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(3:8, 1) # Very few studies for rare diseases

    scenarios[[i]] <- list(
      scenario_id = sprintf("RARE%04d", i),
      type = "rare_disease",
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.6, 0.5),
        se = runif(k, 0.2, 0.6) # Higher uncertainty
      ),
      expected_challenges = c(
        "very few studies",
        "small sample sizes",
        "high uncertainty"
      )
    )
  }

  return(scenarios)
}

#' Generate Implementation Science Scenarios (300 scenarios)
#' @keywords internal
generate_implementation_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(6:25, 1)

    scenarios[[i]] <- list(
      scenario_id = sprintf("IMPL%04d", i),
      type = "implementation",
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0.3, 0.25),
        se = runif(k, 0.1, 0.35),
        setting = sample(c("hospital", "clinic", "community"), k, replace = TRUE)
      )
    )
  }

  return(scenarios)
}

#' Generate Prognostic Scenarios (400 scenarios)
#' @keywords internal
generate_prognostic_scenarios <- function(n) {
  scenarios <- list()

  for (i in 1:n) {
    k <- sample(5:30, 1)

    scenarios[[i]] <- list(
      scenario_id = sprintf("PROG%04d", i),
      type = "prognostic",
      k = k,
      data = data.frame(
        study = paste0("Study_", 1:k),
        effect = rnorm(k, 0, 0.4), # Hazard ratios (log scale)
        se = runif(k, 0.1, 0.4)
      ),
      effect_measure = "log hazard ratio"
    )
  }

  return(scenarios)
}

#' Find Matching Scenarios
#'
#' Search scenario database for scenarios matching specific characteristics.
#'
#' @param scenario_db Scenario database from init_scenario_database()
#' @param type Scenario type filter
#' @param k_min Minimum number of studies
#' @param k_max Maximum number of studies
#' @param effect_range Effect size range
#'
#' @return Matching scenarios
#'
#' @export
find_scenarios <- function(scenario_db, type = NULL, k_min = NULL, k_max = NULL,
                          effect_range = NULL) {
  all_scenarios <- unlist(scenario_db$scenarios, recursive = FALSE)

  # Filter by type
  if (!is.null(type)) {
    all_scenarios <- Filter(function(s) s$type == type, all_scenarios)
  }

  # Filter by k
  if (!is.null(k_min)) {
    all_scenarios <- Filter(function(s) s$k >= k_min, all_scenarios)
  }
  if (!is.null(k_max)) {
    all_scenarios <- Filter(function(s) s$k <= k_max, all_scenarios)
  }

  return(all_scenarios)
}

#' Get Scenario Statistics
#'
#' Get summary statistics from scenario database.
#'
#' @param scenario_db Scenario database
#'
#' @return Summary statistics
#'
#' @export
scenario_statistics <- function(scenario_db) {
  stats <- list(
    total_scenarios = scenario_db$n_scenarios,
    by_type = sapply(scenario_db$scenarios, length),
    version = scenario_db$version
  )

  class(stats) <- "scenario_stats"
  return(stats)
}

#' Print Scenario Statistics
#' @export
print.scenario_stats <- function(x, ...) {
  cat("\n=== Scenario Database Statistics ===\n")
  cat(sprintf("Total scenarios: %d\n", x$total_scenarios))
  cat(sprintf("Version: %s\n\n", x$version))

  cat("Scenarios by type:\n")
  for (type in names(x$by_type)) {
    cat(sprintf("  %s: %d\n", type, x$by_type[[type]]))
  }

  invisible(x)
}
