#' Living Systematic Review Features
#'
#' Functions for conducting and maintaining living systematic reviews that
#' continuously update as new evidence emerges. Includes sequential analysis,
#' evidence monitoring, and automated updating.
#'
#' @name living_systematic_review
NULL

#' Initialize Living Meta-Analysis
#'
#' Sets up infrastructure for a living meta-analysis with version control
#' and change tracking.
#'
#' @param initial_data Initial meta-analysis data frame
#' @param project_name Name for the living review project
#' @param update_frequency Expected update frequency (e.g., "monthly", "quarterly")
#' @param stopping_rules List of stopping rules for sequential analysis
#' @param storage_dir Directory to store versioned results (default: current dir)
#'
#' @return Object of class "living_ma" with project metadata
#'
#' @export
#' @examples
#' \dontrun{
#' living_project <- init_living_ma(
#'   initial_data = my_data,
#'   project_name = "covid_treatment_ma",
#'   update_frequency = "monthly",
#'   stopping_rules = list(alpha = 0.01, power = 0.95)
#' )
#' }
init_living_ma <- function(initial_data, project_name, update_frequency = "monthly",
                          stopping_rules = NULL, storage_dir = ".") {
  # Create project directory
  project_dir <- file.path(storage_dir, project_name)
  if (!dir.exists(project_dir)) {
    dir.create(project_dir, recursive = TRUE)
  }

  # Initialize version control
  versions_dir <- file.path(project_dir, "versions")
  if (!dir.exists(versions_dir)) {
    dir.create(versions_dir)
  }

  # Save initial data as version 1
  version_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  version_file <- file.path(versions_dir, sprintf("v1_%s.rds", version_id))

  initial_result <- list(
    version = 1,
    timestamp = Sys.time(),
    data = initial_data,
    n_studies = nrow(initial_data)
  )

  saveRDS(initial_result, version_file)

  # Create project metadata
  metadata <- list(
    project_name = project_name,
    creation_date = Sys.time(),
    update_frequency = update_frequency,
    current_version = 1,
    total_studies = nrow(initial_data),
    stopping_rules = stopping_rules,
    project_dir = project_dir,
    versions_dir = versions_dir,
    updates_log = data.frame(
      version = 1,
      timestamp = Sys.time(),
      n_studies = nrow(initial_data),
      n_new_studies = nrow(initial_data),
      notes = "Initial version",
      stringsAsFactors = FALSE
    )
  )

  # Save metadata
  saveRDS(metadata, file.path(project_dir, "metadata.rds"))

  message(sprintf("Living meta-analysis project '%s' initialized", project_name))
  message(sprintf("Project directory: %s", project_dir))

  class(metadata) <- "living_ma"
  return(metadata)
}

#' Update Living Meta-Analysis
#'
#' Adds new studies to a living meta-analysis and performs updated analysis
#' with sequential monitoring.
#'
#' @param living_ma Living MA object from init_living_ma()
#' @param new_data Data frame with new studies
#' @param notes Optional notes about this update
#' @param run_analysis Perform meta-analysis (default: TRUE)
#'
#' @return Updated living MA object with new results
#'
#' @export
update_living_ma <- function(living_ma, new_data, notes = "",
                            run_analysis = TRUE) {
  if (!inherits(living_ma, "living_ma")) {
    stop("living_ma must be of class 'living_ma'")
  }

  # Load current metadata
  metadata_file <- file.path(living_ma$project_dir, "metadata.rds")
  if (file.exists(metadata_file)) {
    metadata <- readRDS(metadata_file)
  } else {
    metadata <- living_ma
  }

  # Load most recent data
  current_version <- metadata$current_version
  version_files <- list.files(metadata$versions_dir, pattern = sprintf("v%d_", current_version),
                              full.names = TRUE)
  if (length(version_files) == 0) {
    stop("Cannot find current version data")
  }
  current_data_obj <- readRDS(version_files[1])
  current_data <- current_data_obj$data

  # Combine with new data
  combined_data <- rbind(current_data, new_data)

  # Remove duplicates based on study name
  combined_data <- combined_data[!duplicated(combined_data$study), ]

  # Create new version
  new_version <- current_version + 1
  version_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  version_file <- file.path(metadata$versions_dir,
                           sprintf("v%d_%s.rds", new_version, version_id))

  # Perform analysis if requested
  if (run_analysis) {
    yi <- combined_data$effect
    vi <- combined_data$se^2

    wi <- 1 / vi
    Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
    df <- length(yi) - 1
    C <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - df) / C)

    wi_re <- 1 / (vi + tau2)
    estimate <- sum(wi_re * yi) / sum(wi_re)
    se <- sqrt(1 / sum(wi_re))

    analysis_result <- list(
      estimate = estimate,
      se = se,
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      tau2 = tau2,
      I2 = max(0, 100 * tau2 / (tau2 + mean(vi))),
      Q = Q,
      p_value = 2 * (1 - pnorm(abs(estimate / se)))
    )
  } else {
    analysis_result <- NULL
  }

  # Save new version
  version_data <- list(
    version = new_version,
    timestamp = Sys.time(),
    data = combined_data,
    n_studies = nrow(combined_data),
    n_new_studies = nrow(new_data),
    analysis = analysis_result
  )

  saveRDS(version_data, version_file)

  # Update metadata
  metadata$current_version <- new_version
  metadata$total_studies <- nrow(combined_data)
  metadata$last_update <- Sys.time()

  # Add to updates log
  new_log_entry <- data.frame(
    version = new_version,
    timestamp = Sys.time(),
    n_studies = nrow(combined_data),
    n_new_studies = nrow(new_data),
    notes = notes,
    stringsAsFactors = FALSE
  )

  metadata$updates_log <- rbind(metadata$updates_log, new_log_entry)

  # Check stopping rules
  if (!is.null(metadata$stopping_rules) && run_analysis) {
    stop_check <- check_stopping_rules(analysis_result, metadata$stopping_rules)
    metadata$stop_recommendation <- stop_check
  }

  # Save updated metadata
  saveRDS(metadata, metadata_file)

  message(sprintf("Living MA updated to version %d", new_version))
  message(sprintf("Added %d new studies (total: %d)", nrow(new_data), nrow(combined_data)))

  if (run_analysis) {
    message(sprintf("Updated estimate: %.3f (95%% CI: %.3f to %.3f)",
                   estimate, analysis_result$ci_lower, analysis_result$ci_upper))
  }

  class(metadata) <- "living_ma"
  return(metadata)
}

#' Sequential Meta-Analysis with Monitoring Boundaries
#'
#' Performs sequential meta-analysis with O'Brien-Fleming or Lan-DeMets
#' spending function boundaries to control Type I error in living reviews.
#'
#' @param living_ma Living MA object
#' @param method Boundary method: "obrien_fleming", "pocock", "lan_demets"
#' @param alpha Overall Type I error rate (default: 0.05)
#' @param max_studies Maximum planned number of studies
#'
#' @return Object with sequential analysis results and boundaries
#'
#' @export
sequential_meta_analysis <- function(living_ma, method = "obrien_fleming",
                                    alpha = 0.05, max_studies = NULL) {
  # Load version history
  versions_dir <- living_ma$versions_dir
  version_files <- list.files(versions_dir, pattern = "^v\\d+_", full.names = TRUE)
  version_files <- version_files[order(version_files)]

  n_versions <- length(version_files)

  if (is.null(max_studies)) {
    max_studies <- living_ma$total_studies * 1.5  # Assume 50% more studies possible
  }

  # Extract results from each version
  version_results <- list()

  for (i in seq_along(version_files)) {
    version_data <- readRDS(version_files[i])

    if (!is.null(version_data$analysis)) {
      version_results[[i]] <- list(
        version = version_data$version,
        n_studies = version_data$n_studies,
        estimate = version_data$analysis$estimate,
        se = version_data$analysis$se,
        z = version_data$analysis$estimate / version_data$analysis$se,
        p_value = version_data$analysis$p_value
      )
    }
  }

  # Calculate monitoring boundaries
  information_fractions <- sapply(version_results, function(v) v$n_studies) / max_studies

  if (method == "obrien_fleming") {
    # O'Brien-Fleming boundaries
    boundaries <- qnorm(1 - alpha / (2 * sqrt(information_fractions)))
  } else if (method == "pocock") {
    # Pocock boundaries (constant)
    boundaries <- rep(qnorm(1 - alpha / (2 * log(1 + (n_versions - 1)))), n_versions)
  } else if (method == "lan_demets") {
    # Lan-DeMets O'Brien-Fleming approximation
    boundaries <- qnorm(1 - 2 * (1 - pnorm(qnorm(1 - alpha/2) / sqrt(information_fractions))))
  } else {
    stop("Unknown method. Use 'obrien_fleming', 'pocock', or 'lan_demets'")
  }

  # Check if any boundary crossed
  z_values <- sapply(version_results, function(v) abs(v$z))
  boundary_crossed <- any(z_values > boundaries)

  result <- list(
    version_results = version_results,
    information_fractions = information_fractions,
    boundaries = boundaries,
    z_values = z_values,
    boundary_crossed = boundary_crossed,
    method = method,
    alpha = alpha,
    recommendation = if(boundary_crossed) "Stop for efficacy" else "Continue monitoring"
  )

  class(result) <- "sequential_ma"
  return(result)
}

#' Check Stopping Rules for Living Review
#'
#' Evaluates whether predefined stopping criteria have been met.
#'
#' @param analysis_result Current meta-analysis result
#' @param stopping_rules List of stopping rules (alpha, power, futility_bound)
#'
#' @return List indicating whether to stop and reasons
#'
#' @export
check_stopping_rules <- function(analysis_result, stopping_rules) {
  stop_reasons <- character()
  should_stop <- FALSE

  # Efficacy stopping: strong statistical significance
  if (!is.null(stopping_rules$alpha)) {
    if (analysis_result$p_value < stopping_rules$alpha) {
      should_stop <- TRUE
      stop_reasons <- c(stop_reasons,
                       sprintf("Efficacy boundary crossed (p = %.4f < %.3f)",
                              analysis_result$p_value, stopping_rules$alpha))
    }
  }

  # Futility stopping: confidence interval excludes meaningful effect
  if (!is.null(stopping_rules$futility_bound)) {
    if (analysis_result$ci_upper < stopping_rules$futility_bound &&
        analysis_result$ci_lower > -stopping_rules$futility_bound) {
      should_stop <- TRUE
      stop_reasons <- c(stop_reasons,
                       "Futility: effect size conclusively below meaningful threshold")
    }
  }

  # Precision stopping: narrow confidence interval
  if (!is.null(stopping_rules$max_ci_width)) {
    ci_width <- analysis_result$ci_upper - analysis_result$ci_lower
    if (ci_width < stopping_rules$max_ci_width) {
      should_stop <- TRUE
      stop_reasons <- c(stop_reasons,
                       sprintf("Precision achieved: CI width = %.3f", ci_width))
    }
  }

  result <- list(
    should_stop = should_stop,
    reasons = stop_reasons,
    timestamp = Sys.time()
  )

  return(result)
}

#' Compare Versions in Living Review
#'
#' Compares results across versions to track evolution of evidence.
#'
#' @param living_ma Living MA object
#' @param versions Vector of version numbers to compare (default: all)
#'
#' @return Data frame comparing estimates across versions
#'
#' @export
compare_versions <- function(living_ma, versions = NULL) {
  versions_dir <- living_ma$versions_dir
  version_files <- list.files(versions_dir, pattern = "^v\\d+_", full.names = TRUE)
  version_files <- version_files[order(version_files)]

  results_list <- list()

  for (vfile in version_files) {
    version_data <- readRDS(vfile)

    if (!is.null(version_data$analysis)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        version = version_data$version,
        timestamp = version_data$timestamp,
        n_studies = version_data$n_studies,
        estimate = version_data$analysis$estimate,
        ci_lower = version_data$analysis$ci_lower,
        ci_upper = version_data$analysis$ci_upper,
        tau2 = version_data$analysis$tau2,
        I2 = version_data$analysis$I2,
        stringsAsFactors = FALSE
      )
    }
  }

  results <- do.call(rbind, results_list)

  # Filter by requested versions if specified
  if (!is.null(versions)) {
    results <- results[results$version %in% versions, ]
  }

  return(results)
}

#' Plot Evolution of Evidence
#'
#' Visualizes how effect estimates evolve over versions in a living review.
#'
#' @param living_ma Living MA object
#'
#' @return Plot showing effect estimate trajectory with confidence intervals
#'
#' @export
plot_living_ma <- function(living_ma) {
  comparison <- compare_versions(living_ma)

  if (nrow(comparison) == 0) {
    stop("No analysis results found")
  }

  par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

  # Plot effect estimates over versions
  plot(comparison$version, comparison$estimate,
       type = "o", pch = 19, cex = 1.5,
       xlab = "Version", ylab = "Effect Estimate",
       main = sprintf("Evidence Evolution: %s", living_ma$project_name),
       ylim = range(c(comparison$ci_lower, comparison$ci_upper)))

  # Add confidence intervals
  for (i in 1:nrow(comparison)) {
    lines(c(comparison$version[i], comparison$version[i]),
          c(comparison$ci_lower[i], comparison$ci_upper[i]),
          lwd = 2, col = "blue")
  }

  # Add null line
  abline(h = 0, lty = 2, col = "red")

  # Add grid
  grid()

  # Add sample size information
  text(comparison$version, comparison$ci_upper + 0.1,
       labels = paste0("n=", comparison$n_studies),
       cex = 0.7, pos = 3)
}

#' Automated Evidence Monitoring
#'
#' Sets up automated monitoring that checks for new evidence and sends alerts.
#'
#' @param living_ma Living MA object
#' @param search_function Custom function to search for new studies
#' @param alert_threshold Threshold for change in estimate to trigger alert
#' @param check_interval Days between checks (default: 30)
#'
#' @return Monitoring configuration object
#'
#' @export
setup_monitoring <- function(living_ma, search_function = NULL,
                            alert_threshold = 0.1, check_interval = 30) {
  monitoring_config <- list(
    project = living_ma$project_name,
    last_check = Sys.time(),
    next_check = Sys.time() + check_interval * 86400,
    search_function = search_function,
    alert_threshold = alert_threshold,
    check_interval = check_interval,
    baseline_estimate = NULL
  )

  # Get baseline estimate from current version
  versions_dir <- living_ma$versions_dir
  version_files <- list.files(versions_dir, pattern = "^v\\d+_", full.names = TRUE)
  if (length(version_files) > 0) {
    latest_version <- readRDS(version_files[length(version_files)])
    if (!is.null(latest_version$analysis)) {
      monitoring_config$baseline_estimate <- latest_version$analysis$estimate
    }
  }

  # Save monitoring config
  monitoring_file <- file.path(living_ma$project_dir, "monitoring_config.rds")
  saveRDS(monitoring_config, monitoring_file)

  message("Monitoring configured for living meta-analysis")
  message(sprintf("Next check scheduled for: %s", monitoring_config$next_check))

  return(monitoring_config)
}
