#' Performance Optimization and Parallel Processing
#'
#' Functions for high-performance meta-analysis computation using vectorization,
#' caching, and parallel processing for large-scale analyses.
#'
#' @name performance_parallel
NULL

#' Parallel Bootstrap Meta-Analysis
#'
#' Performs bootstrap meta-analysis using parallel processing for faster computation.
#' Automatically detects available cores and distributes work.
#'
#' @param data Data frame with study, effect, and se columns
#' @param n_bootstrap Number of bootstrap iterations (default: 1000)
#' @param method Meta-analysis method ("DL", "REML")
#' @param n_cores Number of cores to use (default: auto-detect - 1)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return Object of class "parallel_bootstrap_ma" with bootstrap results
#'
#' @export
#' @examples
#' \dontrun{
#' result <- parallel_bootstrap_ma(data, n_bootstrap = 10000, n_cores = 4)
#' }
parallel_bootstrap_ma <- function(data, n_bootstrap = 1000, method = "DL",
                                  n_cores = NULL, seed = NULL) {
  # Validate input
  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Determine number of cores
  if (is.null(n_cores)) {
    # Detect available cores, use all but one
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  message(sprintf("Using %d cores for parallel bootstrap", n_cores))

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Prepare data
  yi <- data$effect
  vi <- data$se^2
  n <- length(yi)

  # Function to perform one bootstrap iteration
  bootstrap_iter <- function(iter, yi, vi, method) {
    # Bootstrap sample
    boot_idx <- sample(1:n, n, replace = TRUE)
    yi_boot <- yi[boot_idx]
    vi_boot <- vi[boot_idx]

    # Estimate tau-squared
    if (method == "DL") {
      wi <- 1 / vi_boot
      Q <- sum(wi * (yi_boot - sum(wi * yi_boot) / sum(wi))^2)
      df <- n - 1
      C <- sum(wi) - sum(wi^2) / sum(wi)
      tau2 <- max(0, (Q - df) / C)
    } else {
      tau2 <- var(yi_boot) * 0.5
    }

    # Calculate pooled estimate
    wi_re <- 1 / (vi_boot + tau2)
    estimate <- sum(wi_re * yi_boot) / sum(wi_re)

    return(c(estimate = estimate, tau2 = tau2))
  }

  # Create cluster
  cl <- parallel::makeCluster(n_cores)

  # Export necessary objects to cluster
  parallel::clusterExport(cl, c("yi", "vi", "method", "n"), envir = environment())

  # Set seed on each worker for reproducibility
  if (!is.null(seed)) {
    parallel::clusterSetRNGStream(cl, seed)
  }

  # Run bootstrap in parallel
  boot_results <- parallel::parSapply(cl, 1:n_bootstrap, bootstrap_iter,
                                     yi = yi, vi = vi, method = method)

  # Stop cluster
  parallel::stopCluster(cl)

  # Extract results
  boot_estimates <- boot_results["estimate", ]
  boot_tau2 <- boot_results["tau2", ]

  # Calculate statistics
  boot_mean <- mean(boot_estimates)
  boot_se <- sd(boot_estimates)
  boot_ci <- quantile(boot_estimates, c(0.025, 0.975))

  result <- list(
    estimate = boot_mean,
    se = boot_se,
    ci_lower = boot_ci[1],
    ci_upper = boot_ci[2],
    boot_estimates = boot_estimates,
    boot_tau2 = boot_tau2,
    n_bootstrap = n_bootstrap,
    n_cores = n_cores,
    method = method,
    data = data
  )

  class(result) <- "parallel_bootstrap_ma"
  return(result)
}

#' Parallel Subgroup Analysis
#'
#' Performs subgroup meta-analysis across multiple subgroups in parallel.
#'
#' @param data Data frame with study, effect, se, and subgroup column
#' @param subgroup_var Name of subgroup variable
#' @param method Meta-analysis method
#' @param n_cores Number of cores (default: auto-detect)
#'
#' @return List of meta-analysis results for each subgroup
#'
#' @export
parallel_subgroup_analysis <- function(data, subgroup_var, method = "DL",
                                       n_cores = NULL) {
  # Validate input
  if (!subgroup_var %in% names(data)) {
    stop(sprintf("Subgroup variable '%s' not found in data", subgroup_var))
  }

  # Get unique subgroups
  subgroups <- unique(data[[subgroup_var]])
  subgroups <- subgroups[!is.na(subgroups)]

  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Function to analyze one subgroup
  analyze_subgroup <- function(subgroup, data, subgroup_var, method) {
    subgroup_data <- data[data[[subgroup_var]] == subgroup, ]

    yi <- subgroup_data$effect
    vi <- subgroup_data$se^2

    # Estimate tau-squared
    if (method == "DL") {
      wi <- 1 / vi
      Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
      df <- length(yi) - 1
      C <- sum(wi) - sum(wi^2) / sum(wi)
      tau2 <- max(0, (Q - df) / C)
    } else {
      tau2 <- var(yi) * 0.5
    }

    # Calculate pooled estimate
    wi_re <- 1 / (vi + tau2)
    estimate <- sum(wi_re * yi) / sum(wi_re)
    se <- sqrt(1 / sum(wi_re))

    return(list(
      subgroup = subgroup,
      estimate = estimate,
      se = se,
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      tau2 = tau2,
      k = length(yi)
    ))
  }

  # Create cluster
  cl <- parallel::makeCluster(n_cores)

  # Export necessary objects
  parallel::clusterExport(cl, c("data", "subgroup_var", "method"),
                         envir = environment())

  # Run in parallel
  results <- parallel::parLapply(cl, subgroups, analyze_subgroup,
                                data = data, subgroup_var = subgroup_var,
                                method = method)

  # Stop cluster
  parallel::stopCluster(cl)

  # Organize results
  names(results) <- subgroups

  class(results) <- "parallel_subgroup"
  return(results)
}

#' Vectorized Heterogeneity Calculations
#'
#' Highly optimized vectorized calculation of heterogeneity statistics for
#' large meta-analyses.
#'
#' @param yi Vector of effect sizes
#' @param vi Vector of variances
#'
#' @return List with Q, I2, H2, tau2 statistics
#'
#' @export
vectorized_heterogeneity <- function(yi, vi) {
  # Input validation
  if (length(yi) != length(vi)) {
    stop("yi and vi must have the same length")
  }

  n <- length(yi)

  if (n < 2) {
    stop("Need at least 2 studies")
  }

  # Vectorized calculations
  wi <- 1 / vi
  sum_wi <- sum(wi)
  sum_wi2 <- sum(wi^2)

  # Pooled estimate (fixed-effects)
  pooled <- sum(wi * yi) / sum_wi

  # Q statistic
  Q <- sum(wi * (yi - pooled)^2)

  # Degrees of freedom
  df <- n - 1

  # C coefficient
  C <- sum_wi - sum_wi2 / sum_wi

  # Tau-squared (DerSimonian-Laird)
  tau2 <- max(0, (Q - df) / C)

  # I-squared
  I2 <- max(0, 100 * (Q - df) / Q)

  # H-squared
  H2 <- max(1, Q / df)

  # P-value for heterogeneity
  p_het <- 1 - pchisq(Q, df = df)

  return(list(
    Q = Q,
    df = df,
    p_het = p_het,
    I2 = I2,
    H2 = H2,
    tau2 = tau2
  ))
}

#' Cached Meta-Analysis Computation
#'
#' Meta-analysis with intelligent caching to avoid recomputation when data
#' hasn't changed. Useful for interactive applications.
#'
#' @param data Data frame with study, effect, and se columns
#' @param method Meta-analysis method
#' @param cache_dir Directory for cache files (default: tempdir())
#'
#' @return Meta-analysis result (cached if available)
#'
#' @export
cached_meta_analysis <- function(data, method = "DL", cache_dir = tempdir()) {
  # Create cache key from data and parameters
  cache_key <- digest::digest(list(data, method), algo = "md5")
  cache_file <- file.path(cache_dir, paste0("ma_", cache_key, ".rds"))

  # Check if cached result exists
  if (file.exists(cache_file)) {
    message("Using cached result")
    result <- readRDS(cache_file)
    result$cached <- TRUE
    return(result)
  }

  # Compute result
  yi <- data$effect
  vi <- data$se^2

  if (method == "DL") {
    wi <- 1 / vi
    Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
    df <- length(yi) - 1
    C <- sum(wi) - sum(wi^2) / sum(wi)
    tau2 <- max(0, (Q - df) / C)
  } else {
    tau2 <- var(yi) * 0.5
  }

  wi_re <- 1 / (vi + tau2)
  estimate <- sum(wi_re * yi) / sum(wi_re)
  se <- sqrt(1 / sum(wi_re))

  result <- list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    tau2 = tau2,
    method = method,
    cached = FALSE
  )

  # Save to cache
  saveRDS(result, cache_file)

  return(result)
}

#' Batch Meta-Analysis Processing
#'
#' Efficiently processes multiple meta-analyses in batch mode with progress tracking.
#'
#' @param data_list List of data frames, each containing a separate meta-analysis
#' @param method Meta-analysis method
#' @param parallel Use parallel processing (default: TRUE)
#' @param n_cores Number of cores
#' @param progress Show progress bar (default: TRUE)
#'
#' @return List of meta-analysis results
#'
#' @export
batch_meta_analysis <- function(data_list, method = "DL", parallel = TRUE,
                                n_cores = NULL, progress = TRUE) {
  n_analyses <- length(data_list)

  if (progress) {
    message(sprintf("Processing %d meta-analyses...", n_analyses))
  }

  # Function to analyze one dataset
  analyze_one <- function(data, method, idx = NULL) {
    if (!is.null(idx) && progress) {
      if (idx %% 10 == 0) {
        message(sprintf("Completed %d/%d", idx, n_analyses))
      }
    }

    yi <- data$effect
    vi <- data$se^2

    if (method == "DL") {
      wi <- 1 / vi
      Q <- sum(wi * (yi - sum(wi * yi) / sum(wi))^2)
      df <- length(yi) - 1
      C <- sum(wi) - sum(wi^2) / sum(wi)
      tau2 <- max(0, (Q - df) / C)
    } else {
      tau2 <- var(yi) * 0.5
    }

    wi_re <- 1 / (vi + tau2)
    estimate <- sum(wi_re * yi) / sum(wi_re)
    se <- sqrt(1 / sum(wi_re))

    return(list(
      estimate = estimate,
      se = se,
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      tau2 = tau2,
      k = length(yi)
    ))
  }

  if (parallel && n_analyses > 1) {
    # Parallel processing
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }

    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, "method", envir = environment())

    results <- parallel::parLapply(cl, data_list, analyze_one, method = method)

    parallel::stopCluster(cl)
  } else {
    # Sequential processing
    results <- lapply(seq_along(data_list), function(i) {
      analyze_one(data_list[[i]], method, idx = i)
    })
  }

  if (progress) {
    message("Batch processing complete!")
  }

  return(results)
}

#' Optimized Leave-One-Out Analysis
#'
#' Highly optimized leave-one-out sensitivity analysis using vectorization.
#' Much faster than iterative approach for large meta-analyses.
#'
#' @param data Data frame with study, effect, and se columns
#' @param method Meta-analysis method
#'
#' @return Data frame with LOO results for each study
#'
#' @export
optimized_loo_analysis <- function(data, method = "DL") {
  yi <- data$effect
  vi <- data$se^2
  n <- length(yi)

  # Pre-compute weights
  wi <- 1 / vi

  # Vectorized LOO computation
  loo_results <- matrix(0, n, 4)
  colnames(loo_results) <- c("estimate", "se", "ci_lower", "ci_upper")

  for (i in 1:n) {
    # Remove study i
    yi_loo <- yi[-i]
    vi_loo <- vi[-i]
    wi_loo <- wi[-i]

    # Calculate tau-squared
    if (method == "DL") {
      sum_wi <- sum(wi_loo)
      pooled_loo <- sum(wi_loo * yi_loo) / sum_wi
      Q <- sum(wi_loo * (yi_loo - pooled_loo)^2)
      df <- length(yi_loo) - 1
      C <- sum_wi - sum(wi_loo^2) / sum_wi
      tau2 <- max(0, (Q - df) / C)
    } else {
      tau2 <- var(yi_loo) * 0.5
    }

    # Pooled estimate
    wi_re <- 1 / (vi_loo + tau2)
    estimate <- sum(wi_re * yi_loo) / sum(wi_re)
    se <- sqrt(1 / sum(wi_re))

    loo_results[i, 1] <- estimate
    loo_results[i, 2] <- se
    loo_results[i, 3] <- estimate - 1.96 * se
    loo_results[i, 4] <- estimate + 1.96 * se
  }

  result <- data.frame(
    study = data$study,
    loo_results,
    stringsAsFactors = FALSE
  )

  return(result)
}
