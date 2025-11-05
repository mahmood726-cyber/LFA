#' Compute Transport Weights for Generalizability
#'
#' Computes inverse odds weights for transporting (generalizing) meta-analytic
#' results to a target population. This addresses concerns about external validity
#' when the study populations differ from the target population of interest.
#'
#' @param data A data frame containing study-level covariate data
#' @param target_population A named list of target population characteristics
#' @param covariates Character vector of covariate names to use for weighting.
#'   If NULL, uses all columns in target_population that exist in data
#' @param method Method for computing weights: "simple" (default) or "propensity"
#'
#' @return A list containing:
#'   \itemize{
#'     \item weights: Vector of transport weights for each study
#'     \item normalized_weights: Normalized weights that sum to 1
#'     \item effective_n: Effective sample size after weighting
#'     \item diagnostics: Data frame with weight diagnostics
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:5),
#'   age_mean = c(55, 60, 65, 58, 62),
#'   female_pct = c(0.45, 0.50, 0.55, 0.48, 0.52),
#'   effect = rnorm(5, 0.5, 0.2),
#'   se = runif(5, 0.1, 0.3)
#' )
#'
#' target_population <- list(
#'   age_mean = 60,
#'   female_pct = 0.50
#' )
#'
#' weights <- compute_transport_weights(data, target_population)
#' }
compute_transport_weights <- function(data,
                                       target_population,
                                       covariates = NULL,
                                       method = "simple") {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.list(target_population)) {
    stop("target_population must be a named list")
  }

  if (is.null(names(target_population)) || any(names(target_population) == "")) {
    stop("target_population must have named elements")
  }

  # Determine covariates to use
  if (is.null(covariates)) {
    covariates <- names(target_population)
  }

  # Check that covariates exist in data
  missing_covs <- setdiff(covariates, names(data))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in data: ", paste(missing_covs, collapse = ", "))
  }

  # Check that all covariates have target values
  missing_targets <- setdiff(covariates, names(target_population))
  if (length(missing_targets) > 0) {
    stop("Target values not provided for: ", paste(missing_targets, collapse = ", "))
  }

  k <- nrow(data)

  if (method == "simple") {
    # Simple distance-based weighting
    # Compute standardized distances from target population

    distances <- numeric(k)

    for (cov in covariates) {
      study_vals <- data[[cov]]
      target_val <- target_population[[cov]]

      # Standardize by standard deviation
      sd_val <- sd(study_vals, na.rm = TRUE)
      if (sd_val > 0) {
        std_dist <- (study_vals - target_val) / sd_val
      } else {
        std_dist <- rep(0, k)
      }

      distances <- distances + std_dist^2
    }

    # Convert distances to weights (inverse of distance)
    # Add small constant to avoid division by zero
    weights <- 1 / (1 + sqrt(distances))

  } else if (method == "propensity") {
    # Propensity score-based weighting
    # This is a simplified version; full implementation would use logistic regression

    # Create a pseudo-target population observation
    target_df <- as.data.frame(target_population[covariates])
    target_df$is_target <- 1

    # Mark study data as non-target
    study_df <- data[, covariates, drop = FALSE]
    study_df$is_target <- 0

    # Combine
    combined_df <- rbind(target_df, study_df)

    # Compute distances from target (simplified propensity approach)
    study_vals_matrix <- as.matrix(study_df[, covariates, drop = FALSE])
    target_vals <- unlist(target_population[covariates])

    # Mahalanobis-like distance
    cov_matrix <- cov(study_vals_matrix)
    if (all(eigen(cov_matrix)$values > 1e-8)) {
      # Compute Mahalanobis distance
      inv_cov <- solve(cov_matrix)
      distances <- numeric(k)
      for (i in 1:k) {
        diff <- study_vals_matrix[i, ] - target_vals
        distances[i] <- sqrt(t(diff) %*% inv_cov %*% diff)
      }
    } else {
      # Fall back to Euclidean distance if covariance matrix is singular
      distances <- sqrt(rowSums((study_vals_matrix -
                                  matrix(target_vals, nrow = k, ncol = length(covariates), byrow = TRUE))^2))
    }

    # Convert to weights
    weights <- 1 / (1 + distances)

  } else {
    stop("method must be 'simple' or 'propensity'")
  }

  # Normalize weights to sum to 1
  normalized_weights <- weights / sum(weights)

  # Effective sample size
  effective_n <- sum(normalized_weights)^2 / sum(normalized_weights^2)

  # Diagnostics
  diagnostics <- data.frame(
    study = if ("study" %in% names(data)) data$study else paste0("Study", 1:k),
    weight = weights,
    normalized_weight = normalized_weights,
    stringsAsFactors = FALSE
  )

  result <- list(
    weights = weights,
    normalized_weights = normalized_weights,
    effective_n = effective_n,
    diagnostics = diagnostics,
    covariates = covariates,
    method = method,
    target_population = target_population
  )

  return(result)
}


#' Compute Analysis Weights
#'
#' Computes analysis weights combining sampling variance and transport weights.
#'
#' @param data A data frame with effect sizes and standard errors
#' @param transport_weights Optional transport weights from compute_transport_weights()
#'
#' @return Vector of analysis weights
#' @export
compute_analysis_weights <- function(data, transport_weights = NULL) {
  if (!"var" %in% names(data)) {
    if (!"se" %in% names(data)) {
      stop("data must contain 'se' or 'var' column")
    }
    data$var <- data$se^2
  }

  # Inverse variance weights
  iv_weights <- 1 / data$var

  if (!is.null(transport_weights)) {
    # Combine with transport weights
    if (is.list(transport_weights) && "weights" %in% names(transport_weights)) {
      tw <- transport_weights$weights
    } else {
      tw <- transport_weights
    }

    if (length(tw) != nrow(data)) {
      stop("Length of transport_weights does not match number of studies")
    }

    # Combined weights
    weights <- iv_weights * tw
  } else {
    weights <- iv_weights
  }

  # Normalize
  return(weights / sum(weights))
}
