#' Cumulative Meta-Analysis
#'
#' Performs a cumulative meta-analysis, computing pooled estimates as studies
#' are added sequentially. Useful for assessing how the evidence evolves over time
#' and for detecting when results become stable.
#'
#' @param data A data frame containing the meta-analysis data with columns:
#'   study, effect, se, and optionally year or other ordering variable
#' @param order_by Character string specifying the variable to order studies by.
#'   Common choices: "year", "precision" (1/se), or "study"
#' @param decreasing Logical indicating whether to sort in decreasing order (default: FALSE)
#' @param method Method for meta-analysis (default: "DL")
#' @param assess_stability Logical indicating whether to assess stability (default: TRUE)
#' @param stability_threshold Threshold for considering estimates stable (default: 0.1)
#'
#' @return An object of class "cbamm_cumulative" containing:
#'   \itemize{
#'     \item cumulative_results: Data frame with cumulative estimates
#'     \item stability_index: Index where results become stable (if assess_stability=TRUE)
#'     \item is_stable: Logical indicating if results are currently stable
#'     \item final_estimate: Final pooled estimate
#'     \item order_by: Variable used for ordering
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   study = paste0("Study", 1:10),
#'   effect = rnorm(10, 0.5, 0.2),
#'   se = runif(10, 0.1, 0.3),
#'   year = 2010:2019
#' )
#' cum_result <- cumulative_meta_analysis(data, order_by = "year")
#' print(cum_result)
#' }
cumulative_meta_analysis <- function(data,
                                      order_by = "year",
                                      decreasing = FALSE,
                                      method = "DL",
                                      assess_stability = TRUE,
                                      stability_threshold = 0.1) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (nrow(data) < 2) {
    stop("Need at least 2 studies for cumulative meta-analysis")
  }

  # Check if order_by column exists
  if (!order_by %in% names(data)) {
    if (order_by == "precision") {
      # Create precision column
      data$precision <- 1 / data$se
    } else {
      stop(sprintf("Column '%s' not found in data", order_by))
    }
  }

  # Sort data
  data <- data[order(data[[order_by]], decreasing = decreasing), ]

  # Initialize results storage
  k <- nrow(data)
  cumulative_results <- data.frame(
    k = 2:k,
    estimate = numeric(k - 1),
    se = numeric(k - 1),
    ci_lower = numeric(k - 1),
    ci_upper = numeric(k - 1),
    tau2 = numeric(k - 1),
    I2 = numeric(k - 1),
    Q = numeric(k - 1)
  )

  # Perform cumulative meta-analysis
  for (i in 2:k) {
    subset_data <- data[1:i, ]
    ma_result <- cbamm_fast(subset_data, method = method, verbose = FALSE)

    cumulative_results[i - 1, "estimate"] <- ma_result$estimate
    cumulative_results[i - 1, "se"] <- ma_result$se
    cumulative_results[i - 1, "ci_lower"] <- ma_result$ci_lower
    cumulative_results[i - 1, "ci_upper"] <- ma_result$ci_upper
    cumulative_results[i - 1, "tau2"] <- ma_result$tau2
    cumulative_results[i - 1, "I2"] <- ma_result$I2
    cumulative_results[i - 1, "Q"] <- ma_result$Q
  }

  # Assess stability
  stability_index <- NA
  is_stable <- FALSE

  if (assess_stability && k >= 5) {
    # Check when estimate changes by less than threshold
    estimate_changes <- abs(diff(cumulative_results$estimate))

    # Find first instance where changes remain below threshold for 3 consecutive additions
    for (i in 1:(length(estimate_changes) - 2)) {
      if (all(estimate_changes[i:(i + 2)] < stability_threshold)) {
        stability_index <- i + 2  # Index in cumulative_results
        is_stable <- TRUE
        break
      }
    }

    # If we reached the end and last few are stable
    if (is.na(stability_index) && length(estimate_changes) >= 3) {
      last_three <- estimate_changes[(length(estimate_changes) - 2):length(estimate_changes)]
      if (all(last_three < stability_threshold)) {
        is_stable <- TRUE
      }
    }
  }

  # Final estimate
  final_result <- cbamm_fast(data, method = method, verbose = FALSE)

  # Create result object
  result <- list(
    cumulative_results = cumulative_results,
    stability_index = stability_index,
    is_stable = is_stable,
    stability_threshold = stability_threshold,
    final_estimate = final_result,
    order_by = order_by,
    n_studies = k,
    data = data
  )

  class(result) <- "cbamm_cumulative"
  return(result)
}
