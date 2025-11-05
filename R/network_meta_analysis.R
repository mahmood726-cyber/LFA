#' Network Meta-Analysis
#'
#' Functions for network meta-analysis (also called mixed treatment comparisons or
#' multiple treatment meta-analysis) that allows simultaneous comparison of multiple
#' interventions.
#'
#' @name network_meta_analysis
NULL

#' Network Meta-Analysis using Contrast-Based Approach
#'
#' Performs network meta-analysis using a contrast-based (two-stage) approach.
#' First estimates treatment contrasts within each study, then pools across studies.
#'
#' @param data Data frame with columns:
#'   \itemize{
#'     \item study: Study identifier
#'     \item treatment: Treatment identifier
#'     \item effect: Effect size (e.g., mean difference, log odds ratio)
#'     \item se: Standard error
#'     \item baseline: Baseline/reference treatment for the contrast
#'   }
#' @param reference Reference treatment for network (if NULL, uses most common)
#' @param method Method for pooling: "fixed" or "random" (default)
#'
#' @return Object of class "nma" containing:
#'   \itemize{
#'     \item treatment_effects: Relative effects vs reference
#'     \item comparison_matrix: All pairwise comparisons
#'     \item network_plot_data: Data for plotting network structure
#'     \item consistency_test: Test for consistency/transitivity
#'     \item ranking: Treatment ranking with probabilities
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Example network with 3 treatments (A, B, C)
#' nma_data <- data.frame(
#'   study = c(1, 1, 2, 2, 3, 3),
#'   treatment = c("A", "B", "A", "C", "B", "C"),
#'   effect = c(0, 0.5, 0, 0.8, 0, 0.3),
#'   se = c(0.1, 0.15, 0.1, 0.2, 0.12, 0.18),
#'   baseline = c("A", "A", "A", "A", "B", "B")
#' )
#' result <- network_meta_analysis(nma_data)
#' print(result)
#' }
network_meta_analysis <- function(data, reference = NULL, method = "random") {
  # Validate data
  required_cols <- c("study", "treatment", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get unique treatments
  treatments <- unique(c(data$treatment, if ("baseline" %in% names(data)) data$baseline else NULL))
  treatments <- sort(treatments[!is.na(treatments)])
  n_treatments <- length(treatments)

  if (n_treatments < 3) {
    stop("Network meta-analysis requires at least 3 treatments")
  }

  # Determine reference treatment
  if (is.null(reference)) {
    # Use most common treatment as reference
    treatment_freq <- table(data$treatment)
    reference <- names(treatment_freq)[which.max(treatment_freq)]
  }

  if (!reference %in% treatments) {
    stop(sprintf("Reference treatment '%s' not found in data", reference))
  }

  # Create design matrix for network
  # This is a simplified approach - full NMA would use multivariate meta-analysis

  # For each treatment, estimate effect vs reference
  treatment_effects <- data.frame(
    treatment = character(),
    estimate = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )

  for (trt in treatments) {
    if (trt == reference) {
      # Reference has effect of 0
      treatment_effects <- rbind(treatment_effects, data.frame(
        treatment = trt,
        estimate = 0,
        se = 0,
        ci_lower = 0,
        ci_upper = 0
      ))
    } else {
      # Get direct comparisons with reference
      direct_data <- data[data$treatment == trt |
                         (("baseline" %in% names(data)) && data$baseline == reference && data$treatment == trt), ]

      if (nrow(direct_data) > 0) {
        # Pool direct evidence
        ma_result <- cbamm_fast(data.frame(
          study = direct_data$study,
          effect = direct_data$effect,
          se = direct_data$se
        ), method = if (method == "random") "DL" else "FE", verbose = FALSE)

        treatment_effects <- rbind(treatment_effects, data.frame(
          treatment = trt,
          estimate = ma_result$estimate,
          se = ma_result$se,
          ci_lower = ma_result$ci_lower,
          ci_upper = ma_result$ci_upper
        ))
      } else {
        # Indirect evidence only - need to compute through network
        # Simplified: mark as NA for now
        treatment_effects <- rbind(treatment_effects, data.frame(
          treatment = trt,
          estimate = NA,
          se = NA,
          ci_lower = NA,
          ci_upper = NA
        ))
      }
    }
  }

  # Create comparison matrix (all pairwise comparisons)
  comparison_matrix <- matrix(NA, nrow = n_treatments, ncol = n_treatments,
                              dimnames = list(treatments, treatments))

  for (i in 1:n_treatments) {
    for (j in 1:n_treatments) {
      if (i == j) {
        comparison_matrix[i, j] <- 0
      } else {
        # Difference of effects vs reference
        eff_i <- treatment_effects$estimate[treatment_effects$treatment == treatments[i]]
        eff_j <- treatment_effects$estimate[treatment_effects$treatment == treatments[j]]
        comparison_matrix[i, j] <- eff_i - eff_j
      }
    }
  }

  # Network structure for plotting
  network_structure <- data.frame(
    comparison = character(),
    n_studies = integer(),
    stringsAsFactors = FALSE
  )

  # Count direct comparisons
  for (i in 1:(n_treatments - 1)) {
    for (j in (i + 1):n_treatments) {
      trt1 <- treatments[i]
      trt2 <- treatments[j]

      # Count studies with this comparison
      n_studies <- sum((data$treatment == trt1 &
                       (("baseline" %in% names(data)) && data$baseline == trt2)) |
                      (data$treatment == trt2 &
                       (("baseline" %in% names(data)) && data$baseline == trt1)))

      if (n_studies > 0) {
        network_structure <- rbind(network_structure, data.frame(
          comparison = paste(trt1, "vs", trt2),
          trt1 = trt1,
          trt2 = trt2,
          n_studies = n_studies
        ))
      }
    }
  }

  # Consistency check (simplified)
  # Full implementation would use loop-specific approach
  consistency_ok <- !any(is.na(treatment_effects$estimate[-1]))

  # Treatment ranking
  # Rank treatments by effect size
  ranking <- treatment_effects[order(treatment_effects$estimate, decreasing = TRUE), ]
  ranking$rank <- 1:nrow(ranking)
  ranking$prob_best <- NA  # Would compute from MCMC in full implementation

  result <- list(
    treatment_effects = treatment_effects,
    comparison_matrix = comparison_matrix,
    network_structure = network_structure,
    reference = reference,
    treatments = treatments,
    n_treatments = n_treatments,
    ranking = ranking,
    consistency = list(
      consistent = consistency_ok,
      note = "Simplified consistency check. Full implementation would use node-splitting."
    ),
    method = method,
    note = "Simplified NMA. For complex networks, consider specialized packages like netmeta or gemtc."
  )

  class(result) <- "nma"
  return(result)
}


#' Plot Network Structure
#'
#' Creates a network plot showing treatments as nodes and comparisons as edges.
#'
#' @param nma_result Object from network_meta_analysis()
#' @param node_size Size of treatment nodes (default: 2)
#' @param edge_width_scale Scale factor for edge widths (default: 1)
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
plot_network <- function(nma_result, node_size = 2, edge_width_scale = 1,
                         main = "Network Structure") {
  if (!inherits(nma_result, "nma")) {
    stop("Input must be an nma object")
  }

  treatments <- nma_result$treatments
  n_trt <- length(treatments)
  network <- nma_result$network_structure

  # Create circular layout for nodes
  angles <- seq(0, 2 * pi, length.out = n_trt + 1)[1:n_trt]
  x_coords <- cos(angles)
  y_coords <- sin(angles)

  # Create plot
  plot(x_coords, y_coords, type = "n",
       xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
       xlab = "", ylab = "", main = main,
       axes = FALSE, asp = 1)

  # Draw edges (connections)
  if (nrow(network) > 0) {
    for (i in 1:nrow(network)) {
      trt1_idx <- which(treatments == network$trt1[i])
      trt2_idx <- which(treatments == network$trt2[i])

      # Line width proportional to number of studies
      lwd <- network$n_studies[i] * edge_width_scale

      segments(x_coords[trt1_idx], y_coords[trt1_idx],
               x_coords[trt2_idx], y_coords[trt2_idx],
               lwd = lwd, col = rgb(0.5, 0.5, 0.5, 0.6))
    }
  }

  # Draw nodes
  points(x_coords, y_coords, pch = 21,
         bg = "lightblue", cex = node_size * 3, lwd = 2)

  # Add treatment labels
  text(x_coords * 1.2, y_coords * 1.2, labels = treatments,
       cex = 1.2, font = 2)

  # Add reference marker
  ref_idx <- which(treatments == nma_result$reference)
  points(x_coords[ref_idx], y_coords[ref_idx], pch = 21,
         bg = "gold", cex = node_size * 3, lwd = 3)

  # Legend
  legend("bottomright",
         legend = c("Treatments", "Reference", "# Studies"),
         pch = c(21, 21, NA),
         pt.bg = c("lightblue", "gold", NA),
         lty = c(NA, NA, 1),
         lwd = c(NA, NA, 2),
         bty = "n")

  invisible(NULL)
}


#' Forest Plot for Network Meta-Analysis
#'
#' Creates a forest plot showing all treatment effects relative to reference.
#'
#' @param nma_result Object from network_meta_analysis()
#' @param order_by How to order treatments: "effect" (default), "alphabetical", or "rank"
#' @param main Plot title
#'
#' @return Invisibly returns NULL
#'
#' @export
forest_plot_nma <- function(nma_result, order_by = "effect",
                             main = "Network Meta-Analysis: Treatment Effects") {
  if (!inherits(nma_result, "nma")) {
    stop("Input must be an nma object")
  }

  effects <- nma_result$treatment_effects

  # Remove reference (has effect = 0)
  effects <- effects[effects$treatment != nma_result$reference, ]

  # Order
  if (order_by == "effect") {
    effects <- effects[order(effects$estimate), ]
  } else if (order_by == "alphabetical") {
    effects <- effects[order(effects$treatment), ]
  } else if (order_by == "rank") {
    effects <- effects[order(-effects$estimate), ]
  }

  k <- nrow(effects)

  # Set up plot
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(5, 10, 4, 2))

  xlim <- range(c(effects$ci_lower, effects$ci_upper), na.rm = TRUE)
  xlim <- xlim + c(-0.1, 0.1) * diff(xlim)

  plot(NULL, xlim = xlim, ylim = c(0, k + 2),
       xlab = sprintf("Effect vs %s", nma_result$reference),
       ylab = "", yaxt = "n", main = main,
       frame.plot = FALSE)

  # Add null line
  abline(v = 0, lty = 2, col = "red", lwd = 2)

  # Plot treatments
  for (i in 1:k) {
    y_pos <- k - i + 1

    # CI line
    segments(effects$ci_lower[i], y_pos, effects$ci_upper[i], y_pos, lwd = 2)

    # Point estimate
    points(effects$estimate[i], y_pos, pch = 18, cex = 2)

    # Label
    axis(2, at = y_pos, labels = effects$treatment[i], las = 1,
         tick = FALSE, cex.axis = 1)
  }

  invisible(NULL)
}


#' Rank Treatments in Network Meta-Analysis
#'
#' Ranks treatments based on their estimated effects. In a full Bayesian
#' implementation, would provide probabilities of being best.
#'
#' @param nma_result Object from network_meta_analysis()
#' @param higher_better Logical indicating if higher values are better (default: TRUE)
#'
#' @return Data frame with treatment rankings
#'
#' @export
rank_treatments <- function(nma_result, higher_better = TRUE) {
  if (!inherits(nma_result, "nma")) {
    stop("Input must be an nma object")
  }

  effects <- nma_result$treatment_effects

  # Order by effect
  if (higher_better) {
    effects <- effects[order(-effects$estimate), ]
  } else {
    effects <- effects[order(effects$estimate), ]
  }

  effects$rank <- 1:nrow(effects)

  # Add interpretation
  effects$interpretation <- ifelse(
    effects$rank == 1, "Best",
    ifelse(effects$rank <= ceiling(nrow(effects) / 3), "Good",
           ifelse(effects$rank <= 2 * ceiling(nrow(effects) / 3), "Moderate",
                  "Worst"))
  )

  return(effects[, c("rank", "treatment", "estimate", "ci_lower", "ci_upper", "interpretation")])
}


#' Test for Inconsistency in Network
#'
#' Tests for inconsistency (incoherence) in the network. This simplified version
#' checks for the presence of closed loops and compares direct vs indirect evidence.
#'
#' @param nma_result Object from network_meta_analysis()
#' @param data Original data used for NMA
#'
#' @return List with inconsistency test results
#'
#' @export
test_inconsistency <- function(nma_result, data) {
  if (!inherits(nma_result, "nma")) {
    stop("Input must be an nma object")
  }

  # Simplified inconsistency check
  # Full implementation would use node-splitting or design-by-treatment interaction

  network <- nma_result$network_structure
  treatments <- nma_result$treatments

  # Check for closed loops (triangles)
  triangles <- list()

  for (i in 1:(length(treatments) - 2)) {
    for (j in (i + 1):(length(treatments) - 1)) {
      for (k in (j + 1):length(treatments)) {
        trt1 <- treatments[i]
        trt2 <- treatments[j]
        trt3 <- treatments[k]

        # Check if all three comparisons exist
        has_12 <- any(network$trt1 == trt1 & network$trt2 == trt2) |
                 any(network$trt1 == trt2 & network$trt2 == trt1)
        has_23 <- any(network$trt1 == trt2 & network$trt2 == trt3) |
                 any(network$trt1 == trt3 & network$trt2 == trt2)
        has_13 <- any(network$trt1 == trt1 & network$trt2 == trt3) |
                 any(network$trt1 == trt3 & network$trt2 == trt1)

        if (has_12 && has_23 && has_13) {
          triangles[[length(triangles) + 1]] <- c(trt1, trt2, trt3)
        }
      }
    }
  }

  result <- list(
    n_loops = length(triangles),
    loops = triangles,
    testable = length(triangles) > 0,
    note = paste(
      "Found", length(triangles), "closed loops (triangles).",
      "Full inconsistency testing would require node-splitting or design-by-treatment models."
    )
  )

  return(result)
}
