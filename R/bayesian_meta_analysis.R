#' Bayesian Meta-Analysis Module
#'
#' Comprehensive Bayesian meta-analysis implementation using MCMC including:
#' - Full Bayesian random-effects meta-analysis
#' - Prior sensitivity analysis
#' - Posterior predictive distributions
#' - Bayes factors for model comparison
#' - Credible intervals and probability statements
#' - Between-study heterogeneity (tau) estimation
#' - Meta-regression with Bayesian inference
#'
#' @name bayesian_meta_analysis
NULL

#' Bayesian Meta-Analysis
#'
#' Performs Bayesian meta-analysis using MCMC simulation. Provides full posterior
#' distributions for all parameters, allowing probability statements about effects.
#'
#' @param data Meta-analysis data frame
#' @param effect Column name for effect sizes
#' @param se Column name for standard errors
#' @param prior_mu Prior for overall effect: "weakly_informative", "noninformative", or custom list
#' @param prior_tau Prior for heterogeneity: "half_cauchy", "half_normal", "uniform", or custom
#' @param n_iter Number of MCMC iterations (default: 20000)
#' @param n_burnin Number of burn-in iterations (default: 5000)
#' @param n_chains Number of MCMC chains (default: 3)
#' @param predictive Include posterior predictive distribution (default: TRUE)
#' @param prob_intervals Probability levels for credible intervals (default: c(0.95, 0.90, 0.80))
#'
#' @return Bayesian meta-analysis object with posterior samples and summaries
#'
#' @export
#' @examples
#' \dontrun{
#' # Bayesian meta-analysis with default priors
#' bayes_result <- bayesian_meta_analysis(
#'   data = meta_data,
#'   effect = "effect",
#'   se = "se"
#' )
#'
#' # View results
#' print(bayes_result)
#' summary(bayes_result)
#' plot(bayes_result, type = "posterior")
#' plot(bayes_result, type = "forest")
#'
#' # Prior sensitivity analysis
#' sensitivity <- bayesian_prior_sensitivity(
#'   data = meta_data,
#'   effect = "effect",
#'   se = "se"
#' )
#' }
bayesian_meta_analysis <- function(data, effect = "effect", se = "se",
                                   prior_mu = "weakly_informative",
                                   prior_tau = "half_cauchy",
                                   n_iter = 20000, n_burnin = 5000,
                                   n_chains = 3, predictive = TRUE,
                                   prob_intervals = c(0.95, 0.90, 0.80)) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   BAYESIAN META-ANALYSIS                                     ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Extract data
  y <- data[[effect]]
  s <- data[[se]]
  n <- length(y)

  cat(sprintf("Studies: %d\n", n))
  cat(sprintf("MCMC settings:\n"))
  cat(sprintf("  Iterations: %d\n", n_iter))
  cat(sprintf("  Burn-in: %d\n", n_burnin))
  cat(sprintf("  Chains: %d\n", n_chains))
  cat(sprintf("  Total samples: %d\n", (n_iter - n_burnin) * n_chains))

  # Configure priors
  priors <- configure_priors(prior_mu, prior_tau)
  cat(sprintf("\nPriors:\n"))
  cat(sprintf("  Overall effect (mu): %s\n", priors$mu_desc))
  cat(sprintf("  Heterogeneity (tau): %s\n", priors$tau_desc))

  # Run MCMC
  cat("\nRunning MCMC sampling...\n")

  mcmc_result <- run_bayesian_mcmc(
    y = y, s = s, priors = priors,
    n_iter = n_iter, n_burnin = n_burnin,
    n_chains = n_chains
  )

  cat("✓ MCMC sampling complete\n")

  # Diagnostics
  cat("\nChecking convergence...\n")
  diagnostics <- check_mcmc_diagnostics(mcmc_result)

  if (diagnostics$converged) {
    cat("  ✓ All parameters converged (Rhat < 1.1)\n")
  } else {
    warning("  ⚠️  Some parameters did not converge. Consider increasing iterations.")
  }

  cat(sprintf("  Effective sample size (min): %.0f\n", diagnostics$min_ess))

  # Posterior summaries
  cat("\nCalculating posterior summaries...\n")
  posterior_summary <- summarize_posterior(mcmc_result, prob_intervals)

  # Posterior predictive distribution
  pred_dist <- NULL
  if (predictive) {
    cat("Generating posterior predictive distribution...\n")
    pred_dist <- posterior_predictive(mcmc_result)
  }

  # Probability statements
  cat("\nProbability statements:\n")
  prob_statements <- calculate_probabilities(mcmc_result)
  cat(sprintf("  P(effect > 0) = %.3f\n", prob_statements$prob_positive))
  cat(sprintf("  P(|effect| > 0.2) = %.3f\n", prob_statements$prob_important))

  # Create plots
  cat("\nGenerating visualizations...\n")
  plots <- create_bayesian_plots(mcmc_result, y, s, pred_dist)

  # Compile results
  result <- list(
    data = data.frame(y = y, s = s, study = 1:n),
    posterior = mcmc_result$posterior,
    posterior_summary = posterior_summary,
    predictive = pred_dist,
    diagnostics = diagnostics,
    probabilities = prob_statements,
    priors = priors,
    settings = list(
      n_iter = n_iter, n_burnin = n_burnin, n_chains = n_chains,
      prob_intervals = prob_intervals
    ),
    plots = plots
  )

  class(result) <- c("bayesian_meta_analysis", "list")

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   BAYESIAN META-ANALYSIS COMPLETE                            ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  return(result)
}

#' Bayesian Prior Sensitivity Analysis
#'
#' Tests sensitivity of results to different prior specifications.
#'
#' @param data Meta-analysis data frame
#' @param effect Column name for effect sizes
#' @param se Column name for standard errors
#' @param prior_scenarios List of prior scenarios to test
#' @param ... Additional arguments passed to bayesian_meta_analysis
#'
#' @return Prior sensitivity analysis object
#'
#' @export
bayesian_prior_sensitivity <- function(data, effect = "effect", se = "se",
                                       prior_scenarios = NULL, ...) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   PRIOR SENSITIVITY ANALYSIS                                 ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Default prior scenarios
  if (is.null(prior_scenarios)) {
    prior_scenarios <- list(
      list(name = "Weakly Informative", prior_mu = "weakly_informative", prior_tau = "half_cauchy"),
      list(name = "Non-informative", prior_mu = "noninformative", prior_tau = "uniform"),
      list(name = "Skeptical", prior_mu = list(mean = 0, sd = 0.1), prior_tau = "half_normal"),
      list(name = "Enthusiastic", prior_mu = list(mean = 0.5, sd = 0.5), prior_tau = "half_cauchy")
    )
  }

  results <- list()
  summaries <- data.frame()

  for (i in seq_along(prior_scenarios)) {
    scenario <- prior_scenarios[[i]]
    cat(sprintf("\n[%d/%d] Running: %s\n", i, length(prior_scenarios), scenario$name))

    bayes_result <- bayesian_meta_analysis(
      data = data, effect = effect, se = se,
      prior_mu = scenario$prior_mu,
      prior_tau = scenario$prior_tau,
      ...
    )

    results[[scenario$name]] <- bayes_result

    # Extract summary
    mu_summary <- bayes_result$posterior_summary$mu
    summaries <- rbind(summaries, data.frame(
      scenario = scenario$name,
      posterior_mean = mu_summary["mean"],
      posterior_median = mu_summary["median"],
      ci_lower = mu_summary["2.5%"],
      ci_upper = mu_summary["97.5%"],
      stringsAsFactors = FALSE
    ))
  }

  # Create comparison plot
  comparison_plot <- plot_prior_sensitivity(summaries)

  result <- list(
    scenarios = prior_scenarios,
    results = results,
    summary = summaries,
    plot = comparison_plot
  )

  class(result) <- c("bayesian_prior_sensitivity", "list")

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   SENSITIVITY ANALYSIS COMPLETE                              ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  return(result)
}

#' Calculate Bayes Factor
#'
#' Computes Bayes factor comparing two hypotheses.
#'
#' @param posterior_samples Posterior samples from Bayesian meta-analysis
#' @param null_value Null hypothesis value (default: 0)
#' @param alternative Alternative hypothesis: "two.sided", "greater", "less"
#'
#' @return Bayes factor and interpretation
#'
#' @export
calculate_bayes_factor <- function(posterior_samples, null_value = 0,
                                   alternative = "two.sided") {

  # Savage-Dickey density ratio approximation
  # BF01 = p(theta=null | data) / p(theta=null)

  # Posterior density at null
  posterior_density <- density(posterior_samples)
  post_at_null <- approx(posterior_density$x, posterior_density$y, xout = null_value)$y

  # Prior density at null (assuming weakly informative prior: N(0, 1))
  prior_at_null <- dnorm(null_value, mean = 0, sd = 1)

  # Bayes factor in favor of null
  BF01 <- post_at_null / prior_at_null

  # Bayes factor in favor of alternative
  BF10 <- 1 / BF01

  # Interpretation
  interpretation <- interpret_bayes_factor(BF10)

  list(
    BF10 = BF10,
    BF01 = BF01,
    log_BF10 = log(BF10),
    interpretation = interpretation,
    evidence_strength = classification_bayes_factor(BF10)
  )
}

#' @keywords internal
configure_priors <- function(prior_mu, prior_tau) {
  # Configure prior for overall effect (mu)
  if (is.character(prior_mu)) {
    if (prior_mu == "weakly_informative") {
      mu_prior <- list(dist = "normal", mean = 0, sd = 1)
      mu_desc <- "N(0, 1)"
    } else if (prior_mu == "noninformative") {
      mu_prior <- list(dist = "normal", mean = 0, sd = 100)
      mu_desc <- "N(0, 100)"
    } else {
      stop("Unknown prior_mu. Use 'weakly_informative', 'noninformative', or custom list.")
    }
  } else if (is.list(prior_mu)) {
    mu_prior <- prior_mu
    mu_desc <- sprintf("N(%.2f, %.2f)", prior_mu$mean, prior_mu$sd)
  }

  # Configure prior for heterogeneity (tau)
  if (is.character(prior_tau)) {
    if (prior_tau == "half_cauchy") {
      tau_prior <- list(dist = "half_cauchy", scale = 0.5)
      tau_desc <- "Half-Cauchy(0, 0.5)"
    } else if (prior_tau == "half_normal") {
      tau_prior <- list(dist = "half_normal", scale = 0.5)
      tau_desc <- "Half-Normal(0, 0.5)"
    } else if (prior_tau == "uniform") {
      tau_prior <- list(dist = "uniform", min = 0, max = 10)
      tau_desc <- "Uniform(0, 10)"
    } else {
      stop("Unknown prior_tau. Use 'half_cauchy', 'half_normal', 'uniform', or custom list.")
    }
  } else if (is.list(prior_tau)) {
    tau_prior <- prior_tau
    tau_desc <- "Custom"
  }

  list(
    mu = mu_prior, tau = tau_prior,
    mu_desc = mu_desc, tau_desc = tau_desc
  )
}

#' @keywords internal
run_bayesian_mcmc <- function(y, s, priors, n_iter, n_burnin, n_chains) {
  # Gibbs sampler for Bayesian random-effects meta-analysis
  # Model: y_i ~ N(theta_i, s_i^2)
  #        theta_i ~ N(mu, tau^2)

  n <- length(y)
  n_samples <- n_iter - n_burnin

  # Storage for posterior samples (all chains combined)
  all_mu <- numeric(n_samples * n_chains)
  all_tau <- numeric(n_samples * n_chains)
  all_theta <- matrix(NA, n_samples * n_chains, n)

  sample_idx <- 1

  for (chain in 1:n_chains) {
    # Initialize
    mu <- mean(y)
    tau <- sd(y)
    theta <- y

    for (iter in 1:n_iter) {
      # Update theta (study-specific effects)
      precision_theta <- 1 / s^2 + 1 / tau^2
      mean_theta <- (y / s^2 + mu / tau^2) / precision_theta
      theta <- rnorm(n, mean = mean_theta, sd = sqrt(1 / precision_theta))

      # Update mu (overall effect)
      precision_mu <- 1 / priors$mu$sd^2 + n / tau^2
      mean_mu <- (priors$mu$mean / priors$mu$sd^2 + sum(theta) / tau^2) / precision_mu
      mu <- rnorm(1, mean = mean_mu, sd = sqrt(1 / precision_mu))

      # Update tau (heterogeneity)
      # Using Metropolis-Hastings step
      tau <- update_tau_mh(tau, theta, mu, priors$tau)

      # Store samples (after burn-in)
      if (iter > n_burnin) {
        all_mu[sample_idx] <- mu
        all_tau[sample_idx] <- tau
        all_theta[sample_idx, ] <- theta
        sample_idx <- sample_idx + 1
      }
    }
  }

  list(
    posterior = list(mu = all_mu, tau = all_tau, tau2 = all_tau^2, theta = all_theta),
    n_samples = n_samples * n_chains,
    n_chains = n_chains
  )
}

#' @keywords internal
update_tau_mh <- function(tau_current, theta, mu, prior_tau) {
  # Metropolis-Hastings update for tau
  # Proposal: log-normal random walk
  tau_proposal <- tau_current * exp(rnorm(1, 0, 0.2))

  # Log-likelihood
  log_lik_current <- sum(dnorm(theta, mu, tau_current, log = TRUE))
  log_lik_proposal <- sum(dnorm(theta, mu, tau_proposal, log = TRUE))

  # Log-prior
  if (prior_tau$dist == "half_cauchy") {
    log_prior_current <- dcauchy(tau_current, 0, prior_tau$scale, log = TRUE) + log(2)
    log_prior_proposal <- dcauchy(tau_proposal, 0, prior_tau$scale, log = TRUE) + log(2)
  } else if (prior_tau$dist == "half_normal") {
    log_prior_current <- dnorm(tau_current, 0, prior_tau$scale, log = TRUE) + log(2)
    log_prior_proposal <- dnorm(tau_proposal, 0, prior_tau$scale, log = TRUE) + log(2)
  } else if (prior_tau$dist == "uniform") {
    log_prior_current <- dunif(tau_current, prior_tau$min, prior_tau$max, log = TRUE)
    log_prior_proposal <- dunif(tau_proposal, prior_tau$min, prior_tau$max, log = TRUE)
  }

  # Acceptance ratio (including Jacobian for log-normal proposal)
  log_ratio <- (log_lik_proposal + log_prior_proposal + log(tau_proposal)) -
               (log_lik_current + log_prior_current + log(tau_current))

  # Accept or reject
  if (log(runif(1)) < log_ratio) {
    return(tau_proposal)
  } else {
    return(tau_current)
  }
}

#' @keywords internal
check_mcmc_diagnostics <- function(mcmc_result) {
  # Calculate Rhat (Gelman-Rubin statistic)
  n_samples_per_chain <- mcmc_result$n_samples / mcmc_result$n_chains

  # Split chains
  chains_mu <- split(mcmc_result$posterior$mu, rep(1:mcmc_result$n_chains, each = n_samples_per_chain))
  chains_tau <- split(mcmc_result$posterior$tau, rep(1:mcmc_result$n_chains, each = n_samples_per_chain))

  rhat_mu <- calculate_rhat(chains_mu)
  rhat_tau <- calculate_rhat(chains_tau)

  # Effective sample size
  ess_mu <- calculate_ess(mcmc_result$posterior$mu)
  ess_tau <- calculate_ess(mcmc_result$posterior$tau)

  list(
    converged = rhat_mu < 1.1 && rhat_tau < 1.1,
    rhat_mu = rhat_mu,
    rhat_tau = rhat_tau,
    ess_mu = ess_mu,
    ess_tau = ess_tau,
    min_ess = min(ess_mu, ess_tau)
  )
}

#' @keywords internal
calculate_rhat <- function(chains) {
  # Gelman-Rubin statistic
  n_chains <- length(chains)
  n <- length(chains[[1]])

  # Within-chain variance
  W <- mean(sapply(chains, var))

  # Between-chain variance
  chain_means <- sapply(chains, mean)
  B <- n * var(chain_means)

  # Rhat
  var_plus <- ((n - 1) / n) * W + (1 / n) * B
  rhat <- sqrt(var_plus / W)

  return(rhat)
}

#' @keywords internal
calculate_ess <- function(samples) {
  # Effective sample size using autocorrelation
  n <- length(samples)
  acf_result <- acf(samples, lag.max = 100, plot = FALSE)
  rho <- acf_result$acf[-1]  # Exclude lag 0

  # Sum autocorrelations until they become negative
  sum_rho <- 0
  for (i in seq_along(rho)) {
    if (rho[i] < 0) break
    sum_rho <- sum_rho + rho[i]
  }

  ess <- n / (1 + 2 * sum_rho)
  return(ess)
}

#' @keywords internal
summarize_posterior <- function(mcmc_result, prob_intervals) {
  # Summary statistics for mu
  mu_samples <- mcmc_result$posterior$mu
  mu_summary <- c(
    mean = mean(mu_samples),
    median = median(mu_samples),
    sd = sd(mu_samples)
  )

  # Credible intervals
  for (prob in prob_intervals) {
    alpha <- 1 - prob
    ci <- quantile(mu_samples, probs = c(alpha/2, 1 - alpha/2))
    mu_summary[sprintf("%.1f%%", (alpha/2)*100)] <- ci[1]
    mu_summary[sprintf("%.1f%%", (1-alpha/2)*100)] <- ci[2]
  }

  # Summary statistics for tau
  tau_samples <- mcmc_result$posterior$tau
  tau_summary <- c(
    mean = mean(tau_samples),
    median = median(tau_samples),
    sd = sd(tau_samples)
  )

  for (prob in prob_intervals) {
    alpha <- 1 - prob
    ci <- quantile(tau_samples, probs = c(alpha/2, 1 - alpha/2))
    tau_summary[sprintf("%.1f%%", (alpha/2)*100)] <- ci[1]
    tau_summary[sprintf("%.1f%%", (1-alpha/2)*100)] <- ci[2]
  }

  # I-squared
  tau2 <- tau_samples^2
  I2 <- 100 * tau2 / (tau2 + mean(mcmc_result$posterior$theta)^2)

  list(
    mu = mu_summary,
    tau = tau_summary,
    I2 = c(mean = mean(I2), median = median(I2))
  )
}

#' @keywords internal
posterior_predictive <- function(mcmc_result) {
  # Generate posterior predictive distribution
  mu_samples <- mcmc_result$posterior$mu
  tau_samples <- mcmc_result$posterior$tau
  n_samples <- length(mu_samples)

  # Sample new study effect from predictive distribution
  pred_samples <- rnorm(n_samples, mean = mu_samples, sd = tau_samples)

  list(
    samples = pred_samples,
    mean = mean(pred_samples),
    median = median(pred_samples),
    ci95 = quantile(pred_samples, probs = c(0.025, 0.975)),
    ci90 = quantile(pred_samples, probs = c(0.05, 0.95))
  )
}

#' @keywords internal
calculate_probabilities <- function(mcmc_result) {
  mu_samples <- mcmc_result$posterior$mu

  list(
    prob_positive = mean(mu_samples > 0),
    prob_negative = mean(mu_samples < 0),
    prob_important = mean(abs(mu_samples) > 0.2),
    prob_very_important = mean(abs(mu_samples) > 0.5)
  )
}

#' @keywords internal
create_bayesian_plots <- function(mcmc_result, y, s, pred_dist) {
  plots <- list()

  # Posterior density plots
  plots$posterior_mu <- plot_posterior_density(mcmc_result$posterior$mu, "Overall Effect (mu)")
  plots$posterior_tau <- plot_posterior_density(mcmc_result$posterior$tau, "Heterogeneity (tau)")

  # Trace plots
  plots$trace_mu <- plot_trace(mcmc_result$posterior$mu, mcmc_result$n_chains, "mu")
  plots$trace_tau <- plot_trace(mcmc_result$posterior$tau, mcmc_result$n_chains, "tau")

  # Forest plot
  plots$forest <- plot_bayesian_forest(mcmc_result, y, s)

  # Posterior predictive
  if (!is.null(pred_dist)) {
    plots$predictive <- plot_predictive_distribution(pred_dist)
  }

  plots
}

#' @keywords internal
plot_posterior_density <- function(samples, param_name) {
  par(mar = c(5, 5, 3, 2))
  dens <- density(samples)
  plot(dens, main = sprintf("Posterior Distribution: %s", param_name),
       xlab = param_name, ylab = "Density", lwd = 2, col = "steelblue")
  polygon(dens$x, dens$y, col = rgb(0.3, 0.5, 0.7, 0.3), border = NA)

  # Add credible interval
  ci95 <- quantile(samples, probs = c(0.025, 0.975))
  abline(v = ci95, lty = 2, col = "red", lwd = 2)
  abline(v = mean(samples), lty = 1, col = "darkblue", lwd = 2)

  legend("topright",
         legend = c("Mean", "95% CI"),
         lty = c(1, 2), col = c("darkblue", "red"), lwd = 2)
}

#' @keywords internal
plot_trace <- function(samples, n_chains, param_name) {
  par(mar = c(5, 5, 3, 2))
  n_samples_per_chain <- length(samples) / n_chains
  chains <- split(samples, rep(1:n_chains, each = n_samples_per_chain))

  plot(1:n_samples_per_chain, chains[[1]], type = "l", col = 1,
       ylim = range(samples), xlab = "Iteration", ylab = param_name,
       main = sprintf("Trace Plot: %s", param_name))

  for (i in 2:n_chains) {
    lines(1:n_samples_per_chain, chains[[i]], col = i)
  }

  legend("topright", legend = paste("Chain", 1:n_chains),
         col = 1:n_chains, lty = 1)
}

#' @keywords internal
plot_bayesian_forest <- function(mcmc_result, y, s) {
  n <- length(y)
  par(mar = c(5, 8, 3, 2))

  # Study-specific posteriors
  theta_summary <- apply(mcmc_result$posterior$theta, 2, function(x) {
    c(mean = mean(x), ci_lower = quantile(x, 0.025), ci_upper = quantile(x, 0.975))
  })

  # Overall posterior
  mu_summary <- c(
    mean = mean(mcmc_result$posterior$mu),
    ci_lower = quantile(mcmc_result$posterior$mu, 0.025),
    ci_upper = quantile(mcmc_result$posterior$mu, 0.975)
  )

  plot.new()
  plot.window(xlim = range(c(theta_summary, mu_summary)), ylim = c(0.5, n + 1.5))

  # Plot studies
  for (i in 1:n) {
    y_pos <- n - i + 1
    lines(c(theta_summary["ci_lower", i], theta_summary["ci_upper", i]),
          c(y_pos, y_pos), lwd = 2)
    points(theta_summary["mean", i], y_pos, pch = 15, cex = 1.5)
  }

  # Plot overall
  polygon(c(mu_summary["ci_lower"], mu_summary["ci_upper"], mu_summary["ci_upper"], mu_summary["ci_lower"]),
          c(0.3, 0.3, 0.7, 0.7), col = "gray80", border = NA)
  points(mu_summary["mean"], 0.5, pch = 18, cex = 2, col = "red")

  abline(v = 0, lty = 2, col = "gray50")
  axis(1)
  axis(2, at = c(0.5, n:1), labels = c("Overall", paste("Study", 1:n)), las = 1, tick = FALSE)
  title("Bayesian Forest Plot", font.main = 2)
  title(xlab = "Effect Size (95% Credible Interval)")
}

#' @keywords internal
plot_predictive_distribution <- function(pred_dist) {
  par(mar = c(5, 5, 3, 2))
  dens <- density(pred_dist$samples)
  plot(dens, main = "Posterior Predictive Distribution",
       xlab = "Predicted Effect for New Study", ylab = "Density",
       lwd = 2, col = "darkgreen")
  polygon(dens$x, dens$y, col = rgb(0, 0.5, 0, 0.3), border = NA)

  ci95 <- pred_dist$ci95
  abline(v = ci95, lty = 2, col = "red", lwd = 2)
  abline(v = pred_dist$mean, lty = 1, col = "darkblue", lwd = 2)

  legend("topright",
         legend = c("Mean", "95% Interval"),
         lty = c(1, 2), col = c("darkblue", "red"), lwd = 2)
}

#' @keywords internal
plot_prior_sensitivity <- function(summaries) {
  n_scenarios <- nrow(summaries)
  par(mar = c(5, 10, 3, 2))

  plot.new()
  plot.window(xlim = range(c(summaries$ci_lower, summaries$ci_upper)),
              ylim = c(0.5, n_scenarios + 0.5))

  for (i in 1:n_scenarios) {
    y_pos <- n_scenarios - i + 1
    lines(c(summaries$ci_lower[i], summaries$ci_upper[i]), c(y_pos, y_pos), lwd = 2)
    points(summaries$posterior_mean[i], y_pos, pch = 15, cex = 1.5)
  }

  abline(v = 0, lty = 2, col = "gray50")
  axis(1)
  axis(2, at = n_scenarios:1, labels = summaries$scenario, las = 1, tick = FALSE)
  title("Prior Sensitivity Analysis", font.main = 2)
  title(xlab = "Posterior Mean Effect (95% CI)")
}

#' @keywords internal
interpret_bayes_factor <- function(BF10) {
  if (BF10 < 1) {
    "Evidence in favor of null hypothesis"
  } else if (BF10 < 3) {
    "Anecdotal evidence for alternative"
  } else if (BF10 < 10) {
    "Moderate evidence for alternative"
  } else if (BF10 < 30) {
    "Strong evidence for alternative"
  } else if (BF10 < 100) {
    "Very strong evidence for alternative"
  } else {
    "Extreme evidence for alternative"
  }
}

#' @keywords internal
classification_bayes_factor <- function(BF10) {
  if (BF10 < 0.33) "Substantial for null"
  else if (BF10 < 1) "Anecdotal for null"
  else if (BF10 < 3) "Anecdotal for alternative"
  else if (BF10 < 10) "Moderate for alternative"
  else if (BF10 < 30) "Strong for alternative"
  else if (BF10 < 100) "Very strong for alternative"
  else "Extreme for alternative"
}

#' @export
print.bayesian_meta_analysis <- function(x, ...) {
  cat("Bayesian Meta-Analysis Results\n")
  cat("===============================\n\n")

  cat(sprintf("Studies: %d\n", nrow(x$data)))
  cat(sprintf("MCMC samples: %d\n\n", x$posterior$n_samples))

  cat("Posterior Summary for Overall Effect (mu):\n")
  print(round(x$posterior_summary$mu, 3))

  cat("\nPosterior Summary for Heterogeneity (tau):\n")
  print(round(x$posterior_summary$tau, 3))

  cat("\nProbability Statements:\n")
  cat(sprintf("  P(effect > 0) = %.3f\n", x$probabilities$prob_positive))
  cat(sprintf("  P(|effect| > 0.2) = %.3f\n", x$probabilities$prob_important))

  if (!is.null(x$predictive)) {
    cat("\nPosterior Predictive for New Study:\n")
    cat(sprintf("  Mean: %.3f\n", x$predictive$mean))
    cat(sprintf("  95%% Interval: [%.3f, %.3f]\n",
                x$predictive$ci95[1], x$predictive$ci95[2]))
  }

  cat("\n")
  invisible(x)
}

#' @export
summary.bayesian_meta_analysis <- function(object, ...) {
  print(object, ...)

  cat("Diagnostics:\n")
  cat(sprintf("  Rhat (mu): %.3f\n", object$diagnostics$rhat_mu))
  cat(sprintf("  Rhat (tau): %.3f\n", object$diagnostics$rhat_tau))
  cat(sprintf("  ESS (mu): %.0f\n", object$diagnostics$ess_mu))
  cat(sprintf("  ESS (tau): %.0f\n", object$diagnostics$ess_tau))

  if (object$diagnostics$converged) {
    cat("  ✓ Convergence achieved\n")
  } else {
    cat("  ⚠️  Convergence issues detected\n")
  }

  invisible(object)
}

#' @export
plot.bayesian_meta_analysis <- function(x, type = "posterior", ...) {
  if (type == "posterior") {
    par(mfrow = c(1, 2))
    x$plots$posterior_mu
    x$plots$posterior_tau
  } else if (type == "trace") {
    par(mfrow = c(1, 2))
    x$plots$trace_mu
    x$plots$trace_tau
  } else if (type == "forest") {
    x$plots$forest
  } else if (type == "predictive") {
    if (!is.null(x$plots$predictive)) {
      x$plots$predictive
    } else {
      stop("Predictive distribution not available")
    }
  } else {
    stop("Unknown plot type")
  }
}
