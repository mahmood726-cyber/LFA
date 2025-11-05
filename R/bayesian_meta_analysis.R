#' Bayesian Meta-Analysis
#'
#' Bayesian approaches to meta-analysis using MCMC sampling (simplified implementation).
#' For full Bayesian analysis, consider using specialized packages like brms or rstan.
#'
#' @name bayesian_meta_analysis
NULL

#' Bayesian Random-Effects Meta-Analysis
#'
#' Performs Bayesian random-effects meta-analysis using a simplified Metropolis-Hastings
#' MCMC sampler. Provides posterior distributions for effect size and between-study variance.
#'
#' @param data Data frame with columns: study, effect, se
#' @param n_iter Number of MCMC iterations (default: 10000)
#' @param n_burn Burn-in period (default: 2000)
#' @param n_thin Thinning interval (default: 2)
#' @param prior_mean Prior mean for effect size (default: 0)
#' @param prior_sd Prior SD for effect size (default: 10)
#' @param prior_tau Prior for between-study SD: list(shape, scale) for half-Cauchy (default: list(0, 1))
#' @param seed Random seed for reproducibility
#'
#' @return Object of class "bayesian_ma" containing:
#'   \itemize{
#'     \item posterior_samples: Matrix of posterior samples
#'     \item summary: Posterior summary statistics
#'     \item diagnostics: MCMC diagnostics (acceptance rate, ESS)
#'     \item prior: Prior specifications
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' bayes_result <- bayesian_ma(example_meta, n_iter = 5000, seed = 123)
#' print(bayes_result)
#' plot(bayes_result)
#' }
bayesian_ma <- function(data,
                        n_iter = 10000,
                        n_burn = 2000,
                        n_thin = 2,
                        prior_mean = 0,
                        prior_sd = 10,
                        prior_tau = list(shape = 0, scale = 1),
                        seed = NULL) {
  # Validate data
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  required_cols <- c("study", "effect", "se")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  k <- nrow(data)
  yi <- data$effect
  sei <- data$se
  vi <- sei^2

  # Initialize parameters
  theta <- mean(yi)  # Overall effect
  tau <- sd(yi)      # Between-study SD

  # Storage for samples
  n_samples <- ceiling((n_iter - n_burn) / n_thin)
  posterior_samples <- matrix(NA, nrow = n_samples, ncol = 2)
  colnames(posterior_samples) <- c("theta", "tau")

  # MCMC sampling
  accept_theta <- 0
  accept_tau <- 0
  sample_idx <- 1

  # Proposal standard deviations (tuning parameters)
  prop_sd_theta <- 0.1
  prop_sd_tau <- 0.05

  for (iter in 1:n_iter) {
    # Update theta (overall effect)
    theta_prop <- theta + rnorm(1, 0, prop_sd_theta)

    # Log posterior for current theta
    log_post_current <- sum(dnorm(yi, mean = theta, sd = sqrt(vi + tau^2), log = TRUE)) +
                        dnorm(theta, mean = prior_mean, sd = prior_sd, log = TRUE)

    # Log posterior for proposed theta
    log_post_prop <- sum(dnorm(yi, mean = theta_prop, sd = sqrt(vi + tau^2), log = TRUE)) +
                     dnorm(theta_prop, mean = prior_mean, sd = prior_sd, log = TRUE)

    # Accept/reject
    log_alpha <- log_post_prop - log_post_current
    if (log(runif(1)) < log_alpha) {
      theta <- theta_prop
      if (iter > n_burn) accept_theta <- accept_theta + 1
    }

    # Update tau (between-study SD)
    # Use log scale for positivity constraint
    log_tau <- log(tau)
    log_tau_prop <- log_tau + rnorm(1, 0, prop_sd_tau)
    tau_prop <- exp(log_tau_prop)

    # Log posterior for current tau (with Jacobian adjustment)
    log_post_current <- sum(dnorm(yi, mean = theta, sd = sqrt(vi + tau^2), log = TRUE)) +
                        dcauchy(tau, location = 0, scale = prior_tau$scale, log = TRUE) +
                        log(tau)  # Jacobian for log transformation

    # Log posterior for proposed tau
    log_post_prop <- sum(dnorm(yi, mean = theta, sd = sqrt(vi + tau_prop^2), log = TRUE)) +
                     dcauchy(tau_prop, location = 0, scale = prior_tau$scale, log = TRUE) +
                     log(tau_prop)  # Jacobian

    # Accept/reject
    log_alpha <- log_post_prop - log_post_current
    if (log(runif(1)) < log_alpha) {
      tau <- tau_prop
      if (iter > n_burn) accept_tau <- accept_tau + 1
    }

    # Store samples after burn-in and thinning
    if (iter > n_burn && (iter - n_burn) %% n_thin == 0) {
      posterior_samples[sample_idx, ] <- c(theta, tau)
      sample_idx <- sample_idx + 1
    }
  }

  # Calculate summary statistics
  summary_stats <- data.frame(
    parameter = c("theta (effect)", "tau (heterogeneity SD)", "tau^2", "I^2 (%)"),
    mean = c(mean(posterior_samples[, "theta"]),
             mean(posterior_samples[, "tau"]),
             mean(posterior_samples[, "tau"]^2),
             NA),
    sd = c(sd(posterior_samples[, "theta"]),
           sd(posterior_samples[, "tau"]),
           sd(posterior_samples[, "tau"]^2),
           NA),
    median = c(median(posterior_samples[, "theta"]),
               median(posterior_samples[, "tau"]),
               median(posterior_samples[, "tau"]^2),
               NA),
    ci_lower = c(quantile(posterior_samples[, "theta"], 0.025),
                 quantile(posterior_samples[, "tau"], 0.025),
                 quantile(posterior_samples[, "tau"]^2, 0.025),
                 NA),
    ci_upper = c(quantile(posterior_samples[, "theta"], 0.975),
                 quantile(posterior_samples[, "tau"], 0.975),
                 quantile(posterior_samples[, "tau"]^2, 0.975),
                 NA),
    stringsAsFactors = FALSE
  )

  # Calculate I^2 (percentage of variation due to heterogeneity)
  # I^2 = tau^2 / (tau^2 + typical_vi)
  typical_vi <- median(vi)
  I2_samples <- 100 * posterior_samples[, "tau"]^2 / (posterior_samples[, "tau"]^2 + typical_vi)
  summary_stats[4, c("mean", "sd", "median", "ci_lower", "ci_upper")] <-
    c(mean(I2_samples), sd(I2_samples), median(I2_samples),
      quantile(I2_samples, 0.025), quantile(I2_samples, 0.975))

  # Diagnostics
  n_post_burn <- n_iter - n_burn
  diagnostics <- list(
    acceptance_rate_theta = accept_theta / n_post_burn,
    acceptance_rate_tau = accept_tau / n_post_burn,
    n_effective_theta = coda::effectiveSize(posterior_samples[, "theta"]),
    n_effective_tau = coda::effectiveSize(posterior_samples[, "tau"]),
    note = "Acceptance rates between 0.2-0.5 are generally good"
  )

  # Try to calculate effective sample size
  tryCatch({
    diagnostics$n_effective_theta <- coda::effectiveSize(posterior_samples[, "theta"])
    diagnostics$n_effective_tau <- coda::effectiveSize(posterior_samples[, "tau"])
  }, error = function(e) {
    # If coda not available, estimate roughly
    diagnostics$n_effective_theta <- nrow(posterior_samples) / 2
    diagnostics$n_effective_tau <- nrow(posterior_samples) / 2
  })

  result <- list(
    posterior_samples = posterior_samples,
    summary = summary_stats,
    diagnostics = diagnostics,
    prior = list(
      effect_mean = prior_mean,
      effect_sd = prior_sd,
      tau_prior = prior_tau
    ),
    settings = list(
      n_iter = n_iter,
      n_burn = n_burn,
      n_thin = n_thin,
      n_samples = nrow(posterior_samples)
    ),
    data = data
  )

  class(result) <- "bayesian_ma"
  return(result)
}


#' Bayesian Meta-Regression
#'
#' Performs Bayesian meta-regression with covariates.
#'
#' @param data Data frame with effect sizes, SEs, and covariates
#' @param formula Formula for regression (e.g., ~ year + quality)
#' @param n_iter Number of MCMC iterations
#' @param n_burn Burn-in period
#' @param seed Random seed
#'
#' @return Object of class "bayesian_metareg" with posterior samples
#'
#' @export
bayesian_metareg <- function(data, formula, n_iter = 10000, n_burn = 2000, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract design matrix
  mf <- model.frame(formula, data = data, na.action = na.pass)
  X <- model.matrix(formula, data = mf)
  p <- ncol(X)
  k <- nrow(data)

  yi <- data$effect
  sei <- data$se
  vi <- sei^2

  # Initialize
  beta <- rep(0, p)
  tau <- sd(yi)

  # Storage
  n_samples <- n_iter - n_burn
  posterior_samples <- matrix(NA, nrow = n_samples, ncol = p + 1)
  colnames(posterior_samples) <- c(colnames(X), "tau")

  sample_idx <- 1

  # MCMC
  for (iter in 1:n_iter) {
    # Update regression coefficients
    for (j in 1:p) {
      # Propose new value
      beta_prop <- beta
      beta_prop[j] <- beta[j] + rnorm(1, 0, 0.1)

      # Calculate fitted values
      mu_current <- X %*% beta
      mu_prop <- X %*% beta_prop

      # Log likelihood
      ll_current <- sum(dnorm(yi, mean = mu_current, sd = sqrt(vi + tau^2), log = TRUE))
      ll_prop <- sum(dnorm(yi, mean = mu_prop, sd = sqrt(vi + tau^2), log = TRUE))

      # Prior (flat)
      prior_current <- dnorm(beta[j], 0, 10, log = TRUE)
      prior_prop <- dnorm(beta_prop[j], 0, 10, log = TRUE)

      # Accept/reject
      log_alpha <- (ll_prop + prior_prop) - (ll_current + prior_current)
      if (log(runif(1)) < log_alpha) {
        beta <- beta_prop
      }
    }

    # Update tau
    log_tau <- log(tau)
    log_tau_prop <- log_tau + rnorm(1, 0, 0.05)
    tau_prop <- exp(log_tau_prop)

    mu <- X %*% beta
    ll_current <- sum(dnorm(yi, mean = mu, sd = sqrt(vi + tau^2), log = TRUE))
    ll_prop <- sum(dnorm(yi, mean = mu, sd = sqrt(vi + tau_prop^2), log = TRUE))

    prior_current <- dcauchy(tau, 0, 1, log = TRUE) + log(tau)
    prior_prop <- dcauchy(tau_prop, 0, 1, log = TRUE) + log(tau_prop)

    log_alpha <- (ll_prop + prior_prop) - (ll_current + prior_current)
    if (log(runif(1)) < log_alpha) {
      tau <- tau_prop
    }

    # Store
    if (iter > n_burn) {
      posterior_samples[sample_idx, ] <- c(beta, tau)
      sample_idx <- sample_idx + 1
    }
  }

  # Summary
  summary_stats <- data.frame(
    parameter = colnames(posterior_samples),
    mean = colMeans(posterior_samples),
    sd = apply(posterior_samples, 2, sd),
    ci_lower = apply(posterior_samples, 2, quantile, 0.025),
    ci_upper = apply(posterior_samples, 2, quantile, 0.975),
    stringsAsFactors = FALSE
  )

  result <- list(
    posterior_samples = posterior_samples,
    summary = summary_stats,
    formula = formula,
    data = data
  )

  class(result) <- "bayesian_metareg"
  return(result)
}


#' Prior Predictive Check
#'
#' Generates samples from the prior predictive distribution to assess
#' prior reasonableness.
#'
#' @param n_samples Number of samples to generate
#' @param prior_mean Prior mean for effect
#' @param prior_sd Prior SD for effect
#' @param prior_tau Prior for between-study SD
#'
#' @return Matrix of prior predictive samples
#'
#' @export
prior_predictive_check <- function(n_samples = 1000, prior_mean = 0,
                                    prior_sd = 10, prior_tau = list(scale = 1)) {
  # Sample from priors
  theta_samples <- rnorm(n_samples, prior_mean, prior_sd)
  tau_samples <- abs(rcauchy(n_samples, 0, prior_tau$scale))

  # For each, simulate a study
  study_effects <- rnorm(n_samples, theta_samples, tau_samples)

  prior_samples <- data.frame(
    theta = theta_samples,
    tau = tau_samples,
    study_effect = study_effects
  )

  return(prior_samples)
}


#' Posterior Predictive Check
#'
#' Generates posterior predictive samples to assess model fit.
#'
#' @param bayesian_result Object from bayesian_ma()
#' @param n_rep Number of replications per posterior sample
#'
#' @return Matrix of posterior predictive samples
#'
#' @export
posterior_predictive_check <- function(bayesian_result, n_rep = 1) {
  if (!inherits(bayesian_result, "bayesian_ma")) {
    stop("Input must be a bayesian_ma object")
  }

  posterior <- bayesian_result$posterior_samples
  n_samples <- nrow(posterior)
  k <- nrow(bayesian_result$data)

  # For each posterior sample, simulate new studies
  y_rep <- matrix(NA, nrow = n_samples * n_rep, ncol = k)

  idx <- 1
  for (i in 1:n_samples) {
    theta <- posterior[i, "theta"]
    tau <- posterior[i, "tau"]

    for (r in 1:n_rep) {
      # Simulate study-specific effects
      study_effects <- rnorm(k, theta, tau)
      # Add sampling error
      y_rep[idx, ] <- rnorm(k, study_effects, bayesian_result$data$se)
      idx <- idx + 1
    }
  }

  return(y_rep)
}


#' Bayes Factor for Meta-Analysis
#'
#' Computes approximate Bayes factor comparing null hypothesis (effect = 0)
#' to alternative hypothesis using Savage-Dickey ratio.
#'
#' @param bayesian_result Object from bayesian_ma()
#' @param null_value Value under null hypothesis (default: 0)
#'
#' @return List with Bayes factor and interpretation
#'
#' @export
bayes_factor_ma <- function(bayesian_result, null_value = 0) {
  if (!inherits(bayesian_result, "bayesian_ma")) {
    stop("Input must be a bayesian_ma object")
  }

  posterior <- bayesian_result$posterior_samples[, "theta"]

  # Density at null value
  # Posterior density
  post_density_at_null <- density(posterior, from = null_value, to = null_value, n = 1)$y

  # Prior density at null
  prior_mean <- bayesian_result$prior$effect_mean
  prior_sd <- bayesian_result$prior$effect_sd
  prior_density_at_null <- dnorm(null_value, prior_mean, prior_sd)

  # Savage-Dickey ratio
  BF10 <- prior_density_at_null / post_density_at_null

  # Interpretation (Kass & Raftery, 1995)
  interpretation <- if (BF10 < 1/100) "Decisive evidence for H0"
                   else if (BF10 < 1/10) "Strong evidence for H0"
                   else if (BF10 < 1/3) "Moderate evidence for H0"
                   else if (BF10 < 3) "Anecdotal evidence"
                   else if (BF10 < 10) "Moderate evidence for H1"
                   else if (BF10 < 100) "Strong evidence for H1"
                   else "Decisive evidence for H1"

  result <- list(
    BF10 = BF10,
    BF01 = 1 / BF10,
    interpretation = interpretation,
    null_value = null_value,
    note = "BF10: Evidence for alternative vs null. BF01: Evidence for null vs alternative."
  )

  return(result)
}
