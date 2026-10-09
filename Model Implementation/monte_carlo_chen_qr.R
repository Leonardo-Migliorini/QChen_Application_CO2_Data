# ============================================================================
# Monte Carlo simulation for the Chen quantile regression model
# with regression structures for both mu_t and lambda_t
# ============================================================================

source("chen_qr_fit.R")

# -----------------------------------------------------------------------------
# True parameter values
# -----------------------------------------------------------------------------
beta_true <- c(-0.5, 1.5, 0.5, 1.3)
nu_true   <- c(-0.5, 0.5, -0.3)

names(beta_true) <- c("beta0", "beta1", "beta2", "beta3")
names(nu_true)   <- c("nu0", "nu1", "nu2")

# -----------------------------------------------------------------------------
# Design matrices
# Covariates are generated once for each sample size and kept fixed over
# Monte Carlo replications, as described in the manuscript.
# -----------------------------------------------------------------------------
make_design <- function(n, seed) {
  set.seed(seed)

  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)

  z1 <- runif(n)
  z2 <- runif(n)

  X <- cbind("(Intercept)" = 1, x1 = x1, x2 = x2, x3 = x3)
  Z <- cbind("(Intercept)" = 1, z1 = z1, z2 = z2)

  list(X = X, Z = Z)
}

# -----------------------------------------------------------------------------
# Generate one sample from the proposed model
# -----------------------------------------------------------------------------
simu_chen_qr <- function(X, Z, beta, nu, tau) {
  mu <- exp(drop(X %*% beta))
  lambda <- exp(drop(Z %*% nu))

  y <- rchen_qr(mu = mu, lambda = lambda, tau = tau)

  list(
    y = y,
    mu = mu,
    lambda = lambda
  )
}

# -----------------------------------------------------------------------------
# Monte Carlo for one combination of n and tau
# -----------------------------------------------------------------------------
run_mc_scenario <- function(n,
                            tau,
                            B = 5000,
                            beta = beta_true,
                            nu = nu_true,
                            X,
                            Z,
                            seed = 123,
                            method = "BFGS") {

  true_par <- c(beta, nu)
  par_names <- names(true_par)
  p_total <- length(true_par)

  estimates <- matrix(
    NA_real_, nrow = B, ncol = p_total,
    dimnames = list(NULL, par_names)
  )

  ses <- matrix(
    NA_real_, nrow = B, ncol = p_total,
    dimnames = list(NULL, par_names)
  )

  converged <- rep(FALSE, B)
  vcov_ok <- rep(FALSE, B)

  set.seed(seed)

  for (b in seq_len(B)) {

    sim <- simu_chen_qr(
      X = X,
      Z = Z,
      beta = beta,
      nu = nu,
      tau = tau
    )

    fit <- try(
      chen_qr_fit(
        y = sim$y,
        X = X,
        Z = Z,
        tau = tau,
        method = method,
        use_score = (method == "BFGS")
      ),
      silent = TRUE
    )

    if (!inherits(fit, "try-error") && !is.null(fit$coefficients)) {
      converged[b] <- isTRUE(fit$converged)
      vcov_ok[b] <- isTRUE(fit$vcov_ok)

      if (converged[b]) {
        # chen_qr_fit names parameters using the X/Z column names.
        # Store by position to retain the simple beta0,...,nu2 names here.
        estimates[b, ] <- as.numeric(fit$coefficients)

        if (vcov_ok[b]) {
          ses[b, ] <- as.numeric(fit$se)
        }
      }
    }

    if (b %% 100 == 0) {
      cat(
        "n =", n,
        " tau =", tau,
        " replication =", b,
        " converged =", sum(converged[1:b]), "\n"
      )
    }
  }

  ok_est <- converged & apply(estimates, 1, function(x) all(is.finite(x)))
  ok_ci <- ok_est & vcov_ok & apply(ses, 1, function(x) all(is.finite(x) & x > 0))

  # Summary by parameter
  summary_list <- lapply(seq_along(true_par), function(j) {
    est_j <- estimates[ok_est, j]
    se_j <- ses[ok_ci, j]
    est_ci_j <- estimates[ok_ci, j]
    theta <- true_par[j]

    mean_est <- if (length(est_j) > 0) mean(est_j) else NA_real_
    bias <- if (length(est_j) > 0) mean(est_j - theta) else NA_real_
    rb <- if (length(est_j) > 0 && theta != 0) 100 * bias / theta else NA_real_
    sd_est <- if (length(est_j) > 1) sd(est_j) else NA_real_
    mse <- if (length(est_j) > 0) mean((est_j - theta)^2) else NA_real_

    coverage <- NA_real_
    if (length(est_ci_j) > 0) {
      lower <- est_ci_j - qnorm(0.975) * se_j
      upper <- est_ci_j + qnorm(0.975) * se_j
      coverage <- 100 * mean(lower <= theta & theta <= upper)
    }

    data.frame(
      n = n,
      tau = tau,
      parameter = par_names[j],
      true = theta,
      mean = mean_est,
      bias = bias,
      RB_percent = rb,
      SD = sd_est,
      MSE = mse,
      CR_percent = coverage,
      stringsAsFactors = FALSE
    )
  })

  parameter_summary <- do.call(rbind, summary_list)

  scenario_summary <- data.frame(
    n = n,
    tau = tau,
    replications = B,
    converged = sum(ok_est),
    convergence_percent = 100 * mean(ok_est),
    valid_information = sum(ok_ci),
    valid_information_percent = 100 * mean(ok_ci),
    stringsAsFactors = FALSE
  )

  list(
    parameter_summary = parameter_summary,
    scenario_summary = scenario_summary,
    estimates = estimates,
    ses = ses,
    converged = converged,
    vcov_ok = vcov_ok,
    X = X,
    Z = Z
  )
}

# =============================================================================
# COMPLETE MONTE CARLO DESIGN
# =============================================================================

B <- 5000
n_values <- c(50, 100, 500)
tau_values <- c(0.25, 0.50, 0.75)

# For a quick code check before the final run, use for example:
# B <- 100

all_parameter_results <- list()
all_scenario_results <- list()
all_details <- list()

counter <- 1

for (n in n_values) {

  # Same covariates are used for all replications and all tau values at this n.
  design <- make_design(n = n, seed = 1000 + n)

  for (tau in tau_values) {

    cat("\n============================================================\n")
    cat("Starting scenario: n =", n, " tau =", tau, "\n")
    cat("============================================================\n")

    ans <- run_mc_scenario(
      n = n,
      tau = tau,
      B = B,
      beta = beta_true,
      nu = nu_true,
      X = design$X,
      Z = design$Z,
      seed = 50000 + 100 * n + round(100 * tau),
      method = "BFGS"
    )

    all_parameter_results[[counter]] <- ans$parameter_summary
    all_scenario_results[[counter]] <- ans$scenario_summary
    all_details[[paste0("n", n, "_tau", tau)]] <- ans

    counter <- counter + 1
  }
}

mc_parameters <- do.call(rbind, all_parameter_results)
mc_scenarios <- do.call(rbind, all_scenario_results)

# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------
write.csv(mc_parameters, "MC_ChenQR_parameters.csv", row.names = FALSE)
write.csv(mc_scenarios, "MC_ChenQR_convergence.csv", row.names = FALSE)
saveRDS(all_details, "MC_ChenQR_details.rds")

cat("\nParameter results:\n")
print(mc_parameters)

cat("\nConvergence results:\n")
print(mc_scenarios)

# Optional LaTeX output if xtable is installed
if (requireNamespace("xtable", quietly = TRUE)) {
  latex_table <- xtable::xtable(mc_parameters, digits = 4)
  print(latex_table, include.rownames = FALSE)
}
