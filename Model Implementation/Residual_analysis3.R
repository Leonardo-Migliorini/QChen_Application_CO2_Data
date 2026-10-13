library(ggplot2)

# ============================================================================
# Residual diagnostic plots for the quantile-parameterized Chen regression
#
# Model:
#   g_mu(mu_t)         = x_t' beta
#   g_lambda(lambda_t) = z_t' nu
#
# Requires rchen_qr() and chen_qr_fit() from chen_qr_fit_links.R.
# Residuals are normalized quantile residuals: qnorm(F(y_t; mu_t, lambda_t)).
# ============================================================================
residual_plots <- function(model,
                           sim = 100L,
                           conf = 0.95,
                           save_pdf = FALSE,
                           output_dir = ".",
                           seed = NULL) {

  # Model components from the current chen_qr_fit() return structure.
  required <- c("mu", "lambda", "fitted", "residuals", "X", "Z",
                "tau", "coefficients", "converged")
  missing <- setdiff(required, names(model))
  if (length(missing)) {
    stop("Missing model components: ", paste(missing, collapse = ", "))
  }
  if (!isTRUE(model$converged)) {
    stop("The supplied model did not converge; check the fit before diagnostics.")
  }

  mu <- as.numeric(model$mu)
  lambda <- as.numeric(model$lambda)
  fitted_values <- as.numeric(model$fitted)  # fitted tau-quantiles (= mu)
  res <- as.numeric(model$residuals)
  X <- as.matrix(model$X)
  Z <- as.matrix(model$Z)
  tau <- model$tau
  mu_link <- if (is.null(model$mu.link)) "log" else model$mu.link
  lambda_link <- if (is.null(model$lambda.link)) "log" else model$lambda.link
  n <- length(res)

  if (n < 3L || any(lengths(list(mu, lambda, fitted_values)) != n) ||
      nrow(X) != n || nrow(Z) != n) {
    stop("Incompatible observation counts in the model object.")
  }
  if (any(!is.finite(c(res, mu, lambda, fitted_values))) ||
      any(mu <= 0) || any(lambda <= 0)) {
    stop("Residuals or fitted parameters are invalid/non-finite.")
  }
  if (length(tau) != 1L || !is.finite(tau) || tau <= 0 || tau >= 1) {
    stop("tau must be between 0 and 1.")
  }
  if (length(conf) != 1L || !is.finite(conf) || conf <= 0 || conf >= 1) {
    stop("conf must be between 0 and 1.")
  }
  if (length(sim) != 1L || is.na(sim) || !is.finite(sim) ||
      sim < 0 || sim != as.integer(sim) || sim == 1L) {
    stop("sim must be 0 (no envelope) or an integer >= 2.")
  }
  sim <- as.integer(sim)

  if (!is.null(seed)) set.seed(seed)
  alpha <- (1 - conf) / 2

  # ===========================================================================
  # Parametric bootstrap envelope for NORMAL Q-Q plot.
  # Each simulated sample uses BOTH fitted distribution-parameter vectors.
  # Each simulated response is refitted with the original X and Z matrices.
  # ===========================================================================
  theoretical <- qnorm(ppoints(n))
  qq_df <- data.frame(theoretical = theoretical, observed = sort(res))
  envelope <- NULL
  n_outside <- NA_integer_
  proportion_outside <- NA_real_
  successes <- 0L
  attempts <- 0L

  if (sim > 0L) {
    if (!exists("rchen_qr", mode = "function") ||
        !exists("chen_qr_fit", mode = "function")) {
      stop("Source chen_qr_fit_links.R before calling residual_plots().")
    }

    simulated_residuals <- matrix(NA_real_, nrow = n, ncol = sim)
    max_attempts <- 3L * sim  # avoids infinite loops if refits fail
    fit_method <- if (is.null(model$method)) "BFGS" else model$method

    while (successes < sim && attempts < max_attempts) {
      attempts <- attempts + 1L

      y_sim <- try(rchen_qr(mu = mu, lambda = lambda, tau = tau),
                   silent = TRUE)
      if (inherits(y_sim, "try-error") || length(y_sim) != n ||
          any(!is.finite(y_sim)) || any(y_sim <= 0)) next

      fit_sim <- try(
        suppressWarnings(
          chen_qr_fit(
            y = y_sim,
            X = X,
            Z = Z,
            tau = tau,
            start = unname(model$coefficients),
            method = fit_method,
            diag = 0,
            mu.link = mu_link,
            lambda.link = lambda_link
          )
        ),
        silent = TRUE
      )

      if (inherits(fit_sim, "try-error") ||
          !isTRUE(fit_sim$converged) ||
          length(fit_sim$residuals) != n ||
          any(!is.finite(fit_sim$residuals))) next

      successes <- successes + 1L
      simulated_residuals[, successes] <- sort(fit_sim$residuals)
    }

    if (successes < sim) {
      warning(sprintf(
        "Only %d of %d bootstrap refits converged after %d attempts.",
        successes, sim, attempts
      ))
    }

    if (successes >= 2L) {
      limits <- t(apply(
        simulated_residuals[, seq_len(successes), drop = FALSE],
        1L, quantile, probs = c(alpha, 1 - alpha), names = FALSE
      ))
      envelope <- data.frame(
        theoretical = theoretical,
        lower = limits[, 1L],
        upper = limits[, 2L]
      )

      # Classify each ordered observed residual using its pointwise limits.
      qq_df$lower <- envelope$lower
      qq_df$upper <- envelope$upper
      qq_df$outside <- qq_df$observed < qq_df$lower |
        qq_df$observed > qq_df$upper
      qq_df$band_status <- factor(
        ifelse(qq_df$outside, "Outside envelope", "Within envelope"),
        levels = c("Within envelope", "Outside envelope")
      )
      n_outside <- sum(qq_df$outside)
      proportion_outside <- n_outside / n
    } else {
      warning("Insufficient successful refits to construct the Q-Q envelope.")
    }
  }

  # ===========================================================================
  # Common theme
  # ===========================================================================
  theme_residuals <- theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  # Plot 1: Residuals vs index
  p1 <- ggplot(
    data.frame(index = seq_len(n), res = res),
    aes(x = index, y = res)
  ) +
    geom_point(shape = 3, size = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = 3) +
    labs(x = "Index", y = "Quantile residuals") +
    coord_cartesian(ylim = c(-4.5, 4.5)) +
    theme_residuals

  # Plot 2: Empirical residual density vs standard normal density
  dens <- density(res, from = -5, to = 5, n = 512)
  dens_df <- data.frame(
    x = rep(dens$x, times = 2L),
    y = c(dens$y, dnorm(dens$x)),
    type = rep(c("Estimated density", "Standard normal density"),
               each = length(dens$x))
  )

  p2 <- ggplot(dens_df, aes(x = x, y = y, linetype = type)) +
    geom_line(linewidth = 0.8) +
    scale_linetype_manual(
      values = c("Estimated density" = "solid",
                 "Standard normal density" = "dashed"),
      name = NULL
    ) +
    labs(x = "Residuals", y = "Density") +
    coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(0, 0.5)) +
    theme_residuals +
    theme(legend.position = "top", legend.text = element_text(size = 8))

  # Plot 3: Residuals vs fitted tau-quantiles
  p3 <- ggplot(
    data.frame(fitted = fitted_values, res = res),
    aes(x = fitted, y = res)
  ) +
    geom_point(shape = 3, size = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = 3) +
    labs(x = "Fitted quantiles (mu)", y = "Quantile residuals") +
    coord_cartesian(ylim = c(-4.5, 4.5)) +
    theme_residuals

  # Plot 4: Normal Q-Q plot with pointwise parametric bootstrap envelope
  p4 <- ggplot(qq_df, aes(x = theoretical, y = observed))
  if (!is.null(envelope)) {
    p4 <- p4 +
      geom_ribbon(
        data = envelope,
        aes(x = theoretical, ymin = lower, ymax = upper),
        inherit.aes = FALSE,
        fill = "grey80", alpha = 0.7
      )
  }
  p4 <- p4 +
    geom_abline(intercept = 0, slope = 1, linetype = 2)

  if (!is.null(envelope)) {
    p4 <- p4 +
      geom_point(aes(color = band_status), size = 1.5) +
      scale_color_manual(
        values = c("Within envelope" = "black",
                   "Outside envelope" = "#D55E00"),
        drop = FALSE,
        name = NULL
      ) +
      labs(subtitle = sprintf(
        "Outside bands: %d/%d (%.1f%%)",
        n_outside, n, 100 * proportion_outside
      ), caption = sprintf("%.0f%% pointwise bootstrap envelope", 100 * conf))
  } else {
    p4 <- p4 + geom_point(size = 1.2)
  }

  p4 <- p4 +
    labs(x = "Theoretical normal quantiles", y = "Sample quantiles") +
    theme_residuals
  if (!is.null(envelope)) {
    p4 <- p4 + theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(size = 8)
    )
  }

  # ===========================================================================
  # Optional PDF export (one file per plot; same names as the original script)
  # ===========================================================================
  if (isTRUE(save_pdf)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    plots <- list(
      residual_vs_index = p1,
      density_plot = p2,
      residual_vs_fitted = p3,
      simulation_envelope = p4
    )
    for (plot_name in names(plots)) {
      ggsave(
        filename = file.path(output_dir, paste0(plot_name, ".pdf")),
        plot = plots[[plot_name]],
        width = 90, height = 65, units = "mm", device = "pdf"
      )
    }
  }

  return(list(
    residual_vs_index = p1,
    density_plot = p2,
    residual_vs_fitted = p3,
    simulation_envelope = p4,
    qq_data = qq_df,
    n_outside = n_outside,
    proportion_outside = proportion_outside,
    sim_success = successes,
    sim_attempts = attempts
  ))
}
