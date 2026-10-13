# ============================================================================
# Residual diagnostics for quantile-parameterized Chen regression
#
# Q-Q envelope: same method as the PLOTTED band in Residual_analysis.R:
#   qqplotr::geom_qq_band(bandType = "boot")
#
# This method is a PARAMETRIC NORMAL bootstrap of the residuals:
#  1. Estimate Normal(mean, sd) parameters from the observed residuals by MLE.
#  2. Simulate 'sim' samples of size n from that fitted Normal distribution.
#  3. Sort each simulated sample.
#  4. Compute pointwise 'conf' intervals of the order statistics.
#
# Unlike Residual_analysis3.R, this method DOES NOT simulate new responses
# from the Chen model and DOES NOT refit regression parameters.
# It is written explicitly so that the same limits can be used for plotting
# and for marking/counting residuals outside the bands.
#
# Expected model: output of chen_qr_fit(), including versions with mu.link
# and lambda.link. No model refitting is performed by this script.
# ============================================================================

library(ggplot2)

residual_plots <- function(model,
                           sim = 100L,
                           conf = 0.95,
                           save_pdf = FALSE,
                           output_dir = ".",
                           seed = NULL) {

  required <- c("residuals", "fitted")
  missing <- setdiff(required, names(model))
  if (length(missing) > 0L) {
    stop("Missing model components: ", paste(missing, collapse = ", "))
  }
  if (!is.null(model$converged) && !isTRUE(model$converged)) {
    stop("The model did not converge; check its fit before diagnostics.")
  }

  res <- as.numeric(model$residuals)
  fitted_values <- as.numeric(model$fitted)
  n <- length(res)

  if (n < 3L || length(fitted_values) != n ||
      any(!is.finite(res)) || any(!is.finite(fitted_values))) {
    stop("Fitted values and residuals must be finite vectors of equal length (n >= 3).")
  }
  if (length(conf) != 1L || !is.numeric(conf) ||
      !is.finite(conf) || conf <= 0 || conf >= 1) {
    stop("conf must lie strictly between 0 and 1.")
  }
  if (length(sim) != 1L || !is.numeric(sim) || !is.finite(sim) ||
      sim != as.integer(sim) || !(sim == 0L || sim >= 2L)) {
    stop("sim must be 0 (no envelope) or an integer >= 2.")
  }
  sim <- as.integer(sim)
  if (!is.null(seed)) set.seed(seed)

  # Normal QQ coordinates (same plotting positions as qqplotr for 'norm').
  theoretical <- stats::qnorm(stats::ppoints(n))
  observed <- sort(res)
  qq_df <- data.frame(theoretical = theoretical, observed = observed)

  envelope <- NULL
  n_outside <- NA_integer_
  proportion_outside <- NA_real_

  # --------------------------------------------------------------------------
  # NORMAL PARAMETRIC BOOTSTRAP: reproduces qqplotr bandType = "boot"
  # when distribution = "norm" and dparams are estimated from the residuals.
  # MASS::fitdistr(..., "normal") uses mean(res) and the MLE of sd,
  # sqrt(sum((res - mean(res))^2) / n), not stats::sd(res) (n-1 divisor).
  # --------------------------------------------------------------------------
  normal_mean <- mean(res)
  normal_sd_mle <- sqrt(mean((res - normal_mean)^2))
  if (!is.finite(normal_sd_mle) || normal_sd_mle <= 0) {
    stop("Residual variability is zero; cannot construct the Normal Q-Q envelope.")
  }

  if (sim > 0L) {
    simulated <- matrix(
      stats::rnorm(n * sim, mean = normal_mean, sd = normal_sd_mle),
      nrow = n, ncol = sim
    )
    sorted_sim <- apply(simulated, 2L, sort)
    alpha <- (1 - conf) / 2
    limits <- t(apply(
      sorted_sim, 1L, stats::quantile,
      probs = c(alpha, 1 - alpha), names = FALSE
    ))

    envelope <- data.frame(
      theoretical = theoretical,
      lower = limits[, 1L],
      upper = limits[, 2L]
    )

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
  }

  # --------------------------------------------------------------------------
  # Common theme
  # --------------------------------------------------------------------------
  theme_residuals <- theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 9)
    )

  # 1: Residuals vs observation index
  p1 <- ggplot(
    data.frame(index = seq_len(n), res = res), aes(x = index, y = res)
  ) +
    geom_point(shape = 3, size = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = 3) +
    labs(x = "Index", y = "Quantile residuals") +
    coord_cartesian(ylim = c(-4.5, 4.5)) +
    theme_residuals

  # 2: Residual density and standard N(0, 1) reference
  dens <- stats::density(res, from = -5, to = 5, n = 512)
  density_df <- data.frame(
    x = rep(dens$x, times = 2L),
    y = c(dens$y, stats::dnorm(dens$x)),
    type = rep(c("Estimated density", "Standard normal density"),
               each = length(dens$x))
  )
  p2 <- ggplot(density_df, aes(x = x, y = y, linetype = type)) +
    geom_line(linewidth = 0.8) +
    scale_linetype_manual(
      values = c("Estimated density" = "solid",
                 "Standard normal density" = "dashed"), name = NULL
    ) +
    labs(x = "Residuals", y = "Density") +
    coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(0, 0.5)) +
    theme_residuals +
    theme(legend.position = "top", legend.text = element_text(size = 8))

  # 3: Residuals vs fitted conditional tau-quantiles, not conditional means
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

  # 4: Normal QQ with original-method bootstrap bands
  p4 <- ggplot(qq_df, aes(x = theoretical, y = observed))
  if (!is.null(envelope)) {
    p4 <- p4 +
      geom_ribbon(
        data = envelope,
        aes(x = theoretical, ymin = lower, ymax = upper),
        inherit.aes = FALSE,
        fill = "grey80", colour = "grey50", alpha = 0.5
      ) +
      geom_point(aes(colour = band_status), size = 1.6) +
      scale_colour_manual(
        values = c("Within envelope" = "black",
                   "Outside envelope" = "#D55E00"),
        drop = FALSE, name = NULL
      ) +
      labs(
        subtitle = sprintf("Outside bands: %d/%d (%.1f%%)",
                           n_outside, n, 100 * proportion_outside),
        caption = sprintf("%.0f%% pointwise Normal bootstrap envelope (B = %d)",
                          100 * conf, sim)
      ) +
      theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 8)
      )
  } else {
    p4 <- p4 + geom_point(size = 1.2)
  }
  p4 <- p4 +
    labs(x = "Theoretical normal quantiles", y = "Sample quantiles") +
    theme_residuals

  # --------------------------------------------------------------------------
  # Optional PDF export
  # --------------------------------------------------------------------------
  if (isTRUE(save_pdf)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    plots <- list(
      residual_vs_index = p1,
      density_plot = p2,
      residual_vs_fitted = p3,
      simulation_envelope = p4
    )
    for (plot_name in names(plots)) {
      ggplot2::ggsave(
        filename = file.path(output_dir, paste0(plot_name, ".pdf")),
        plot = plots[[plot_name]],
        width = 90, height = 65, units = "mm", device = "pdf"
      )
    }
  }

  list(
    residual_vs_index = p1,
    density_plot = p2,
    residual_vs_fitted = p3,
    simulation_envelope = p4,
    qq_data = qq_df,
    envelope = envelope,
    n_outside = n_outside,
    proportion_outside = proportion_outside,
    simulated_samples = sim,
    method = "normal_parametric_bootstrap",
    normal_mean = normal_mean,
    normal_sd_mle = normal_sd_mle
  )
}
