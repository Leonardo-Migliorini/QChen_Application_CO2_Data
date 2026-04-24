library(ggplot2)
library(qqplotr)

# Residual diagnostic plots for Chen regression model using ggplot2
residual_plots <- function(
  model = fit.model,
  sim = 100,
  conf = 0.95,
  save_pdf = FALSE
) {
  alfa <- (1 - conf) / 2
  lambda <- model$lambda
  X <- model$X
  yfitted <- model$fitted
  eta <- model$etahat
  y <- model$serie
  n <- length(y)
  tau <- model$tau
  res <- model$resid

  # Simulation envelope calculations
  e <- matrix(0, n, sim)
  e1 <- numeric(n)
  e2 <- numeric(n)

  i <- 1
  while (i <= sim) {
    md <- exp(eta)
    ynew <- rchen(n, md, lambda, tau)
    fit <- try(chen_reg.fit(ynew, X, diag = 0), silent = TRUE)

    if (!inherits(fit, "try-error") && fit$conv == 0) {
      e[, i] <- sort(abs(fit$resid))
      i <- i + 1
    }
  }

  for (i in seq_len(n)) {
    eo <- sort(e[i, ])
    e1[i] <- quantile(eo, alfa)
    e2[i] <- quantile(eo, 1 - alfa)
  }

  # Common theme
  theme_residuals <- theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    )

  # Plot 1: Residuals vs Index
  p1 <- ggplot(
    data.frame(index = seq_len(n), res = res),
    aes(x = index, y = res)
  ) +
    geom_point(shape = 3, size = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = 3) +
    labs(x = "Index", y = "Residuals") +
    coord_cartesian(ylim = c(-4.5, 4.5)) +
    theme_residuals

  # Plot 2: Density plot of residuals

  dens <- density(res, from = -5, to = 5, n = 512)

  dens_df <- data.frame(
    x = dens$x,
    y = dens$y
  )

  p2 <- ggplot(dens_df, aes(x = x, y = y)) +
    geom_line(linewidth = 0.8) +
    stat_function(
      fun = dnorm,
      args = list(mean = 0, sd = 1),
      linetype = 2,
      linewidth = 0.8,
      xlim = c(-5, 5)
    ) +
    labs(x = "Range", y = "Density") +
    coord_cartesian(xlim = c(-4.5, 4.5)) +
    theme_residuals

  # Plot 3: Residuals vs Fitted
  p3 <- ggplot(
    data.frame(fitted = yfitted, res = res),
    aes(x = fitted, y = res)
  ) +
    geom_point(shape = 3, size = 2) +
    geom_hline(yintercept = c(-3, 3), linetype = 2) +
    geom_hline(yintercept = c(-2, 2), linetype = 3) +
    labs(x = "Fitted Values", y = "Residuals") +
    coord_cartesian(ylim = c(-4.5, 4.5)) +
    theme_residuals

  # Plot 4: QQ plot with simulation envelope
  p4 <- ggplot(data.frame(res = res), aes(sample = res)) +
    qqplotr::geom_qq_band(
      alpha = 0.5,
      fill = "white",
      color = "black",
      B = 100,
      bandType = "boot"
    ) +
    qqplotr::stat_qq_point(size = 1.2) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_residuals

  # Save plots if requested
  if (save_pdf) {
    ggsave("residual_vs_index.pdf", p1, width = 4, height = 4)
    ggsave("density_plot.pdf", p2, width = 4, height = 4)
    ggsave("residual_vs_fitted.pdf", p3, width = 4, height = 4)
    ggsave("simulation_envelope.pdf", p4, width = 4, height = 4)
  }

  return(list(
    residual_vs_index = p1,
    density_plot = p2,
    residual_vs_fitted = p3,
    simulation_envelope = p4
  ))
}
