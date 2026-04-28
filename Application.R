# ==============================================================================
# Install package dependencies (uncomment to install)
# ==============================================================================

# install.packages(c(
#   "ggplot2",
#   "qqplotr",
#   "nortest",
#   "moments"
# ))

# ==============================================================================
# Import Chen Regression Model dependencies
# ==============================================================================

source("Model Implementation\\Chen_functions.R")
source("Model Implementation\\Chen_reg_fit.R")
source("Model Implementation\\Residual_analysis.R")

# ==============================================================================
# Import data
# ==============================================================================

data <- read.csv("Data_2018\\data_2018.csv")

# ==============================================================================
# Data exploration and descriptive statistics
# ==============================================================================

# Descriptive statistics of the response variable
summary(data$CO2)
var(data$CO2)
sd(data$CO2)

# Histogram of the response variable + Chen distribution fit for median (tau = 0.5)

# Estimation process
theta <- c(mu = median(data$CO2), lambda = 0.4)
maxiB <- optim(theta, llchen, y = data$CO2, control = list(fnscale = -1))
x <- seq(0.001, max(data$CO2), by = 0.1)

fit_curve <- data.frame(
    x = x,
    density = dchen(x, lambda = maxiB$par[2], mu = maxiB$par[1], t = 0.5)
)
# Histogram plot + Chen distribution fit curve

histogram <- ggplot2::ggplot(data, ggplot2::aes(x = CO2)) +
    ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        binwidth = 3,
        boundary = 0,
        fill = "white",
        color = "black",
        linewidth = 0.4
    ) +
    ggplot2::geom_line(
        data = fit_curve,
        ggplot2::aes(x = x, y = density),
        color = "black",
        linewidth = 1
    ) +
    ggplot2::labs(
        x = "Carbon dioxide emission (t/pc)",
        y = "Estimated Density"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 40), ylim = c(0, 0.2)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10),
        panel.grid = ggplot2::element_blank()
    )

histogram

# Uncomment the line below to save the histogram plot
# ggplot2::ggsave(
#     filename = "histogram.pdf",
#     plot = histogram,
#     width = 6,
#     height = 4,
#     dpi = 300
# )

# ==============================================================================
# Main model fitting
# ==============================================================================

y <- data$CO2

X <- cbind(1, data$DGCC, data$DOCC, data$UCAC)

# 1st quartile model

model_q25 <- chen_reg.fit(y = y, X = X, tau = 0.25, link = "log", diag = 1)

# Median model

model_q50 <- chen_reg.fit(y = y, X = X, tau = 0.5, link = "log", diag = 1)

# 3rd quartile model

model_q75 <- chen_reg.fit(y = y, X = X, tau = 0.75, link = "log", diag = 1)

# ==============================================================================
# Residual diagnostics
# ==============================================================================

# Function to perform residual diagnostic tests for normality and independence
run_tests <- function(model) {
    shapiro_result <- shapiro.test(model$residual) # Null hypothesis: the residuals are normally distributed
    adtest_result <- nortest::ad.test(model$residual) # Null hypothesis: the residuals are normally distributed
    boxtest_result <- Box.test(model$residual) # Null hypothesis: the residuals are independent (no autocorrelation)

    results <- list(
        shapiro = shapiro_result,
        adtest = adtest_result,
        boxtest = boxtest_result
    )
    return(results)
}

# 1st quartile model
residual_plots(model_q25, save_pdf = FALSE)
run_tests(model_q25)

# Median model
residual_plots(model_q50, save_pdf = FALSE)
run_tests(model_q50)

# 3rd quartile model
residual_plots(model_q75, save_pdf = FALSE)
run_tests(model_q75)

# ==============================================================================
# Sensitivity analysis
# ==============================================================================

moments::skewness(model_q50$residual) # Skewness of the residuals
moments::kurtosis(model_q50$residual) # Kurtosis of the residuals

extreme_obs <- which.max(abs(model_q50$residual)) # Index of the observation with the highest absolute residual value

# The observation with the largest absolute residual (observation 82 - Mongolia) was identified for sensitivity analysis.

# Refit dataset after excluding observation 82
data_refit <- data[-extreme_obs, ]

y_refit <- data_refit$CO2
X_refit <- cbind(1, data_refit$DGCC, data_refit$DOCC, data_refit$UCAC)

# Refitting models without observation 82

# 1st quartile model

model_q25_refit <- chen_reg.fit(
    y = y_refit,
    X = X_refit,
    tau = 0.25,
    link = "log",
    diag = 1
)

# Median model

model_q50_refit <- chen_reg.fit(
    y = y_refit,
    X = X_refit,
    tau = 0.5,
    link = "log",
    diag = 1
)

# 3rd quartile model

model_q75_refit <- chen_reg.fit(
    y = y_refit,
    X = X_refit,
    tau = 0.75,
    link = "log",
    diag = 1
)

# ==============================================================================
# Comparison of parameter estimates from the full and refitted models
# ==============================================================================

# 1st quartile model
cbind(full = model_q25$coeff, refit = model_q25_refit$coeff)

# Median model
cbind(full = model_q50$coeff, refit = model_q50_refit$coeff)

# 3rd quartile model
cbind(full = model_q75$coeff, refit = model_q75_refit$coeff)

# ==============================================================================
# Diagnostics after removing observation 82
# ==============================================================================

# 1st quartile model
residual_plots(model_q25_refit, save_pdf = FALSE)
run_tests(model_q25_refit)

# Median model
residual_plots(model_q50_refit, save_pdf = FALSE)
run_tests(model_q50_refit)

# 3rd quartile model
residual_plots(model_q75_refit, save_pdf = FALSE)
run_tests(model_q75_refit)

# Excluding observation 82 improved the residual normality diagnostics,
# but did not materially affect parameter estimates or inferential conclusions.
# Therefore, the model fitted with the complete dataset was retained for final interpretation.
