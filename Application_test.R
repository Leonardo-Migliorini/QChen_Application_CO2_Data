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
# =============================================================================

source("Model Implementation\\chen_functions.R")
# source("Model Implementation\\chen_qr_fit2.R")
source("Model Implementation\\chen_qr_fit3.R")
# source("Model Implementation\\Residual_analysis2.R")
source("Model Implementation\\Residual_analysis4.R")

# ==============================================================================
# Import data
# ==============================================================================

data <- read.csv("data_2018.csv")

# ==============================================================================
# Data exploration and descriptive statistics
# ==============================================================================

# Descriptive statistics of the response variable
summary(data$CO2)
var(data$CO2)
sd(data$CO2)
X <- as.matrix(rep(1, dim(data)[1]))

# Histogram of the response variable + Chen distribution fit for median (tau = 0.5)

# Estimation process
theta <- c(mu = median(data$CO2), lambda = 0.4)
maxiB <- optim(
  par = theta,
  fn = llchen,
  y = data$CO2,
  control = list(fnscale = -1)
)

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
    x = "Carbon dioxide emission (t/capita)",
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
#     width = 90,
#     height = 65,
#     units = "mm",
#     device = cairo_pdf
# )

# ==============================================================================
# Main model fitting
# ==============================================================================

colnames(data)

y <- data$CO2
X0 <- as.matrix(rep(1, dim(data)[1]))

# 1st quartile model

X1 <- cbind(1, data$DGCC, data$DOCC, data$UCAC)
X2 <- cbind(1, data$OR)

# model_q25 <- chen_qr_fit(y = y, X = X1, Z = X0, tau = 0.25)
model_q25 <- chen_qr_fit(
  y = y,
  X = X1,
  Z = X0,
  tau = 0.25,
  lambda.link = "identity"
)

# Median model

X <- cbind(1, data$DGCC, data$DOCC, data$UCAC)
Z <- cbind(1, data$DOCC)

model_q50 <- chen_qr_fit(y = y, X = X, Z = Z, tau = 0.5)
model_q50 <- chen_qr_fit(
  y = y,
  X = X,
  Z = X0,
  tau = 0.5,
  lambda.link = "identity"
)

# 3rd quartile model

X <- cbind(1, data$DGCC, data$DOCC, data$UCAC)
# Z <- cbind(1, data$OR)
Z <- cbind(1, data$DOCC)

model_q75 <- chen_qr_fit(y = y, X = X, Z = Z, tau = 0.75)
model_q75 <- chen_qr_fit(
  y = y,
  X = X,
  Z = X0,
  tau = 0.75,
  lambda.link = "identity"
)

# ==============================================================================
# Residual diagnostics
# ==============================================================================

# Function to perform residual diagnostic tests for normality and independence
run_tests <- function(model) {
  shapiro_result <- shapiro.test(model$residual) # Null hypothesis: the residuals are normally distributed
  adtest_result <- nortest::ad.test(model$residual) # Null hypothesis: the residuals are normally distributed

  results <- list(
    shapiro = shapiro_result,
    adtest = adtest_result
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
