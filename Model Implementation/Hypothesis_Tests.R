# Uncomment the following line to install the required packages if they are not already installed

# packages <- c("randtests", "trend", "Kendall", "tseries", "seastests")
# install.packages(packages)

# Auto
# correlation tests -------------------------------------------------------------

autocorr_test <- function(ts, lag = 20) {
  # Box-Pierce Test
  # H0: Non autocorrelation
  BP <- Box.test(ts, lag = lag, type = "Box-Pierce", fitdf = 0)

  # Ljung-Box Test
  # H0: Non autocorrelation
  LB <- Box.test(ts, lag = lag, type = "Ljung-Box", fitdf = 0)

  ## Summarizing results
  p_value <- c(
    BP$p.value,
    LB$p.value
  )

  conclusion <- ifelse(p_value >= 0.05, "Non autocorrelation", "autocorrelated")

  table <- data.frame(
    Tests = c(
      "Box-Pierce",
      "Ljung-Box"
    ),
    H0 = c(rep("Non autocorrelation", 2)),
    p_value = round(p_value, 4),
    Conclusion = c(conclusion)
  )

  list(
    Box_Pierce = BP,
    Ljung_Box = LB,
    table = table
  )
}

# Deterministic Trend tests ----------------------------------------------------

trend_test <- function(ts) {
  ## Performing Tests

  # Cox Stuart Test
  # H0: Non trend
  CS <- suppressWarnings(randtests::cox.stuart.test(ts, c("two.sided")))

  # Cox and Stuart Trend
  # H0: Non trend
  CeST <- suppressWarnings(trend::cs.test(ts))

  # Mann-Kendall Trend
  # H0: i.i.d. time series / Non trend
  MannKT <- suppressWarnings(trend::mk.test(ts, continuity = TRUE))

  # Mann-Kendall
  # H0: Non trend
  MannK <- suppressWarnings(Kendall::MannKendall(ts))

  # KPSS Test for Trend
  # H0: Non trend
  KPSST <- suppressWarnings(tseries::kpss.test(ts, null = c("Trend")))

  ## Summarizing results
  p_value <- c(
    CS$p.value,
    CeST$p.value,
    MannKT$p.value,
    MannK$sl,
    KPSST$p.value
  )
  conclusion <- ifelse(p_value >= 0.05, "Non trend", "trend")

  table <- data.frame(
    Tests = c(
      "Cox Stuart",
      "Cox and Stuart Trend",
      "Mann-Kendall Trend",
      "Mann-Kendall",
      "KPSS Test for Trend"
    ),
    H0 = c(rep("Non trend", 5)),
    p_value = round(p_value, 4),
    Conclusion = c(conclusion)
  )
  list(
    CS = CS,
    CeST = CeST,
    MannKT = MannKT,
    MannK = MannK,
    KPSST = KPSST,
    table = table
  )
}

# Unit root tests --------------------------------------------------------------

unitroot_test <- function(ts) {
  ## Performing tests

  # Augmented Dickey-Fuller
  # H0: unit root
  ADF <- suppressWarnings(tseries::adf.test(ts, alternative = c("stationary")))

  # Phillips-Perron Unit Root
  # H0: unit root
  PP <- suppressWarnings(tseries::pp.test(ts, alternative = c("stationary")))

  # KPSS Test for Level
  # H0: Non trend
  KPSSL <- suppressWarnings(tseries::kpss.test(ts, null = c("Level")))

  ## Summarizing results
  p_value <- c(ADF$p.value, PP$p.value, KPSSL$p.value)
  conclusion <- ifelse(p_value[1:2] >= 0.05, "trend", "Non trend")
  conclusion[3] <- ifelse(p_value[3] >= 0.05, "Non trend", "trend")

  table <- data.frame(
    Tests = c(
      "Augmented Dickey-Fuller",
      "Phillips-Perron Unit Root",
      "KPSS Test for Level"
    ),
    H0 = c(rep("Trend", 2), "Non Trend"),
    p_valor = round(p_value, 4),
    Conclusao = conclusion
  )
  list(ADF = ADF, PP = PP, KPSSL = KPSSL, table = table)
}

# Seasonality tests ------------------------------------------------------------

seasonality_test <- function(ts, diff = 0, freq = 12) {
  ## Performing Tests

  # Kruskall Wallis
  # H0: Non Seasonality
  KrusW <- suppressWarnings(seastests::kw((ts), diff = diff, freq = freq))

  # Friedman rank
  # H0: Non Seasonality
  Fried <- suppressWarnings(seastests::fried((ts), diff = diff, freq = freq))

  ## Summarizing results
  p_value <- c(KrusW$Pval, Fried$Pval)
  conclusion <- ifelse(p_value >= 0.05, "Non Seasonality", "Seasonality")

  table <- data.frame(
    Tests = c("Kruskall Wallis", "Friedman rank"),
    H0 = c(rep("Non Seasonality", 2)),
    p_valor = round(p_value, 4),
    Conclusao = c(conclusion)
  )
  list(KrusW = KrusW, Fried = Fried, table = table)
}

# Testing functions

# set.seed(1981)
# rb <- ts(rnorm(200))
# plot(ts)

# autocorr_test(rb, lag = 20)
# trend_test(rb)
# unitroot_test(rb)
# seasonality_test(rb, diff = 0, freq = 12)
