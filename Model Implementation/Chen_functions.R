# Chen Probability Density Function

dchen <- function(y, lambda, mu, t) {
  ifelse(
    y > 0,
    log(1 - t) /
      (1 - exp(mu^lambda)) *
      lambda *
      y^(lambda - 1) *
      exp(log(1 - t) / (1 - exp(mu^lambda)) * (1 - exp(y^lambda)) + y^lambda),
    NA
  )
}

# Chen Distribution Function

pchen <- function(y, lambda, mu) {
  t = 0.5
  pr = 1 - exp(log(1 - t) / (1 - exp(mu^lambda)) * (1 - exp(y^lambda)))
  return(pr)
}

# Chen Quantile Function

qchen <- function(p, lambda, mu, tau) {
  q = (log(1 - (log(1 - p) / (log(1 - tau) / (1 - exp(mu^lambda)))))^(1 /
    lambda))
  return(q)
}


# Chen Random Number Generator Function

rchen <- function(n, mu, lambda, tau) {
  u = runif(n)
  y = (log(1 - (log(1 - u) / (log(1 - tau) / (1 - exp(mu^lambda))))))^(1 /
    lambda)
  return(y)
}

rchen(n = 100, mu = 1, lambda = 1, tau = 0.5)

# Log-Likelihood Function for the Chen Distribution
llchen <- function(y, theta) {
  mu = theta[1]
  lambda = theta[2]
  t = 0.5
  lt = log(log(1 - t) / (1 - exp(mu^lambda))) +
    (lambda - 1) * log(y) +
    log(lambda) +
    (log(1 - t) * (1 - exp(y^lambda)) / (1 - exp(mu^lambda))) +
    y^lambda
  result = sum(lt)
  return(result)
}
