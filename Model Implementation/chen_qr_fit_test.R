# ============================================================================
# Chen quantile regression with regression structures for mu_t and lambda_t
#
#   log(mu_t)     = x_t^T beta
#   log(lambda_t) = z_t^T nu
#
# Parameter vector: gamma = (beta^T, nu^T)^T
# ============================================================================

# -----------------------------------------------------------------------------
# Random generation from the quantile-parameterized Chen distribution
# -----------------------------------------------------------------------------
rchen_qr <- function(mu, lambda, tau) {
  mu <- as.numeric(mu)
  lambda <- as.numeric(lambda)

  if (length(mu) != length(lambda)) {
    stop("mu and lambda must have the same length.")
  }
  if (any(mu <= 0) || any(lambda <= 0)) {
    stop("mu and lambda must be positive.")
  }
  if (tau <= 0 || tau >= 1) {
    stop("tau must be in (0,1).")
  }

  n <- length(mu)
  u <- runif(n)
  u <- pmin(pmax(u, .Machine$double.eps), 1 - .Machine$double.eps)

  # A_t = log(1-tau) / {1-exp(mu_t^lambda_t)} > 0
  A <- -log1p(-tau) / expm1(mu^lambda)

  # Inversion of the CDF
  inside <- 1 - log1p(-u) / A
  y <- log(inside)^(1 / lambda)

  return(y)
}

# -----------------------------------------------------------------------------
# CDF of the quantile-parameterized Chen distribution
# -----------------------------------------------------------------------------
pchen_qr <- function(y, mu, lambda, tau) {
  y <- as.numeric(y)
  mu <- as.numeric(mu)
  lambda <- as.numeric(lambda)

  A <- -log1p(-tau) / expm1(mu^lambda)
  expo <- -A * expm1(y^lambda)
  F <- -expm1(expo)

  # Avoid numerical problems in qnorm
  F <- pmin(pmax(F, .Machine$double.eps), 1 - .Machine$double.eps)
  return(F)
}

# -----------------------------------------------------------------------------
# Log-likelihood
# -----------------------------------------------------------------------------
chen_qr_loglik <- function(par, y, X, Z, tau) {
  p <- ncol(X)

  beta <- par[1:p]
  nu <- par[(p + 1):length(par)]

  eta_mu <- drop(X %*% beta)
  eta_lambda <- drop(Z %*% nu)

  mu <- exp(eta_mu)
  lambda <- exp(eta_lambda)

  if (any(!is.finite(mu)) || any(!is.finite(lambda)) ||
      any(mu <= 0) || any(lambda <= 0) || any(y <= 0)) {
    return(-Inf)
  }

  mu_lambda <- mu^lambda
  y_lambda <- y^lambda

  E_mu <- expm1(mu_lambda)
  E_y <- expm1(y_lambda)

  if (any(!is.finite(E_mu)) || any(!is.finite(E_y)) || any(E_mu <= 0)) {
    return(-Inf)
  }

  # Algebraically equivalent to the log-likelihood in the manuscript,
  # written with expm1/log1p for better numerical stability.
  ll <- log(-log1p(-tau)) - log(E_mu) +
    (lambda - 1) * log(y) + log(lambda) +
    log1p(-tau) * E_y / E_mu + y_lambda

  if (any(!is.finite(ll))) {
    return(-Inf)
  }

  sum(ll)
}

# -----------------------------------------------------------------------------
# Analytical score vector
# U_beta = X^T T b and U_nu = Z^T R c
# For log links: T = diag(mu_t), R = diag(lambda_t)
# -----------------------------------------------------------------------------
chen_qr_score <- function(par, y, X, Z, tau) {
  p <- ncol(X)

  beta <- par[1:p]
  nu <- par[(p + 1):length(par)]

  eta_mu <- drop(X %*% beta)
  eta_lambda <- drop(Z %*% nu)

  mu <- exp(eta_mu)
  lambda <- exp(eta_lambda)

  L <- log1p(-tau)

  exp_mu <- exp(mu^lambda)
  exp_y <- exp(y^lambda)
  den <- 1 - exp_mu

  b <- (lambda * exp_mu * mu^(lambda - 1)) / den +
    (lambda * L * exp_mu * mu^(lambda - 1) *
       (1 - exp_y)) / den^2

  c <- (mu^lambda * exp_mu * log(mu)) / den +
    1 / lambda +
    (L * exp_mu * mu^lambda * log(mu) *
       (1 - exp_y)) / den^2 +
    log(y) -
    (L * exp_y * y^lambda * log(y)) / den +
    y^lambda * log(y)

  U_beta <- drop(crossprod(X, mu * b))
  U_nu <- drop(crossprod(Z, lambda * c))

  score <- c(U_beta, U_nu)

  if (any(!is.finite(score))) {
    score[] <- NA_real_
  }

  return(score)
}

# -----------------------------------------------------------------------------
# Model fitting
# -----------------------------------------------------------------------------
chen_qr_fit <- function(y, X, Z, tau,
                        start = NULL,
                        method = "BFGS",
                        use_score = TRUE,
                        maxit = 1000,
                        reltol = 1e-10) {

  y <- as.numeric(y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)

  n <- length(y)
  p <- ncol(X)
  q <- ncol(Z)

  if (nrow(X) != n || nrow(Z) != n) {
    stop("X and Z must have the same number of rows as length(y).")
  }
  if (any(y <= 0) || any(!is.finite(y))) {
    stop("All observations must be positive and finite.")
  }

  # Parameter names
  beta_names <- if (!is.null(colnames(X))) {
    paste0("beta_", colnames(X))
  } else {
    paste0("beta", 0:(p - 1))
  }

  nu_names <- if (!is.null(colnames(Z))) {
    paste0("nu_", colnames(Z))
  } else {
    paste0("nu", 0:(q - 1))
  }

  par_names <- c(beta_names, nu_names)

  # Initial values
  if (is.null(start)) {
    fit_start <- try(lm.fit(X, log(y)), silent = TRUE)

    if (!inherits(fit_start, "try-error")) {
      beta_start <- as.numeric(fit_start$coefficients)
      beta_start[!is.finite(beta_start)] <- 0
    } else {
      beta_start <- rep(0, p)
    }

    # Neutral initial regression for shape; lambda around exp(-0.5) = 0.61
    nu_start <- rep(0, q)
    nu_start[1] <- -0.5

    start <- c(beta_start, nu_start)
  }

  if (length(start) != (p + q)) {
    stop("start must have length ncol(X) + ncol(Z).")
  }

  fn <- function(par) chen_qr_loglik(par, y = y, X = X, Z = Z, tau = tau)
  gr <- function(par) chen_qr_score(par, y = y, X = X, Z = Z, tau = tau)

  opt <- try(
    optim(
      par = start,
      fn = fn,
      gr = if (use_score && method == "BFGS") gr else NULL,
      method = method,
      hessian = TRUE,
      control = list(
        fnscale = -1,
        maxit = maxit,
        reltol = reltol
      )
    ),
    silent = TRUE
  )

  if (inherits(opt, "try-error")) {
    return(list(
      convergence = 999,
      converged = FALSE,
      vcov_ok = FALSE,
      error = as.character(opt)
    ))
  }

  coef <- opt$par
  names(coef) <- par_names

  beta_hat <- coef[1:p]
  nu_hat <- coef[(p + 1):(p + q)]

  mu_hat <- exp(drop(X %*% beta_hat))
  lambda_hat <- exp(drop(Z %*% nu_hat))

  # Observed information and covariance matrix
  information <- -opt$hessian
  vcov <- matrix(NA_real_, p + q, p + q)
  vcov_ok <- FALSE

  if (all(is.finite(information))) {
    vcov_try <- try(chol2inv(chol(information)), silent = TRUE)
    if (!inherits(vcov_try, "try-error") && all(is.finite(vcov_try))) {
      vcov <- vcov_try
      vcov_ok <- TRUE
    }
  }

  dimnames(vcov) <- list(par_names, par_names)

  se <- rep(NA_real_, p + q)
  names(se) <- par_names
  zstat <- pvalue <- se

  if (vcov_ok) {
    se <- sqrt(diag(vcov))
    zstat <- coef / se
    pvalue <- 2 * (1 - pnorm(abs(zstat)))
  }

  fitted_cdf <- pchen_qr(y, mu_hat, lambda_hat, tau)
  residuals <- qnorm(fitted_cdf)

  k <- p + q
  loglik <- opt$value

  list(
    coefficients = coef,
    beta = beta_hat,
    nu = nu_hat,
    mu = mu_hat,
    lambda = lambda_hat,
    fitted = mu_hat,
    residuals = residuals,
    vcov = vcov,
    se = se,
    zstat = zstat,
    pvalue = pvalue,
    loglik = loglik,
    aic = -2 * loglik + 2 * k,
    bic = -2 * loglik + log(n) * k,
    convergence = opt$convergence,
    converged = (opt$convergence == 0) && all(is.finite(coef)),
    vcov_ok = vcov_ok,
    counts = opt$counts,
    method = method,
    tau = tau,
    X = X,
    Z = Z
  )
}
