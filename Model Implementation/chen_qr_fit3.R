# ============================================================================
# Chen quantile regression with regression structures for mu_t and lambda_t
#
#   g_mu(mu_t)         = x_t^T beta
#   g_lambda(lambda_t) = z_t^T nu
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
# Links for positive real-valued distribution parameters
# -----------------------------------------------------------------------------
# Supported links:
#   log       : g(v) = log(v)
#   identity  : g(v) = v
#   inverse   : g(v) = 1/v
#   sqrt      : g(v) = sqrt(v)
#   1/mu^2    : g(v) = 1/v^2 (also accepted: inverse_squared)
#
# For identity/inverse/sqrt/1/mu^2, the linear predictor must be > 0
# for every observation. Log is the only available unrestricted link.
chen_qr_make_link <- function(link = "log") {
  if (!is.character(link) || length(link) != 1L || is.na(link)) {
    stop("Link must be a single character string.")
  }

  if (link %in% c("inverse_squared", "inverse.square", "inverse^2")) {
    link <- "1/mu^2"
  }

  allowed <- c("log", "identity", "inverse", "sqrt", "1/mu^2")
  if (!link %in% allowed) {
    stop("Unsupported link: ", link, ". Choose: ", paste(allowed, collapse = ", "))
  }

  switch(link,
    "log" = list(
      name = link,
      linkfun = function(v) log(v),
      linkinv = function(eta) exp(eta),
      mu.eta = function(eta) exp(eta),
      valideta = function(eta) all(is.finite(eta))
    ),
    "identity" = list(
      name = link,
      linkfun = function(v) v,
      linkinv = function(eta) eta,
      mu.eta = function(eta) rep(1, length(eta)),
      valideta = function(eta) all(is.finite(eta) & eta > 0)
    ),
    "inverse" = list(
      name = link,
      linkfun = function(v) 1 / v,
      linkinv = function(eta) 1 / eta,
      mu.eta = function(eta) -1 / eta^2,
      valideta = function(eta) all(is.finite(eta) & eta > 0)
    ),
    "sqrt" = list(
      name = link,
      linkfun = function(v) sqrt(v),
      linkinv = function(eta) eta^2,
      mu.eta = function(eta) 2 * eta,
      valideta = function(eta) all(is.finite(eta) & eta > 0)
    ),
    "1/mu^2" = list(
      name = link,
      linkfun = function(v) 1 / v^2,
      linkinv = function(eta) 1 / sqrt(eta),
      mu.eta = function(eta) -0.5 * eta^(-3 / 2),
      valideta = function(eta) all(is.finite(eta) & eta > 0)
    )
  )
}

# Compute the distribution parameters and the link derivatives.
chen_qr_predict <- function(par, X, Z, mu.link = "log", lambda.link = "log") {
  p <- ncol(X)
  q <- ncol(Z)
  if (length(par) != p + q) return(NULL)

  mu_link <- chen_qr_make_link(mu.link)
  lambda_link <- chen_qr_make_link(lambda.link)

  eta_mu <- drop(X %*% par[seq_len(p)])
  eta_lambda <- drop(Z %*% par[p + seq_len(q)])

  if (!mu_link$valideta(eta_mu) || !lambda_link$valideta(eta_lambda)) {
    return(NULL)
  }

  mu <- mu_link$linkinv(eta_mu)
  lambda <- lambda_link$linkinv(eta_lambda)
  dmu <- mu_link$mu.eta(eta_mu)
  dlambda <- lambda_link$mu.eta(eta_lambda)

  if (any(!is.finite(c(mu, lambda, dmu, dlambda))) ||
      any(mu <= 0) || any(lambda <= 0)) return(NULL)

  list(mu = mu, lambda = lambda, eta_mu = eta_mu,
       eta_lambda = eta_lambda, dmu = dmu, dlambda = dlambda)
}

# -----------------------------------------------------------------------------
# Log-likelihood
# -----------------------------------------------------------------------------
chen_qr_loglik <- function(par, y, X, Z, tau,
                           mu.link = "log", lambda.link = "log") {
  pred <- chen_qr_predict(par, X, Z, mu.link, lambda.link)
  if (is.null(pred)) return(-Inf)

  mu <- pred$mu
  lambda <- pred$lambda

  mu_lambda <- mu^lambda
  y_lambda <- y^lambda
  E_mu <- expm1(mu_lambda)
  E_y <- expm1(y_lambda)

  if (any(!is.finite(c(E_mu, E_y))) || any(E_mu <= 0)) return(-Inf)

  ll <- log(-log1p(-tau)) - log(E_mu) +
    (lambda - 1) * log(y) + log(lambda) +
    log1p(-tau) * E_y / E_mu + y_lambda

  if (any(!is.finite(ll))) return(-Inf)
  sum(ll)
}

# -----------------------------------------------------------------------------
# Analytical score vector with a CHAIN RULE for each selected link
# -----------------------------------------------------------------------------
# Score(beta) = X' [ d ell / d mu * d mu / d eta_mu ]
# Score(nu)   = Z' [ d ell / d lambda * d lambda / d eta_lambda ]
chen_qr_score <- function(par, y, X, Z, tau,
                          mu.link = "log", lambda.link = "log") {
  p <- ncol(X)
  q <- ncol(Z)
  bad <- rep(NA_real_, p + q)
  pred <- chen_qr_predict(par, X, Z, mu.link, lambda.link)
  if (is.null(pred)) return(bad)

  mu <- pred$mu
  lambda <- pred$lambda
  a <- mu^lambda
  b <- y^lambda
  E_a <- expm1(a)
  E_b <- expm1(b)
  L <- log1p(-tau)

  if (any(!is.finite(c(a, b, E_a, E_b))) || any(E_a <= 0)) {
    return(bad)
  }

  # More stable than the original expression using (1 - exp(a))^2.
  # r = exp(a) / expm1(a) = 1 / (1 - exp(-a))
  r <- -1 / expm1(-a)
  dlda <- -r * (1 + L * (E_b / E_a))
  dldb <- 1 + L * (exp(b) / E_a)

  dell_dmu <- dlda * (lambda * a / mu)
  dell_dlambda <- dlda * a * log(mu) +
    log(y) + 1 / lambda + dldb * b * log(y)

  score <- c(
    drop(crossprod(X, dell_dmu * pred$dmu)),
    drop(crossprod(Z, dell_dlambda * pred$dlambda))
  )
  if (any(!is.finite(score))) return(bad)
  score
}

# Optimization safeguard for invalid parameter regions.
# This is NOT a constraint transformation: the chosen links are unchanged.
# It merely prevents optim() from receiving -Inf during trial steps.
chen_qr_invalid <- function(par, X, Z, mu.link, lambda.link,
                            gradient = FALSE) {
  p <- ncol(X)
  q <- ncol(Z)
  eta_mu <- drop(X %*% par[seq_len(p)])
  eta_lambda <- drop(Z %*% par[p + seq_len(q)])

  # These four links require strictly positive linear predictors.
  v_mu <- if (mu.link == "log") rep(0, nrow(X)) else pmax(1e-8 - eta_mu, 0)
  v_lam <- if (lambda.link == "log") rep(0, nrow(Z)) else pmax(1e-8 - eta_lambda, 0)

  if (gradient) {
    # Direction back into the admissible region (if eta <= 0).
    ans <- c(
      drop(crossprod(X, 2e6 * v_mu)),
      drop(crossprod(Z, 2e6 * v_lam))
    )
    ans[!is.finite(ans)] <- 0
    return(ans)
  }

  penalty <- sum(v_mu^2) + sum(v_lam^2)
  if (!is.finite(penalty)) penalty <- 1e12
  -1e12 - 1e6 * penalty
}

# -----------------------------------------------------------------------------
# Null model for likelihood-based pseudo-R-squared measures
# -----------------------------------------------------------------------------
# The null model has CONSTANT mu and lambda, each estimated by ML.
# Its numerical optimization uses log links to guarantee positivity;
# this does not change the null model or the likelihood, regardless of
# the links selected in the full regression model.
chen_qr_fit_null <- function(y, tau, maxit = 1000, reltol = 1e-10) {
  n <- length(y)
  X0 <- matrix(1, nrow = n, ncol = 1L)
  Z0 <- matrix(1, nrow = n, ncol = 1L)

  mu0 <- as.numeric(stats::quantile(y, probs = tau, names = FALSE))
  lambda0 <- 0.6
  start0 <- log(c(mu0, lambda0))

  null_ll <- function(par) {
    val <- chen_qr_loglik(
      par = par, y = y, X = X0, Z = Z0, tau = tau,
      mu.link = "log", lambda.link = "log"
    )
    if (is.finite(val)) val else -1e12
  }

  # Same optimization method and null likelihood as Chen_reg_fit.R,
  # but log-parameterized to enforce mu > 0 and lambda > 0.
  opt0 <- try(
    stats::optim(
      par = start0, fn = null_ll, method = "Nelder-Mead",
      control = list(fnscale = -1, maxit = maxit, reltol = reltol)
    ),
    silent = TRUE
  )

  if (inherits(opt0, "try-error")) {
    return(list(loglik = NA_real_, converged = FALSE,
                mu = NA_real_, lambda = NA_real_,
                convergence = 999L))
  }

  ell0 <- chen_qr_loglik(
    par = opt0$par, y = y, X = X0, Z = Z0, tau = tau,
    mu.link = "log", lambda.link = "log"
  )
  converged <- (opt0$convergence == 0L) && is.finite(ell0)

  list(
    loglik = if (converged) ell0 else NA_real_,
    converged = converged,
    mu = exp(opt0$par[1]),
    lambda = exp(opt0$par[2]),
    convergence = opt0$convergence
  )
}

# -----------------------------------------------------------------------------
# Model fitting
# -----------------------------------------------------------------------------
chen_qr_fit <- function(y, X, Z, tau,
                        start = NULL,
                        method = "BFGS",
                        use_score = TRUE,
                        maxit = 1000,
                        reltol = 1e-10,
                        diag = 1,
                        mu.link = "log",
                        lambda.link = "log") {

  y <- as.numeric(y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  n <- length(y)
  p <- ncol(X)
  q <- ncol(Z)

  if (!is.numeric(X) || !is.numeric(Z) || p < 1L || q < 1L) {
    stop("X and Z must be nonempty numeric model matrices.")
  }
  if (nrow(X) != n || nrow(Z) != n || any(!is.finite(c(X, Z)))) {
    stop("X and Z must contain n rows of finite numeric values.")
  }
  if (any(!is.finite(y)) || any(y <= 0)) {
    stop("All observations must be positive and finite.")
  }
  if (length(tau) != 1L || !is.finite(tau) || tau <= 0 || tau >= 1) {
    stop("tau must lie in (0, 1).")
  }

  mu_link <- chen_qr_make_link(mu.link)
  lambda_link <- chen_qr_make_link(lambda.link)
  mu.link <- mu_link$name
  lambda.link <- lambda_link$name

  beta_names <- if (!is.null(colnames(X))) paste0("beta_", colnames(X)) else
    paste0("beta", seq_len(p) - 1L)
  nu_names <- if (!is.null(colnames(Z))) paste0("nu_", colnames(Z)) else
    paste0("nu", seq_len(q) - 1L)
  par_names <- c(beta_names, nu_names)

  # Initial regression coefficients on each selected LINK scale.
  # If initial linear models give inadmissible eta values, try fitting
  # a constant quantile / shape on the same model matrices.
  initial_lm <- function(M, response, fallback, link_name) {
    calc <- function(target) {
      obj <- try(lm.fit(M, target), silent = TRUE)
      if (inherits(obj, "try-error")) return(rep(NA_real_, ncol(M)))
      ans <- as.numeric(obj$coefficients)
      ans[!is.finite(ans)] <- 0
      ans
    }
    admissible <- function(coef) {
      eta <- drop(M %*% coef)
      all(is.finite(eta)) &&
        (link_name == "log" || all(eta > 0))
    }
    ans <- calc(response)
    if (!admissible(ans)) ans <- calc(rep(fallback, nrow(M)))
    if (!admissible(ans)) {
      stop("Could not construct positive predictors for ", link_name,
           " link. Provide an admissible 'start' vector or include an intercept.")
    }
    ans
  }

  if (is.null(start)) {
    mu0 <- as.numeric(stats::quantile(y, tau, names = FALSE))
    lambda0 <- 0.6
    beta_start <- initial_lm(X, mu_link$linkfun(y),
                             mu_link$linkfun(mu0), mu.link)
    nu_start <- initial_lm(Z, rep(lambda_link$linkfun(lambda0), n),
                           lambda_link$linkfun(lambda0), lambda.link)
    start <- c(beta_start, nu_start)
  }

  if (!is.numeric(start) || length(start) != p + q ||
      any(!is.finite(start))) {
    stop("start must be a finite numeric vector of length ncol(X) + ncol(Z).")
  }

  pred_start <- chen_qr_predict(start, X, Z, mu.link, lambda.link)
  if (is.null(pred_start)) {
    stop("Initial values violate positivity or numerical constraints. ",
         "Supply a feasible 'start' for the selected links.")
  }
  if (!is.finite(chen_qr_loglik(start, y, X, Z, tau, mu.link, lambda.link))) {
    stop("Initial values yield a non-finite log-likelihood. ",
         "Try another starting vector.")
  }

  fn <- function(par) {
    val <- chen_qr_loglik(par, y, X, Z, tau, mu.link, lambda.link)
    if (is.finite(val)) return(val)
    chen_qr_invalid(par, X, Z, mu.link, lambda.link)
  }
  gr <- function(par) {
    val <- chen_qr_score(par, y, X, Z, tau, mu.link, lambda.link)
    if (all(is.finite(val))) return(val)
    chen_qr_invalid(par, X, Z, mu.link, lambda.link, gradient = TRUE)
  }

  opt <- try(
    optim(
      par = start,
      fn = fn,
      gr = if (use_score && method == "BFGS") gr else NULL,
      method = method,
      hessian = TRUE,
      control = list(fnscale = -1, maxit = maxit, reltol = reltol)
    ),
    silent = TRUE
  )

  if (inherits(opt, "try-error")) {
    fit <- structure(
      list(convergence = 999, converged = FALSE,
           vcov_ok = FALSE, error = as.character(opt),
           mu.link = mu.link, lambda.link = lambda.link),
      class = "chen_qr_fit"
    )
    if (isTRUE(as.logical(diag))) print(fit)
    return(fit)
  }

  coef <- opt$par
  names(coef) <- par_names
  beta_hat <- coef[seq_len(p)]
  nu_hat <- coef[p + seq_len(q)]

  pred <- chen_qr_predict(coef, X, Z, mu.link, lambda.link)
  loglik <- chen_qr_loglik(coef, y, X, Z, tau, mu.link, lambda.link)
  if (is.null(pred) || !is.finite(loglik)) {
    fit <- structure(
      list(convergence = 998, converged = FALSE, vcov_ok = FALSE,
           error = "Optimization ended outside the valid parameter space.",
           mu.link = mu.link, lambda.link = lambda.link),
      class = "chen_qr_fit"
    )
    if (isTRUE(as.logical(diag))) print(fit)
    return(fit)
  }

  mu_hat <- pred$mu
  lambda_hat <- pred$lambda

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

  se <- zstat <- pvalue <- setNames(rep(NA_real_, p + q), par_names)
  if (vcov_ok) {
    se <- sqrt(diag(vcov))
    zstat <- coef / se
    pvalue <- 2 * pnorm(-abs(zstat))
  }

  fitted_cdf <- pchen_qr(y, mu_hat, lambda_hat, tau)
  residuals <- qnorm(fitted_cdf)
  k <- p + q

  # Quantile pseudo-R2: reduction in the quantile check loss.
  check_loss <- function(u, tau) sum(u * (tau - (u < 0)))
  null_quantile <- as.numeric(stats::quantile(y, probs = tau, names = FALSE))
  loss_null <- check_loss(y - null_quantile, tau)
  pseudo_r2_quantile <- if (loss_null > 0) {
    1 - check_loss(y - mu_hat, tau) / loss_null
  } else NA_real_

  # Likelihood-based pseudo-R2 from Chen_reg_fit.R (Cox-Snell).
  # Both mu and lambda are estimated as constants in the null model.
  null_fit <- chen_qr_fit_null(y = y, tau = tau,
                               maxit = maxit, reltol = reltol)
  loglik_null <- null_fit$loglik
  rsq_cox_snell <- if (is.finite(loglik_null)) {
    -expm1(-2 / n * (loglik - loglik_null))
  } else NA_real_

  # Nagelkerke's normalized version. For continuous responses the
  # normalization may be nonpositive; then no meaningful value is returned.
  nagelkerke_den <- if (is.finite(loglik_null)) {
    -expm1(2 / n * loglik_null)
  } else NA_real_
  rsq_nagelkerke <- if (is.finite(nagelkerke_den) && nagelkerke_den > 0) {
    rsq_cox_snell / nagelkerke_den
  } else NA_real_

  coef_table <- cbind(
    Estimate = coef,
    `Std. Error` = se,
    `z value` = zstat,
    `Pr(>|z|)` = pvalue
  )

  fit <- list(
    coefficients = coef,
    beta = beta_hat,
    nu = nu_hat,
    coef_mu = coef_table[seq_len(p), , drop = FALSE],
    coef_lambda = coef_table[p + seq_len(q), , drop = FALSE],
    mu = mu_hat,
    lambda = lambda_hat,
    eta_mu = pred$eta_mu,
    eta_lambda = pred$eta_lambda,
    mu.link = mu.link,
    lambda.link = lambda.link,
    fitted = mu_hat,
    residuals = residuals,
    vcov = vcov,
    se = se,
    zstat = zstat,
    pvalue = pvalue,
    loglik = loglik,
    loglik_null = loglik_null,
    null_converged = null_fit$converged,
    null_fit = null_fit,
    rsq = rsq_cox_snell,                   # Same formula as Chen_reg_fit.R
    pseudo_r2 = rsq_cox_snell,             # Main pseudo-R2 in this version
    pseudo_r2_cox_snell = rsq_cox_snell,
    pseudo_r2_nagelkerke = rsq_nagelkerke,
    pseudo_r2_quantile = pseudo_r2_quantile,
    aic = -2 * loglik + 2 * k,
    bic = -2 * loglik + log(n) * k,
    convergence = opt$convergence,
    converged = (opt$convergence == 0) && all(is.finite(coef)),
    vcov_ok = vcov_ok,
    counts = opt$counts,
    method = method,
    tau = tau,
    y = y,
    X = X,
    Z = Z
  )

  class(fit) <- "chen_qr_fit"
  if (isTRUE(as.logical(diag))) print(fit)
  fit
}

# -----------------------------------------------------------------------------
# Formatted model output (GAMLSS style, one table per distribution parameter)
# -----------------------------------------------------------------------------
print.chen_qr_fit <- function(x, digits = 4, ...) {
  if (is.null(x$coefficients)) {
    cat("Model fitting failed.\n")
    if (!is.null(x$error)) cat(x$error, "\n")
    return(invisible(x))
  }

  print_coef_table <- function(tab, digits) {
    stats::printCoefmat(
      tab, digits = digits, signif.stars = FALSE,
      has.Pvalue = TRUE, P.values = TRUE, na.print = "NA"
    )
  }

  cat("------------------------------------------------------------\n")
  cat(sprintf("Chen quantile regression (tau = %g)\n", x$tau))
  cat("------------------------------------------------------------\n\n")

  cat(sprintf("Mu link function: %s\n", x$mu.link))
  cat("Mu Coefficients (beta):\n")
  print_coef_table(x$coef_mu, digits)

  cat(sprintf("\nLambda link function: %s\n", x$lambda.link))
  cat("Lambda Coefficients (nu):\n")
  print_coef_table(x$coef_lambda, digits)

  cat("\n------------------------------------------------------------\n")
  cat(sprintf("Log-likelihood: %.*f\n", max(0L, digits - 1L), x$loglik))
  cat(sprintf("Number of function evaluations in %s optim: %s\n",
              x$method, x$counts[["function"]]))
  cat(sprintf("AIC: %.*f    BIC: %.*f\n", max(0L, digits - 1L), x$aic,
              max(0L, digits - 1L), x$bic))
  cat(sprintf("Null log-likelihood: %.*f\n", max(0L, digits - 1L),
              x$loglik_null))
  cat(sprintf("Pseudo-R2 (Cox-Snell): %.*f\n", digits, x$pseudo_r2_cox_snell))
  cat(sprintf("Pseudo-R2 (Nagelkerke): %.*f\n", digits, x$pseudo_r2_nagelkerke))
  cat(sprintf("Pseudo-R2 (quantile): %.*f\n", digits, x$pseudo_r2_quantile))
  if (!isTRUE(x$null_converged)) {
    cat("Note: the null model did not converge; likelihood-based pseudo-R2 is unavailable.\n")
  } else if (is.na(x$pseudo_r2_nagelkerke)) {
    cat("Note: Nagelkerke normalization is not positive for this continuous-response model.\n")
  }
  cat(sprintf("Convergence code: %d%s\n", x$convergence,
              if (isTRUE(x$converged)) " (converged)" else " (check fit)"))
  cat("\nResiduals:\n")
  print(summary(x$residuals))
  invisible(x)
}
