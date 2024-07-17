data2stats <- function(data, contrast, regu = c(0, 0, 0), raw = FALSE, proj_est = FALSE) {
  lapply(data, dat2stats, K = contrast(data), regu = regu, raw = raw, proj_est = proj_est)
}

dat2stats <- function(dat, K, regu = c(0, 0, 0), raw = FALSE, proj_est = FALSE) {
  n <- nrow(dat)

  if (raw) {
    est <- (K %*% dat2est(dat, regu)) %>%
      project_estimate(n = n, regu = regu, proj_est = proj_est)
    sig <- NA
    se <- cov2se(K %*% diag(as.numeric(est2var(est, n))) %*% t(K))
  } else {
    mom <- add_moments(prior_moments(ncol(dat), regu), data_moments(dat))
    est <- (K %*% mom2est(mom))
    sig <- K %*% mom2cov(mom) %*% t(K)
    se <- cov2se(sig)
  }

  # output:
  list(
    names = rownames(K),
    est = as.numeric(est),
    cov = sig,
    se = se,
    n = n,
    npseudo = regu[1],
    ntotal = n + regu[1]
  )
}


est2var <- function(est, n) {
  est * (1 - est) / (n + 1)
}


project_estimate <- function(est, n, regu, proj_est = FALSE) {
  if (!proj_est) {
    return(est)
  }

  est_min <- dat2est(matrix(0, n, 1), regu)
  est_max <- dat2est(matrix(1, n, 1), regu)

  pmax(pmin(est, est_max), est_min)
}
