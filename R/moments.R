## data to moments calculations:
data_moments <- function(d) {
  list(n = nrow(d), moments = t(d) %*% d)
}

prior_moments <- function(m, regu = c(1, 1 / 2, 1 / 4)) {
  A <- matrix(regu[3], m, m)
  diag(A) <- rep(regu[2], m)
  list(n = regu[1], moments = A)
}

add_moments <- function(pmom, dmom) {
  return(mapply("+", pmom, dmom, SIMPLIFY = FALSE))
}


## calculations based on moments:
mom2est <- function(mom) {
  return(diag(mom[[2]]) / mom[[1]])
}

mom2cov <- function(mom) {
  n <- mom[[1]]
  A <- mom[[2]]
  a <- diag(A)
  return((n * A - (a %*% t(a))) / (n^2) / (n + 1))
}

cov2var <- function(cov) {
  diag(cov)
}

cov2se <- function(cov) {
  sqrt(cov2var(cov))
}

## calculations based on raw data
dat2est <- function(dat, regu = c(0, 0, 0)) {
  n <- nrow(dat)
  (colSums(dat) + regu[2]) / (n + regu[1])
}

data2est <- function(data, regu = c(0, 0, 0)) {
  lapply(data, function(dat) dat2est(dat, regu = regu))
}

dat2var <- function(dat, regu = c(0, 0, 0)) {
  a <- colSums(dat) + regu[2]
  b <- regu[1] + nrow(dat) - a
  (a * b) / ((a + b)^2 * (a + b + 1))
}

dat2se <- function(dat, regu = c(0, 0, 0)) {
  sqrt(dat2var(dat, regu))
}

## calculations based on stats
stats2est <- function(stats) {
  lapply(stats, function(s) s$est)
}

stats2tstat <- function(stats, mu0, alternative = "greater") {
  stopifnot(is.list(stats))
  G <- length(stats)
  m <- length(stats[[1]]$est)

  if (is.numeric(mu0)) {
    mu0 <- lapply(1:G, function(g) rep(mu0[g], m))
  }
  stopifnot(is.list(mu0))

  lapply(1:G, function(g) {
    tstat_tr((stats[[g]]$est - mu0[[g]]) / stats[[g]]$se, alternative)
  })
}

tstat_tr <- function(x, alternative = "greater") {
  y <- switch(alternative,
    greater = x,
    two.sided = abs(x),
    less = -x
  )
  return(nan2zero(y))
}

nan2zero <- function(x) {
  x[is.nan(x)] <- 0
  return(x)
}

combine_tstat <- function(est, tstat, analysis) {
  if (analysis == "co-primary") {
    b <- pargmin(args = est, rdm = TRUE)
    return(do.call(cbind, tstat)[cbind(1:length(b), b)])
  }
  if (analysis == "full") {
    return(do.call(c, tstat))
  }
}
