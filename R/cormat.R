#' Create an equicorrelation matrix
#'
#' @param m (numeric) \cr dimensions of the (square) matrix
#' @param rho (numeric) \cr correlation parameter in (0,1)
#' @param d (logical | numeric) \cr binary vector of length m, whereby TRUE/FALSE (alternatively 1/0)
#' indicate active/inactive components of underlying random vector.
#'
#' @return
#' (matrix) \cr
#' AR(1) correlation matrix R with entries \eqn{R_{ij} = \rho, i\neq j}
cormat_equi <- function(m, rho, d = TRUE) {
  R <- matrix(rho, m, m)
  stopifnot(length(d) %in% c(1, m))
  R <- diag(d, m) %*% R %*% diag(d, m)
  diag(R) <- 1
  return(R)
}

#' Create an AR(1) correlation matrix
#'
#' @param m (numeric) \cr dimensions of the (square) matrix
#' @param rho (numeric) \cr correlation parameter in (0,1)
#' @param d (logical | numeric) \cr binary vector of length m, whereby TRUE/FALSE (alternatively 1/0)
#' indicate active/inactive components of underlying random vector.
#'
#' @return
#' (matrix) \cr
#' AR(1) correlation matrix R with entries \eqn{R_{ij} = \rho^{|i-j|}}
cormat_ar1 <- function(m, rho, d = TRUE) {
  M <- matrix(rho, m, m)
  R <- M^(abs(col(M) - row(M)))
  stopifnot(length(d) %in% c(1, m))
  R <- diag(d, m) %*% R %*% diag(d, m)
  diag(R) <- 1
  return(R)
}
