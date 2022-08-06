#' Create an equicorrelation matrix
#'
#' @param m integer, dimension
#' @param rho numeric, correlation parameter in (0,1)
#' @param d binary vector of length m, whereby TRUE/FALSE (alternatively 1/0)
#' indicate active/inactive components of underlying random vector. 
#'
#' @return \eqn{R_{ij} = \rho, i\neq j}
cormat_equi <- function(m, rho, d=TRUE){
  R <- matrix(rho, m, m)
  stopifnot(length(d) %in% c(1, m))
  R <- diag(d, m) %*% R %*% diag(d, m)
  diag(R) <- 1
  return(R)
}

#' Create an AR(1) correlation matrix
#'
#' @param m integer, dimension
#' @param rho numeric, correlation parameter in (0,1)
#' @param d binary vector of length m, whereby TRUE/FALSE (alternatively 1/0)
#' indicate active/inactive components of underlying random vector. 
#'
#' @return \eqn{R_{ij} = \rho^{|i-j|}}
cormat_ar1 <- function(m, rho, d=TRUE){
  M <- matrix(rho, m, m)
  R <- M^(abs(col(M) - row(M)))
  stopifnot(length(d) %in% c(1, m))
  R <- diag(d, m) %*% R %*% diag(d, m)
  diag(R) <- 1
  return(R)
}