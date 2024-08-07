#' Generate binary data
#'
#' @param n (numeric) \cr integer, overall sample size
#' @param m (numeric) \cr integer, number of models
#' @param prev (numeric) \cr vector of class prevalences (adding up to 1)
#' @param random (logical) \cr random sampling (TRUE) or fixed group sample sizes (FALSE)
#' @param method (character) \cr either "roc", "lfc" (multiple subgroups) or "prob" (no subgroups)
#' @param pars (list) \cr containing further named parameters passed to \code{\link{draw_data_roc}},
#'  \code{\link{draw_data_lfc}}
#' @param ... (any) \cr further named parameters passed
#'
#' @return (matrix) \cr generated binary data (possibly stratified for subgroups)
#' @export
#'
#' @examples draw_data()
draw_data <- function(n = 200,
                      prev = c(0.5, 0.5),
                      random = FALSE,
                      m = 10,
                      method = c("roc", "lfc", "pr"),
                      pars = list(),
                      ...) {
  method <- match.arg(method)

  ng <- sample_ng(n = n, prev = prev, random = random)

  args <- c(list(ng = ng, m = m), pars, list(...))
  do.call(paste0("draw_data_", method), args)
}

#' @title Sample binary data (single sample)
#'
#' @description This function is wrapper for \code{\link[bindata]{rmvbin}}.
#'
#' @param n (numeric) \cr integer, sample size
#' @param pr (numeric) \cr vector with marginal success probabilities
#' @param R (matrix) \cr square correlation matrix
#'
#' @return (matrix) \cr matrix with n rows and length(pr) columns of randomly generated binary (0, 1) data
#'
#' @importFrom bindata rmvbin
#' @export
draw_data_prb <- function(n = 100,
                          pr = c(0.8, 0.8),
                          R = diag(length(pr))) {
  if (length(pr) == 1) {
    return(bindata::rmvbin(n = n, margprob = pr))
  }
  return(bindata::rmvbin(
    n = n,
    margprob = pr,
    bincorr = R
  ))
}

#' @importFrom extraDistr rmnom
sample_ng <- function(n = 100,
                      prev = c(0.5, 0.5),
                      random = FALSE) {
  stopifnot(all(prev >= 0))
  prev <- prev / sum(prev)
  ng <- rep(0, length(prev))
  while (any(ng == 0)) {
    if (random) {
      ng <- as.numeric(extraDistr::rmnom(
        n = 1,
        size = n,
        prob = prev
      ))
    } else {
      ng <- round(n * prev, 0)
      ng[1] <- n - sum(ng[-1])
    }
  }
  return(ng)
}
