#' Generate binary data (ROC model)
#'
#' @param m integer, number of models
#' @param auc numeric, vector of AUCs of biomarkers
#' @param rho numeric, vector (length 2) of correlations between biomarkers
#' @param dist character, either "normal" or "exponential" specifying the subgroup biomarker distributions
#' @param delta numeric, specify importance of sensitivity and specificity (default 0)
#' @param e numeric, emulates better (worse) model selection quality with higher (lower) values of e
#' @param k integer, technical parameter which adjusts grid size
#' @param n integer, total sample size
#' @param prev numeric, disease and healthy prevalence (adds up to 1)
#' @param random logical, random sampling (TRUE) or fixed prevalence (FALSE)
#' @param modnames character, model names (length m)
#' @param corrplot logical (default: FALSE), if TRUE do not return data but instead plot correlation
#' matrices for final binary data
#' @param ... further arguments (currently unused)
#'
#' @return Generated binary dataset
#' @export
#'
#' @examples 
#' data <- draw_data_roc()
#' head(data)
#' @importFrom corrplot corrplot
draw_data_roc <- function(n = 100,
                          prev = c(0.5, 0.5),
                          random = FALSE,
                          m = 10,
                          auc = seq(0.85, 0.95, length.out = 5),
                          rho = c(0.25, 0.25),
                          dist = c("normal", "exponential"),
                          e = 10,
                          k = 100,
                          delta = 0,
                          modnames = paste0("model", 1:m),
                          corrplot = FALSE,
                          ...) {
  
  dist <- match.arg(dist)
  
  ng <- sample_ng(n, prev, random)
  stopifnot(length(ng) == 2)
  
  r <- length(auc)
  G <- length(ng)
  
  ## draw samples in subgroups:
  S <- draw_subgroups(auc=auc, r=r, rho=rho, ng=ng, dist=dist)
  
  
  ## cutoffs:
  q <- stats::quantile(rbind(S[[1]], S[[2]]), c(0.05, 0.95))
  cu <- seq(q[1], q[2], length.out = k)
  
  ## derive binary data:
  Y1 <-
    apply(S[[1]], 2, function(x)
      lapply(cu, function(t)
        as.numeric(x > t)))
  Y1 <- do.call(cbind, lapply(Y1, function(x)
    do.call(cbind, x)))
  
  Y2 <-
    apply(S[[2]], 2, function(x)
      lapply(cu, function(t)
        as.numeric(x <= t)))
  Y2 <- do.call(cbind, lapply(Y2, function(x)
    do.call(cbind, x)))
  
  
  ## selection of models
  if(dist == "normal"){
    theta1 <- 1 - stats::pnorm(rep(cu, r), mean = rep(S$pars$mu1, each = k))
    theta2 <- stats::pnorm(rep(cu, r), mean = rep(S$pars$mu2, each = k))
  }
  if(dist == "exponential"){
    theta1 <- 1 - stats::pexp(rep(cu, r), rate = rep(S$pars$lambda1, each = k))
    theta2 <- stats::pexp(rep(cu, r), rate = rep(S$pars$lambda2, each = k))
  }
  
  
  tau <- pmin(theta1, theta2 + delta)
  s <- sort(sample(1:(r * k), m, prob = tau ^ e))
  

  ## true parameters values
  info <- data.frame(
    model = modnames,
    auc = rep(auc, each = k)[s],
    cutoff = rep(cu, r)[s],
    se = theta1[s],
    sp = theta2[s]
  )
  
  Y1s <- Y1[, s, drop=FALSE]
  Y2s <- Y2[, s, drop=FALSE]
  

  ## inspect parameter values:
  if (corrplot) {
    R1 <- stats::cov2cor(stats::cov(Y1s))
    R2 <- stats::cov2cor(stats::cov(Y2s))
    corrplot::corrplot(R1)
    corrplot::corrplot(R2)
    return(info)
  }
  
  colnames(Y1s) <- colnames(Y2s) <- modnames
  out <- list(Y2s, Y1s)
  
  names(out) <- c("specificity", "sensitivity")
  attr(out, "info") <- info

  return(out)
}

#draw_data_roc()
#draw_data(pars=list(m=5))


draw_subgroups <- function(auc, r, rho, ng, dist){
  
  mu1 <- mu2 <- lambda1 <- lambda2 <- NA
  
  if(dist == "normal"){
    ## mean vector (diseased: 1, healthy: 2):
    mu1 <- sqrt(2) * stats::qnorm(auc)
    mu2 <- rep(0, r)
    
    ## covariance Matrix:
    C1 <- matrix(rho[1], r, r) + diag(1-rho[1], nrow=r, ncol=r)
    C2 <- matrix(rho[2], r, r) + diag(1-rho[2], nrow=r, ncol=r)
    
    ## subgroup samples:
    S1 <- mvtnorm::rmvnorm(ng[1], mu1, C1)
    S2 <- mvtnorm::rmvnorm(ng[2], mu2, C2)
  }
  
  if(dist == "exponential"){
    
    ## inverse scale parameters:
    lambda2 = rep(1, r)
    lambda1 = lambda2/auc - lambda2
    
    ## subgroup samples:
    S1 <- copula::normalCopula(param=rho[1], dim = r, dispstr = "ex") %>% 
      copula::mvdc(margins=rep("exp", r),
                   paramMargins=lapply(lambda1, function(x) list(rate=x)) ) %>% 
      copula::rMvdc(ng[1], .)
    S2 <- copula::normalCopula(param=rho[2], dim = r, dispstr = "ex") %>% 
      copula::mvdc(margins=rep("exp", r),
                   paramMargins=lapply(lambda2, function(x) list(rate=x)) ) %>% 
      copula::rMvdc(ng[2], .)
    
  }
  
  return(
    list(S1=S1, S2=S2,
         pars = list(
           mu1=mu1, mu2=mu2,
           lambda1 = lambda1, lambda2=lambda2)
    )
  )
}



# TODO: clean up
# https://pjsor.com/pjsor/article/view/992/452
# auc_biexp <- function(lambda0=1, lambda1=2){
#   lambda0 / (lambda0 + lambda1)
# }
# 
# auc_biexp(1, 1/(1:10))
# 
# auc = l0 / (l0+l1)
# 
# 1/auc = (l0+l1)/l0
# 
# l0/auc = l0+l1
# 
# l0/auc -l0 = l1
# 
# 
# auc=.9
# l0=10
# l1 = l0/auc -l0
# 
# l1
# auc_biexp(1, l1)
