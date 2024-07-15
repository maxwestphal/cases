evaluate_maxt <- function(data,
                          contrast = define_contrast("raw"),
                          benchmark = 0.5,
                          alpha = 0.025,
                          alternative = "greater",
                          analysis = "co-primary",
                          transformation = "none",
                          regu = c(1, 1/2, 0),
                          pars = list(),
                          attrs = list()
){
  
  ## inference:
  stats <- data2stats(data, contrast=contrast, regu = regu)
  R <- stats::cov2cor(get_cov(stats, analysis)) 
  critval <- critval_maxt(alpha, alternative, R)
  alpha_adj <- alpha_maxt(alpha, alternative, R)

  ## output:
  stats2results(
    stats = stats,
    alpha = alpha,
    alpha_adj = alpha_adj,
    critval = critval,
    pval_fun = pval_maxt,
    pval_args = list(alternative=alternative, R=R),
    benchmark = benchmark,  
    alternative = alternative,
    analysis = analysis,
    transformation = transformation,
    attrs = attrs
  )
}


# Helper functions ----------------------------------------------------------------------------
get_cov <- function(stats, analysis){
  if(analysis == "co-primary"){
    return(active_cov(stats))
  }
  if(analysis == "full"){
    return(full_cov(stats))
  }
  stop("Value of argument analysis not supported.")
}

full_cov <- function(stats){
  G <- length(stats)
  m <- length(stats[[1]][[1]])
  C <- matrix(0, G*m, G*m)
  for(g in 1:G){
    i <- (1:m)+(g-1)*m
    C[i, i] <- stats[[g]]$cov
  }
  return(C)
}

active_cov <- function(stats){
  G <- length(stats)
  ## argmin vector am (where is minimum entry of estimates?)
  E <- sapply(1:G, function(g) stats[[g]]$est)
  am <- apply(E, 1, argmin)
  
  ## binary coefficient matrices for all g (list)
  Bg <- lapply(1:G, function(g)
    diag(as.integer(am == g)))
  
  ## covariance matrices for all g (list)
  Sg <- lapply(stats, function(x) x$cov)
  
  ## product BSB for all g (list)
  BSBg <-
    mapply(function(B, S) {
      B %*% S %*% B
    }, Bg, Sg, SIMPLIFY = FALSE)
  
  ## final overall covariance matrix of 'active' estimates
  do.call("+", BSBg)
}

#' @importFrom mvtnorm qmvnorm pmvnorm
critval_maxt <-
  function(alpha = 0.05,
           alternative = "greater",
           R,
           ...) {
    
    
    tail <- switch (
      alternative,
      two.sided = "both.tails",
      less = "upper.tail",
      greater = "lower.tail",
    )
    
    q <- mvtnorm::qmvnorm(p = 1-alpha, sigma = R, tail = tail)$quantile
    
    cv <- switch(alternative,
                 greater = c(-q, Inf),
                 two.sided = c(-q, q),
                 less = c(-Inf, -q)
    )
    
    return(cv)
  }

pval_maxt <- function(tstat, alternative, R){
  
  sapply(tstat, function(x) {
    l <- ifelse(alternative == "two.sided", -x, -Inf)
    1 - mvtnorm::pmvnorm(lower = rep(l, nrow(R)), upper = rep(x, nrow(R)), corr=R)[1]
  })
  
}

alpha_maxt <- function(alpha, alternative, R){ 
  
  # critval under independence assumption (Sidak): 
  critval_0 <- critval_none(1-(1-alpha)^(1/nrow(R)), alternative)
  
  1 - mvtnorm::pmvnorm(lower = rep(critval_0[1], nrow(R)),
                       upper = rep(critval_0[2], nrow(R)),
                       corr=R)[1]
  
  
}

