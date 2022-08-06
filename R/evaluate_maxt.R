evaluate_maxt <- function(data = draw_data(seed=1337),
                          contrast = define_contrast("raw"),
                          benchmark = 0.5,
                          alpha = 0.025,
                          alternative = "greater",
                          analysis = "co-primary",
                          transformation = "none",
                          regu = FALSE,
                          pars = list()
){
  
  stats <- data2stats(data, contrast=contrast, regu = regu)
  R <- stats::cov2cor(get_cov(stats, analysis)) 
  cv <- cv_maxt(R, alpha, alternative)
  
  ## output
  stats %>% 
    stats2results(
      alpha = alpha, 
      cv=cv, pval_fun=pval_maxt(R), benchmark,  
      alternative, analysis, transformation
    ) %>% 
    setattr(
      n = sapply(stats, function(x) x$n), m=ncol(R), 
      alpha=alpha, alpha_adj=alpha_maxt(alpha, alternative, R), cv=cv
    ) %>% 
    return()
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
cv_maxt <-
  function(R,
           alpha = 0.05,
           alternative = "greater",
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

pval_maxt <- function(R){
  function(tstat, alternative, analysis){
    if(analysis=="full"){return(NA)}
    m <- length(tstat)
    switch(alternative,
           greater = sapply(tstat, function(x) 
             mvtnorm::pmvnorm(lower = rep(x, m), upper = rep(Inf, m), corr=R)[1]),
           two.sided = sapply(tstat, function(x) 
             mvtnorm::pmvnorm(lower = rep(-abs(x), m), upper = rep(abs(x), m), corr=R)[1]),
           less = sapply(tstat, function(x) 
             mvtnorm::pmvnorm(lower = rep(-Inf, m), upper = rep(x, m), corr=R)[1]))
  } 
}

alpha_maxt <- function(alpha, alternative, R){ 
  NA 
}

