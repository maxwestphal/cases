critval_bootstrap <- function(alpha, alternative, bst_max){
  if(any(!is.finite(bst_max))){
    message(paste0("Attention: ", sum(!is.finite(bst_max)), " bootstrap sample statistics (" ,
                   100*mean(!is.finite(bst_max)) , "%) are not finite!"))
  }
  
  cv <- stats::quantile(bst_max, 1-alpha, na.rm=TRUE)
  
  cv <- switch(alternative,
               greater = c(-cv, Inf), 
               two.sided = c(-cv, cv),
               less = c(-Inf, cv)
  )
  
  return(unname(cv))
}

#'@importFrom stats quantile
alpha_bootstrap <- function(alpha, alternative, bst){ 
  
  # critval under independence (random permutation of columns): 
  cv_0 <- permute_matrix(bst) %>% apply(1, max) %>% stats::quantile(1-alpha) %>% unname()

  mean(apply(bst, 1, max) > cv_0, na.rm=TRUE)

}

permute_matrix <- function(x, margin=2){
  
  apply(x, margin, function(xj){
    xj[sample(length(xj))]
  })
  
}


pval_bootstrap <- function(tstat, bst_max){
  sapply(tstat, function(x) mean(bst_max > x, na.rm=TRUE))  
}




## create bootstrap sample:
bootstrap_sample <- function(data, contrast, regu, alternative, analysis, pars){
  pars$type <- get_from_pars(pars, "type", "pairs")
  pars$nboot <- get_from_pars(pars, "nboot", 2000) 
  
  stopifnot(pars$type %in% c("pairs", "wild"))
  stopifnot(pars$nboot %% 1 == 0)
  
  message(paste0("Drawing ", pars$nboot, " '", pars$type, "' bootstrap samples..."))
  
  do.call(paste0("bootstrap_sample_", pars$type), 
          args=list(data, contrast, regu, alternative, analysis, pars))
}


## pairs bootstrap:
bootstrap_sample_pairs <- function(data, contrast, regu = c(0,0,0), 
                                   alternative = "greater", 
                                   analysis = "co-primary", 
                                   pars = list()){
  
  G <- length(data); ng=sapply(data, nrow)
  mu0 <- stats2est(data2stats(data, contrast, regu, raw=TRUE))
  sapply(1:pars$nboot, function(b){
    st <- data2stats(bs_draw_pairs(data, G=G, ng=ng), contrast, regu, raw=TRUE);
    combine_tstat(stats2est(st), stats2tstat(st, mu0, alternative), analysis) 
  }) %>% 
    t()
}

bs_draw_pairs <- function(data, G=length(data), ng=sapply(data, nrow)){
  lapply(1:G, function(g) data[[g]][sample(ng[g], replace=TRUE), , drop=FALSE] )
}


## wild bootstrap:
bootstrap_sample_wild <- function(data, 
                                  contrast,
                                  regu = c(0,0,0), 
                                  alternative = "greater",
                                  analysis = "co-primary",
                                  pars = list(nboot=2000)){
  
  pars$dist <- get_from_pars(pars, "dist", "Normal") 
  pars$res_tra <- get_from_pars(pars, "res_tra", 0) 
  pars$proj_est <- get_from_pars(pars, "proj_est", TRUE) 
  
  ## insert pseudo obs
  if(regu[1] != 0 ){
    if(!(regu[1]==2 & regu[2]==1)){
      warning("regu should be either c(0,0,?) or c(2,1,?). Switched to second case, as regu[1] != 0!")
    }
    data <- lapply(data, function(d){
      rbind(d, 0, 1)
    })
  }
  
  G <- length(data); ng=sapply(data, nrow); m <- ncol(data[[1]])
  
  mu0_raw <- data2stats(data, 
                        contrast=define_contrast("raw"),
                        regu = c(0, 0, 0), # -> data was augmented already, don't regularize "again"
                        raw=TRUE) %>% 
    stats2est()
  mu0 <- lapply(mu0_raw, function(x) as.numeric(contrast(data) %*% x))
  
  M <- lapply(1:G, function(g){
    matrix(mu0_raw[[g]], nrow=ng[g], ncol=length(mu0_raw[[g]]), byrow=TRUE)
  })
  D <- lapply(1:G, function(g){
    data[[g]] - M[[g]] 
  })
  
  sapply(1:pars$nboot, function(b){
    st <- bs_draw_wild(M, D, G, ng, m,
                       dist=pars$dist,
                       res_tra=pars$res_tra) %>% 
    data2stats(contrast, regu = c(0,0,0), raw=TRUE, proj_est=pars$proj_est);
    combine_tstat(stats2est(st), stats2tstat(st, mu0, alternative), analysis) 
  }) %>% 
    t()
  
}

bs_draw_wild <- function(M, D, 
                         G=length(M), ng=sapply(M, nrow), m=ncol(M[[1]]),
                         dist = "Normal", res_tra=0){
  R <- rm(ng, m, dist); 
  lapply(1:G, function(g){
    M[[g]] + ( res_transform(D[[g]], res_tra = res_tra) * R[[g]] )
  })
}

## random vector for wild bootstrap:
rv <- function(n=100, dist="Normal"){
  if(dist=="Rademacher"){
    return((stats::rbinom(n, 1, 1/2)-0.5)*2)
  }
  if(dist=="Normal"){
    return(stats::rnorm(n))
  }else{
    stop("argument 'dist' not recognized")
  }
}

## random matrix of correct dimensions:
rm <- function(ng=c(5, 10), m=4, dist="Normal"){
  lapply(ng, function(n){
    matrix(rv(n, dist=dist), nrow=n, ncol=m, byrow=FALSE)
  })
}

## residual transformations:
res_transform <- function(x, h=rep(1/nrow(x), nrow(x)), res_tra=0){
  if(res_tra == 0){
    return(x)
  }
  if(res_tra == 1){
    if(nrow(x) <= ncol(x)){stop("For res_tra == 1, nrow(x) > ncol(x) is required...")}
    return(sqrt(nrow(x)/(nrow(x) - ncol(x))) * x)
  }
  if(res_tra == 2){
    return(matrix( (1-h)^(-0.5), nrow=nrow(x), ncol=ncol(x), byrow=FALSE) * x)
  }
  if(res_tra == 3){
    return(matrix( (1-h)^(-1), nrow=nrow(x), ncol=ncol(x), byrow=FALSE) * x)
  }
  stop("pars$res_tra needs to be 0,1,2 or 3.")
} 






