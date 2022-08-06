data2stats <- function(data, contrast, regu=c(0,0,0), raw=FALSE){
  lapply(data, dat2stats, K=contrast(data), regu=regu, raw=raw)
}

dat2stats <- function(dat, K, regu=c(0,0,0), raw=FALSE){
  if(raw){
    est <- K %*% dat2est(dat, regu) 
    sig <- NA
    se <- cov2se(K %*% diag(dat2var(dat, regu)) %*% t(K))
  }else{
    mom <- add_moments(prior_moments(ncol(dat), regu), data_moments(dat))
    est <- K %*% mom2est(mom)
    sig <- K %*% mom2cov(mom) %*% t(K)
    se <- cov2se(sig)
  }

  out <- list(names = rownames(K),
              est = as.numeric(est), 
              cov = sig,
              se = se,
              n = nrow(dat),
              npseudo = regu[1],
              ntotal = nrow(dat) + regu[1])
}



# TESTS ---------------------------------------------------------------------------------------
#data <- draw_data_lfc()
# contrast = define_contrast("dunnett", 1)
# contrast(data)
# data2stats(data, contrast)
# 
# data2stats
# dat <- data[[1]]
