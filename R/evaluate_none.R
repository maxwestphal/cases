evaluate_none <- function(data = draw_data(seed=1337),
                     contrast = define_contrast("raw"),
                     benchmark = 0.5,
                     alpha = 0.05,
                     alternative = "greater",
                     analysis = "co-primary",
                     transformation = "none",
                     regu = FALSE,
                     pars = list()
){  
  stats <- data2stats(data, contrast=contrast, regu = regu)
  cv <- cv_none(alpha, alternative)
  
  ## output
  stats %>% 
    stats2results(
      alpha = alpha, 
      cv=cv, pval_fun=pval_none, benchmark,  
      alternative, analysis, transformation
    ) %>% 
    setattr(
      n = sapply(stats, function(x) x$n), m=ncol(data[[1]]), 
      alpha=alpha, alpha_adj=alpha, cv=cv
    ) %>% 
    return()
}



# Helper functions ----------------------------------------------------------------------------

cv_none <- function(alpha = 0.05,
                   alternative = "two.sided",
                   ...) {
  c(switch(
    alternative,
    two.sided = stats::qnorm(alpha / 2),
    less = -Inf,
    greater = stats::qnorm(alpha)
  ),
  switch(
    alternative,
    two.sided = stats::qnorm(1 - alpha / 2),
    less = stats::qnorm(1 - alpha),
    greater = Inf
  )
  )
}

pval_none <- function(tstat, alternative = "two.sided", analysis){
  switch(
    alternative,
    two.sided = stats::pnorm(abs(tstat), lower.tail = FALSE), 
    less = stats::pnorm(-tstat, lower.tail = FALSE),
    greater = stats::pnorm(tstat, lower.tail = FALSE)
  )
}


