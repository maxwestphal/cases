#' @importFrom dplyr mutate rename
stats2results <- function(stats, alpha = 0.025, cv=c(-1.96, 1.96),
                          pval_fun=pval_none, 
                          benchmark=rep(0.5, length(stats)), 
                          alternative="greater", 
                          analysis = "co-primary",
                          transformation="none",
                          ...) {
  lapply(1:length(stats), function(g) stat2results(stats[[g]], 
                                                   cv,
                                                   pval_fun,
                                                   benchmark[g],
                                                   alternative,
                                                   transformation, 
                                                   analysis)) %>% 
    complete_results(alpha=alpha, benchmark=benchmark, analysis=analysis)
}

#' Complete evaluation results
#'
#' @param results "cases_results" object, i.e. result of \link{evaluate}
#' @param alpha numeric, significance level
#' @param analysis character, either "co-primary" or "full"
#' @param benchmark numeric, vector of benchmark values
#'
#' @details Not exported, but applied at the end of evaluate by default
#'
#' @importFrom dplyr rename mutate
#'
#' @return "cases_results" object
complete_results <- function(results, benchmark, alpha, analysis){
  
  lower <- upper <- NULL
  
  results <- lapply(1:length(results), function(g){
    results[[g]] %>% 
      dplyr::mutate(reject = lower > benchmark[g] | upper < benchmark[g],
                    pval_all = NA,
                    reject_all = NA)
  }) 
  
  pval_all_ <- do.call(pmax, lapply(results, function(x) x$pval))
  reject_all_ <- do.call("&", lapply(results, function(x) x$reject)) 
  
  results <- lapply(1:length(results), function(g){
    results[[g]] %>% 
      dplyr::mutate(pval_all = pval_all_,
                    reject_all = reject_all_)
  }) 
  
  return(results)
  
}


#' @importFrom dplyr mutate_if
stat2results <- function(stat, cv=c(-1.96, 1.96), pval_fun=pval_none,
                         benchmark=0.5, 
                         alternative="greater",
                         transformation="none",
                         analysis = "co-primary") {
  
  tf <- get_tf(transformation)
  est.t <- tf$est_link(stat$est)
  bm.t <- tf$est_link(benchmark)
  se.t <- tf$se_link(stat$se, stat$n, stat$est)
  tstat <- (est.t - bm.t)/se.t 

  result <-
    data.frame(
      parameter = stat$names,
      hypothesis = hypstr(alternative, benchmark),
      estimate = stat$est,
      lower = tf$inv(est.t + cv[1] * se.t), 
      upper = tf$inv(est.t + cv[2] * se.t),
      pval = pval_fun(tstat, alternative, analysis) 
    ) 
  rownames(result) <- NULL
  return(result)
}

altstr <- function(alternative, benchmark){
  paste0(switch(alternative,
                greater = " > ",
                two.sided = " != ",
                less = " < "),
         benchmark)
}

hypstr <- function(alternative, benchmark){
  paste0(switch(alternative,
                greater = " <= ",
                two.sided = " == ",
                less = " >= "),
         benchmark)
}


