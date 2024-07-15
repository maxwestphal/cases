#' @importFrom dplyr mutate rename
stats2results <- function(stats,
                          alpha = 0.05,
                          alpha_adj = NA,
                          critval = c(-1.96, 1.96),
                          pval_fun = pval_none,
                          pval_args = list(),
                          benchmark = rep(0.5, length(stats)),
                          alternative = "greater",
                          analysis = "co-primary",
                          transformation = "none",
                          attrs = list(),
                          ...) {
  lapply(
    1:length(stats),
    function(g) {
      stat2results(
        stats[[g]],
        critval,
        pval_fun,
        pval_args,
        benchmark[g],
        alternative,
        transformation,
        analysis
      )
    }
  ) %>%
    complete_results(
      benchmark = benchmark
    ) %>%
    set_attrs(
      attrs = update_attrs(attrs, critval = critval, alpha_adj = alpha_adj)
    )
}

#' Complete evaluation results
#'
#' @param results "cases_results" object, i.e. result of \link{evaluate}
#' @param benchmark numeric, vector of benchmark values
#'
#' @details Not exported, but applied at the end of evaluate by default
#'
#' @importFrom dplyr rename mutate
#'
#' @return "cases_results" object
complete_results <- function(results, benchmark) {
  lower <- upper <- NULL

  results <- lapply(1:length(results), function(g) {
    results[[g]] %>%
      dplyr::mutate(
        reject = lower > benchmark[g] | upper < benchmark[g],
        pval_all = NA,
        reject_all = NA
      )
  })

  pval_all_ <- do.call(pmax, lapply(results, function(x) x$pval))
  reject_all_ <- do.call("&", lapply(results, function(x) x$reject))

  results <- lapply(1:length(results), function(g) {
    results[[g]] %>%
      dplyr::mutate(
        pval_all = pval_all_,
        reject_all = reject_all_
      )
  })

  return(results)
}


#' @importFrom dplyr mutate_if
stat2results <- function(stat,
                         critval = c(-1.96, 1.96),
                         pval_fun = pval_none,
                         pval_args = list(),
                         benchmark = 0.5,
                         alternative = "greater",
                         transformation = "none",
                         analysis = "co-primary") {
  tf <- get_tf(transformation)
  est.t <- tf$est_link(stat$est)
  bm.t <- tf$est_link(benchmark)
  se.t <- tf$se_link(stat$se, stat$n, stat$est)
  tstat <- tstat_tr((est.t - bm.t) / se.t, alternative = alternative)

  result <-
    data.frame(
      parameter = stat$names,
      hypothesis = hypstr(alternative, benchmark),
      estimate = stat$est,
      lower = tf$est_inv(est.t + critval[1] * se.t),
      upper = tf$est_inv(est.t + critval[2] * se.t),
      tstat = tstat,
      pval = do.call(pval_fun, args = c(list(tstat = tstat), pval_args))
    )
  rownames(result) <- NULL

  return(result)
}

hypstr <- function(alternative, benchmark) {
  paste0(
    switch(alternative,
      greater = " <= ",
      two.sided = " == ",
      less = " >= "
    ),
    benchmark
  )
}
