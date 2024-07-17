evaluate_none <- function(data,
                          contrast = define_contrast("raw"),
                          benchmark = 0.5,
                          alpha = 0.05,
                          alternative = "greater",
                          analysis = "co-primary",
                          transformation = "none",
                          regu = c(1, 1 / 2, 0),
                          pars = list(),
                          attrs = list()) {
  ## inference:
  stats <- data2stats(data, contrast = contrast, regu = regu)
  critval <- critval_none(alpha, alternative)
  alpha_adj <- alpha

  ## output:
  stats2results(
    stats = stats,
    alpha = alpha,
    alpha_adj = alpha_adj,
    critval = critval,
    pval_fun = pval_none,
    pval_args = list(alternative = alternative),
    benchmark = benchmark,
    alternative = alternative,
    analysis = analysis,
    transformation = transformation,
    attrs = attrs
  )
}



# Helper functions ----------------------------------------------------------------------------

critval_none <- function(alpha = 0.05,
                         alternative = "two.sided",
                         ...) {
  c(
    switch(alternative,
      two.sided = stats::qnorm(alpha / 2),
      less = -Inf,
      greater = stats::qnorm(alpha)
    ),
    switch(alternative,
      two.sided = stats::qnorm(1 - alpha / 2),
      less = stats::qnorm(1 - alpha),
      greater = Inf
    )
  )
}

pval_none <- function(tstat, alternative = "two.sided") {
  ifelse(alternative == "two.sided", 2, 1) * stats::pnorm(tstat, lower.tail = FALSE)
}
