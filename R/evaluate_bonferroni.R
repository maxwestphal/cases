evaluate_bonferroni <- function(data,
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
  # number of relevant params, depends on 'analysis':
  m <- dplyr::case_match(
    analysis,
    "co-primary" ~ attrs$n_params,
    "full" ~ attrs$n_params * attrs$n_groups
  )

  stats <- data2stats(data = data, contrast = contrast, regu = regu)
  critval <- critval_bonferroni(alpha = alpha, m = m, alternative = alternative)
  alpha_adj <- alpha / m

  ## output:
  stats2results(
    stats = stats,
    alpha = alpha,
    alpha_adj = alpha_adj,
    critval = critval,
    pval_fun = pval_bonferroni,
    pval_args = list(alternative = alternative, m = m),
    benchmark = benchmark,
    alternative = alternative,
    analysis = analysis,
    transformation = transformation,
    attrs = attrs
  )
}

# Helper functions ----------------------------------------------------------------------------
critval_bonferroni <- function(alpha, m, alternative) {
  critval_none(alpha / m, alternative)
}

pval_bonferroni <- function(tstat, alternative, m) {
  (m * pval_none(tstat, alternative)) %>% pmin(1)
}
