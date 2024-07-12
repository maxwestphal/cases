#' @importFrom boot boot
evaluate_bootstrap <- function(data,
                               contrast = define_contrast("raw"),
                               benchmark = 0.5,
                               alpha = 0.05,
                               alternative = "greater",
                               analysis = "co-primary",
                               transformation = "none",
                               regu = c(1, 1/2, 0),
                               pars = list(),
                               attrs = list()
){
  
  ## inference:
  stats <- data2stats(data, contrast=contrast, regu = regu)
  bst <- bootstrap_sample(data, contrast, regu, alternative, analysis, pars) 
  critval <- critval_bootstrap(alpha, alternative, bst)
  alpha_adj <- alpha_bootstrap(alpha, alternative, bst)
  
  ## output:
  stats2results(
    stats = stats,
    alpha = alpha,
    alpha_adj = alpha_adj,
    critval = critval,
    pval_fun = pval_bootstrap,
    pval_args = list(bst=bst),
    benchmark = benchmark,  
    alternative = alternative,
    analysis = analysis,
    transformation = transformation,
    attrs = attrs
  )
  
}

permute_matrix <- function(x, margin=2){
  
  apply(x, margin, function(xj){
    xj[sample(length(xj))]
  })
  
}