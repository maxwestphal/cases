#' Evaluate the accuracy of multiple (candidate) classifiers in several subgroups
#' 
#' Assess classification accuracy of multiple classifcation rules stratified 
#' by subgroups, e.g. in diseased (sensitivity) and healthy (specificity)
#' individuals.
#'
#' @param data list of n_g x m binary matrix or data.frame (n_g observations of m binary decisions),
#' g is the index of subgroups/classes, usually created via \code{\link{compare}}.
#' @param contrast \code{cases_contrast} object, specified via \code{\link{define_contrast}}
#' @param benchmark value to compare against (RHS), should have same length as data.
#' @param alpha numeric, significance level (default: 0.05)
#' @param alternative character, specify alternative hypothesis
#' @param adjustment character, specify type of statistical adjustment taken to address multiplicity
#' @param transformation character, define transformation to ensure results 
#' (e.g. point estimates, confidence limits) lie in unit interval ("none" (default) or "logit")
#' @param analysis character, "co-primary" or "full"
#' @param regu numeric vector of length 3, specify type of shrinkage.
#' Alternatively, logical of length one (TRUE := c(2, 1, 1/2), FALSE := c(0, 0, 0))
#' @param pars further parameters given as named list
#' @param ... additional named parameters
#'
#' @return cases_results object, which is a list of analysis results
#' @details 
#' Adjustment methods:
#' - "none" (default): no adjustment for multiplicity
#' - "bonferroni": Bonferroni adjustment
#' - "maxt": maxT adjustment
#' - "bootstrap": Bootstrap approach, whereby several details can be set via the 
#' 'pars' argument (see below)
#' - "mbeta": This heuristic Bayesian approach is based on a multivariate beta-binomial model.
#' 
#' Additional parameters to be adjusted via 'pars=list(par1=val1, par2=val2, ...)', e.g. list(type="pairs", nboot=10000), alternatively via '...'
#' - type: "pairs" (default) or "wild" = type of bootstrap
#' - nboot = number of bootstrap draws (default 5000)
#' - res_tra = 0,1,2 or 3 = type of residual transformation of wild boostrap (default = 0: no transformation)
#' (see https://www.math.kth.se/matstat/gru/sf2930/papers/wild.bootstrap.pdf)
#'
#' @export
#'
#' @examples#
#' data <- draw_data_roc()
#' evaluate(data)
evaluate <- function(data,
                     contrast = define_contrast("raw"),
                     benchmark = 0.75, 
                     alpha = 0.05, 
                     alternative = c("two.sided", "greater", "less"), 
                     adjustment = c("none", "bonferroni", "maxt", "bootstrap", "mbeta"),
                     transformation = c("none", "logit"),
                     analysis = c("co-primary", "full"),
                     regu = FALSE,
                     pars = list(),
                     ...) {
  
  ## check 'data' argument:
  stopifnot(is.list(data))
  stopifnot(all(sapply(data, function(x) 
    any(class(x) %in% c("data.frame", "matrix")))))
  if(any(sapply(data, function(x) any(class(x) == "data.frame")))){
    data <- lapply(data, as.matrix)
  }
  stopifnot(all(diff(sapply(data, ncol))==0))
  if(!all(apply(sapply(data, colnames) %>% matrix(nrow=ncol(data[[1]])), 1,
                function(x) length(unique(x))==1 ))){
    stop("Expecting identical column names!")
  }
  
  ## check 'contrast' argument:
  stopifnot("cases_contrast" %in% class(contrast))
  
  ## check 'benchmark' argument:
  if(! (length(benchmark) %in% c(1, length(data)))){
    stop("benchmark argument needs to be numeric of length one or of same length as data argument!")
  }
  if(length(benchmark) == 1){
    benchmark <- rep(benchmark, length(data))
  }
  stopifnot(all(abs(benchmark) < 1))
  
  ## check 'alpha' argument:  
  stopifnot(is.numeric(alpha))
  stopifnot(length(alpha) == 1)
  stopifnot(alpha > 0 & alpha < 1)
  
  ## check 'pars' argument
  stopifnot(is.list(pars))
  
  ## prepare arguments for specific evaluate_xyz function:
  args <-
    list(
      data = data,
      contrast = contrast,
      benchmark = benchmark,
      alpha = alpha,
      alternative = match.arg(alternative),
      analysis = match.arg(analysis),
      transformation = match.arg(transformation),
      regu = preproc_regu(regu),
      pars = c(list(...), pars)
    )
  
  ## calculate & label result:
  do.call(paste0("evaluate_", match.arg(adjustment)), args) %>% 
    setattr(class = c("list", "cases_results"),
            names = names(data),
            contrast = attr(contrast, "contrast"),
            attrlist = args[-(1:2)]) %>% 
    return()
  
}
