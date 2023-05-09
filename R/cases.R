#' \code{cases} package
#'
#' Enables simultaneous statistical inference for the accuracy of multiple classifiers in multiple subgroups (strata). For instance, allows to perform multiple comparisons in diagnostic accuracy studies with co-primary endpoints sensitivity and specificity. (Westphal, Max, and Antonia Zapf. "Statistical Inference for Diagnostic Test Accuracy Studies with Multiple Comparisons." <arXiv:2105.13469> (2021).)
#'
#' See the vignettes
#' vignette()
#'
#' @docType package
#' @name cases
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))