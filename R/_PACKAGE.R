#' @title
#' \code{cases} package
#' 
#' @description
#' Enables simultaneous statistical inference for the accuracy of multiple classifiers
#' in multiple subgroups (strata). For instance, allows to perform multiple comparisons
#' in diagnostic accuracy studies with co-primary endpoints
#' sensitivity (true positive rate, TPR) and specificity (true negative rate, TNR).
#' 
#' @details
#' The package functionality and syntax is described in the vignettes, see examples.
#' 
#' @references
#' Westphal M, Zapf A.
#' Statistical inference for diagnostic test accuracy studies with multiple comparisons.
#' Statistical Methods in Medical Research. 2024;0(0).
#' \href{https://journals.sagepub.com/doi/full/10.1177/09622802241236933}{doi:10.1177/09622802241236933}
#' 
#' @examples
#' # overview over package functionality:
#' vignette("package_overview")
#'
#' # a real-world data example:
#' vignette("example_wdbc")
#' 
#' @docType package
#' @aliases cases-package
#' @name cases
"_PACKAGE"
