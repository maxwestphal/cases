#' \code{cases} package
#'
#' Evaluate the accuracy of multiple (candidate) classifiers in several subgroups
#'
#' See the README on
#' \href{https://github.com/maxwestphal/cases}{GitHub}
#'
#' @docType package
#' @name cases
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))