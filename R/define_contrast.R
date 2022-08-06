#' Define a contrast (matrix) to specify exact hypothesis system
#'
#' @param type character, either "Raw", "dunnett" or "tukey")
#' @param comparator either integer (index of comparator) or character (name of comparator)
#' 
#' @details 
#' "raw" contrast: compare all candidates against specified benchmark values
#' 
#' "dunnett" (all vs. one) contrast: compare all candidates to a single comparator. 
#' 
#' "tukey" (all vs. all) contrast: compare all candidates against each other.
#'
#' @return \code{cases_contrast} object to be passed to \code{\link{evaluate}}
#' @export
#'
#' @examples 
#' define_contrast("dunnett", 1)
#' @importFrom multcomp contrMat
define_contrast <- function(type = c("raw", "dunnett", "tukey"), comparator=NA){
  type <- match.arg(type)
  
  fun <- function(data=NULL,
                  n=colnames(data[[1]])
                  ){
    stopifnot(!is.null(data) | !is.null(n))
    
    x <- rep(1, length(n))
    names(x) <- n
  
    if(type == "raw"){
      K <- diag(x)
      rownames(K) <- colnames(K) <- names(x)
    }
    if(type == "dunnett"){
      stopifnot(comparator %% 1 == 0 & comparator >= 1)
      if(is.numeric(comparator)){
        stopifnot(comparator %in% 1:length(n))
        b <- comparator
        comparator <- n[b]
      }
      if(is.character(comparator)){
        stopifnot(comparator %in% n)
        b <- which(comparator == n)
      }
      K <- multcomp::contrMat(x, type = type, base=b)
    }
    if(type == "tukey"){
      K <- multcomp::contrMat(x, type = type, base=1)
    }
    
    class(K) <- "matrix"
    attr(K, "type") <- type
    attr(K, "comparator") <- ifelse(type %in% c("dunnett"), comparator, NA)
    return(K)
  }
  class(fun) <- append(class(fun), "cases_contrast")
  attr(fun, "contrast") <- c(type, comparator)
  return(fun)
}
