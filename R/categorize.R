#' Categorize continuous values
#' 
#' This function allows to split continuous values, e.g. (risk) scores or (bio)markers,
#' into two or more categories by specifying one or more cutoff values.
#' 
#' @param values numeric matrix of continuous values to be categorized.
#' Assume an (n x r) matrix with n observations (subjects) of r continuous values.
#' @param cutoffs numeric matrix of dimension m x k. Each row of cutoffs defines a split
#' into k+1 distinct categories. Each row must contain distinct values. In the simplest case,
#' cutoffs is a single column matrix whereby is row defines a binary split (<=t vs. >t).
#' In this case (k=1), cutoffs can also be a numeric vector.
#' @param map integer vector of length k with values in 1:r, whereby r = ncol(values).
#' map_l gives the value which column of values should be categorized by ...
#' @param labels character of length m (= number of prediction r)
#'
#' @return numeric (n x k) matrix with categorical outcomes after categorizing.
#' @export
#'
#' @examples
#' set.seed(123)
#' M <- as.data.frame(mvtnorm::rmvnorm(20, mean=rep(0, 3), sigma=2*diag(3)))
#' M
#' categorize(M)
#' C <- matrix(rep(c(-1, 0, 1, -2, 0, 2), 3), ncol=3, byrow = TRUE)
#' C
#' w <- c(1, 1, 2, 2, 3, 3)
#' categorize(M, C, w)
categorize <- function(values,
                       cutoffs = rep(0, ncol(values)),
                       map = 1:ncol(values), 
                       labels = NULL){
  stopifnot(is.matrix(cutoffs) | is.numeric(cutoffs))
  
  values <- as.data.frame(values)
  cutoffs <- as.matrix(cutoffs)
  labels <- preproc_labels(labels, cutoffs, map)
  
  stopifnot(all(map %in% 1:ncol(values)))
  stopifnot(length(map) == nrow(cutoffs))
  if(is.null(names(values))){names(values) <- paste0("v", 1:ncol(values))}
  
  # if(!all(apply(cutoffs, 1, function(x) length(x) == length(unique(x))))){
  #   stop("Marker split cannot be based on duplicate cutoffs.")
  # }
  
  C <- as.data.frame(matrix(NA, nrow=nrow(values), ncol=nrow(cutoffs)))
  
  for(k in 1:nrow(cutoffs)){
    C[, k] <- categorize1(values[, map[k]], cutoffs[k, ])
    
    names(C)[k] <- paste0(names(values)[map[k]], "_", labels[k])
  }
  return(C)
}


categorize1 <- function(x, cutoffs){
  sapply(x, function(xi) sum(xi>cutoffs))
}


preproc_labels <- function(labels, cutoffs, map){
  if(!is.null(labels)){
    stopifnot(is.character(labels))
    stopifnot(length(labels) == length(map))
  }
  if(is.null(labels)){
    if(ncol(cutoffs)==1){
      labels <- as.character(cutoffs)
    }else{
      labels <- sapply(1:nrow(cutoffs), function(x) letters[sum(map[1:x] == map[x])])
    }
  }
}

