#' Compare predictions and labels
#'
#' @param predictions integer, predicted class
#' @param labels integer, true class state (reference standard)
#' @param partition logical, should result be split into one matrix per class (TRUE; default) or not (FALSE)
#' @param names integer (named), values give data values, names give class names
#'
#' @return data matrix with values 1 (correct prediction) and 0 (false prediction)
#' @export
#'
#' @examples
#' pred <- matrix(c(1,1,0), 5, 3)
#' labels <- c(1, 1, 0, 0, 1)
#' compare(pred, labels, FALSE)
#' compare(pred, labels, TRUE)
compare <- function(predictions,
                    labels, 
                    partition = TRUE,
                    names = c(specificity=0, sensitivity=1)){
  if(!(is.matrix(predictions) | is.data.frame(predictions)) ){
    predictions <- matrix(predictions)
  }
  stopifnot(is.numeric(labels))
  stopifnot(nrow(predictions) == length(labels))
  
  comp <- matrix(rep(labels, ncol(predictions)), byrow=FALSE, ncol=ncol(predictions))
  out <- as.data.frame(1 * (predictions == comp))
  
  if(partition){
    out <- split(out, labels)
    if(!is.null(names)){
      names(out) <- names(names)[match(names(out), names)]
    }
  }
  return(out)
}