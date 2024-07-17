#' Compare predictions and labels
#'
#' @param predictions (numeric | character) \cr vector of class predictions, class and unique values+
#' need to match those of \code{labels}.
#' @param labels (numeric | character) \cr vector of true class labels (reference standard)
#' @param partition (logical) \cr should result be split into one matrix per class (TRUE; default) or not (FALSE)
#' @param names (character | NULL) \cr named character. Values specify data values, names specify class names.
#' If \code{names=NULL}, the values and names are defined as \code{unique(labels)}
#'
#' @return (list | matrix) \cr list of matrices (one for each unique value of \code{labels}) with
#' values 1 (correct prediction) and 0 (false prediction).
#' If \code{partition=TRUE}, the matrices are combined in a single matrix with \code{rbind}.
#' @export
#'
#' @examples
#' pred <- matrix(c(1, 1, 0), 5, 3)
#' labels <- c(1, 1, 0, 0, 1)
#' compare(pred, labels, FALSE)
#' compare(pred, labels, TRUE)
compare <- function(predictions,
                    labels,
                    partition = TRUE,
                    names = c(specificity = 0, sensitivity = 1)) {
  if (is.null(names)) {
    names <- unique(labels)
    names(names) <- unique(labels)
  }

  if (!(is.matrix(predictions) | is.data.frame(predictions))) {
    predictions <- matrix(predictions)
  }

  # stopifnot(is.numeric(labels))
  stopifnot(all(!duplicated(names)))
  stopifnot(all(sort(unique(labels)) == sort(unique(names))))
  stopifnot(nrow(predictions) == length(labels))

  comp <- matrix(rep(labels, ncol(predictions)), byrow = FALSE, ncol = ncol(predictions))
  out <- as.data.frame(1 * (predictions == comp))

  if (partition) {
    out <- split(out, labels)
    if (!is.null(names)) {
      names(out) <- names(names)[match(names(out), names)]
    }
  }
  return(out)
}
