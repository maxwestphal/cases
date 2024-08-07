#' @export
print.cases_results <- function(x, info = FALSE, digits = 4, ...) {
  message("[cases] evaluation results:")

  x <- lapply(x, function(xg) dplyr::mutate_if(xg, is.numeric, round, digits))

  if (!info) {
    n <- names(x)
    attributes(x) <- NULL
    names(x) <- n
  }

  print.default(x)
}


#' @export
print.cases_contrast <- function(x, ...) {
  cat(paste0(
    ">> cases_contrast object of type '", attr(x, "type"),
    "' (comparator: ", attr(x, "comparator"), ") <<\n"
  ))
}
