preproc_regu <- function(regu = "2_1_0.5") {
  if (is.logical(regu)) {
    if (!regu) {
      return(c(0, 0, 0))
    }
    if (regu) {
      return(c(1, 1 / 2, 1 / 4))
    }
  }
  if (is.character(regu)) {
    regu <- as.numeric(strsplit(regu, "_")[[1]])
  }
  if (is.numeric(regu) & length(regu) == 1) {
    regu <- c(regu, regu / 2, regu / 4)
  }
  stopifnot(is.numeric(regu))
  stopifnot(length(regu) == 3)
  stopifnot(all(regu >= 0))
  return(regu)
}
