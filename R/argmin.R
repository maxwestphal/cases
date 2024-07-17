argmin <- function(x, rdm = FALSE) {
  am <- which(x == min(x))
  # deterministic output, if required, otherwise randomize in case of ties
  if (!rdm) {
    return(min(am))
  }
  if (length(am) == 1) {
    return(am)
  } else {
    return(sample(am, 1))
  }
}

pargmin <- function(..., args = list(), rdm = FALSE) {
  args <- c(list(...), args)
  stopifnot(is.list(args))
  stopifnot(do.call(all.equal, lapply(args, length)))
  apply(as.data.frame(args), 1, argmin, rdm = rdm)
}

argmax <- function(x, rdm = FALSE) {
  argmin(-x, rdm)
}

pargmax <- function(..., args = list(), rdm = FALSE) {
  args <- c(list(...), args)
  args <- lapply(args, function(x) {
    -x
  })
  pargmin(args = args, rdm = rdm)
}
