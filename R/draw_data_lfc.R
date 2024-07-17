#' Generate binary data (LFC model)
#'
#' @param n integer, total sample size
#' @param prev numeric, disease and healthy prevalence (adds up to 1)
#' @param random logical, random sampling (TRUE) or fixed prevalence (FALSE)
#' @param m integer, number of models
#' @param se numeric, sensitivity (length 1)
#' @param sp numeric, specificity (length 1)
#' @param B integer, between 1 and m, specifies how many sensitivity values are projected to 1
#' @param L numeric, worst alternative is computed under side condition Acc <= L
#' (default value L=1 corresponds to true LFC where values are projected to 1)
#' @param Rse matrix, correlation matrix for empirical sensitivities (m x m)
#' @param Rsp matrix, correlation matrix for empirical specificities (m x m)
#' @param modnames character, model names (length m)
#' @param ... further arguments (currently unused)
#'
#' @return Generated binary dataset
#' @export
#'
#' @examples
#' data <- draw_data_lfc()
#' head(data)
draw_data_lfc <- function(n = 100,
                          prev = c(0.5, 0.5),
                          random = FALSE,
                          m = 10,
                          se = 0.8,
                          sp = 0.8,
                          B = round(m / 2),
                          L = 1,
                          Rse = diag(rep(1, m)),
                          Rsp = diag(rep(1, m)),
                          modnames = paste0("model", 1:m),
                          ...) {
  ng <- sample_ng(n, prev, random)
  stopifnot(length(ng) == 2)

  n1 <- ng[1]
  n0 <- ng[2]

  if (length(se) == 1) {
    se <- rep(se, m)
  }
  if (length(sp) == 1) {
    sp <- rep(sp, m)
  }

  b <- sample(rep(c(TRUE, FALSE), times = c(B, m - B)))

  if (!all(diff(c(length(se), length(sp), dim(Rse), dim(Rse))) == 0)) {
    stop("Wrong dimensions!")
  }
  if (!is.logical(b) | length(b) != length(se)) {
    stop("Something is wrong with b!")
  }

  comp1 <- matrix(-1, ncol = length(se), nrow = n1)
  comp0 <- matrix(-1, ncol = length(sp), nrow = n0)

  # worst alternative under side condition Acc <= L
  sp.alt <- pmin(1, (L - prev[1] * se) / (1 - prev[1]))
  se.alt <- pmin(1, (L - (1 - prev[1]) * sp) / (prev[1]))

  if (sum(b) > 0) {
    comp0[, b] <- draw_data_prb(n0, sp[b], Rsp[b, b])
    comp1[, b] <- sapply(se.alt[b], function(p) stats::rbinom(n1, 1, p))
  }
  if (sum(!b) > 0) {
    comp1[, !b] <- draw_data_prb(n1, se[!b], Rse[!b, !b])
    comp0[, !b] <- sapply(sp.alt[!b], function(p) stats::rbinom(n0, 1, p))
  }

  if (!all(rbind(comp0, comp1) %in% 0:1)) {
    stop("Something went wrong!!!")
  }

  ## true parameters values
  info <- data.frame(
    model = modnames,
    b = b,
    se = (!b) * se + b * se.alt,
    sp = b * sp + (!b) * sp.alt
  )

  colnames(comp1) <- colnames(comp0) <- modnames
  out <- list(comp0, comp1)
  names(out) <- c("specificity", "sensitivity")
  attr(out, "info") <- info

  return(out)
}
