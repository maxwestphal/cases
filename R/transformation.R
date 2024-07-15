get_tf <- function(transformation = c("none", "logit", "arcsin")) {
  transformation <- match.arg(transformation)
  tf <- list()

  if (transformation == "none") {
    id <- function(x, ...) {
      x
    }
    tf$est_link <- id
    tf$est_inv <- id
    tf$se_link <- id
  }
  if (transformation == "logit") {
    tf$est_link <- function(x) {
      log(x / (1 - x))
    }
    tf$est_inv <- function(y) {
      1 / (1 + exp(-y))
    }
    tf$se_link <- function(se, n, est) {
      sqrt(abs(1 / (n * est * (1 - est))))
    }
  }
  if (transformation == "arcsin") {
    tf$est_link <- function(x) {
      asin(sqrt(x))
    }
    tf$est_inv <- function(y) {
      sapply(y, function(z) {
        if (z == -Inf) {
          return(0)
        }
        if (z == +Inf) {
          return(1)
        }
        (sin(z))^2
      })
    }
    tf$se_link <- function(se, n, est) {
      1 / n
    }
  }

  return(tf)
}
