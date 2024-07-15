#' @title Analyze simulated synthetic datasets.
#' @description Process data instances, a list of multiple datasets generated via
#' \link{generate_instance_lfc} or \link{generate_instance_roc}. This function
#' applies \link{evaluate} to all datasets.
#'
#' @param instance generated via \link{generate_instance_lfc} or \link{generate_instance_roc}.
#' @param contrast \code{cases_contrast} object, specified via \code{\link{define_contrast}}
#' @param benchmark value to compare against (RHS), should have same length as data.
#' @param alpha numeric, significance level (default: 0.05)
#' @param alternative character, specify alternative hypothesis
#' @param adjustment character, specify type of statistical adjustment taken to address multiplicity
#' @param transformation character, define transformation to ensure results
#' (e.g. point estimates, confidence limits) lie in unit interval ("none" (default) or "logit")
#' @param analysis character, "co-primary" (default; only option currently)
#' @param regu numeric vector of length 3, specify type of shrinkage.
#' Alternatively, logical of length one (TRUE := c(2, 1, 1/2), FALSE := c(0, 0, 0))
#' @param pars further parameters given as named list
#' @param ... additional named parameters
#' @param data ignored (for batchtools compatibility)
#' @param job for batchtools compatibility, do not change
#'
#' @details Utilizes same arguments as \link{evaluate} unless mentioned above.
#'
#' @return standardized evaluation results
#' @export
process_instance <- function(instance = NULL,
                             contrast = "cases::define_contrast('raw', NA)",
                             benchmark = 0.5,
                             alpha = 0.05,
                             alternative = "greater",
                             adjustment = "none",
                             transformation = "none",
                             analysis = "co-primary",
                             regu = "c(1,1/2,1/4)",
                             pars = "list()",
                             ...,
                             data = NULL,
                             job = list(id = NA)) {
  ## testing:
  # args <- formals(process_instance)
  # args$instance <- generate_instance_lfc(nrep=8)
  # instance <- args$instance

  ## prepare args:
  args <- preproc_args(
    as.list(environment()),
    c("...", "data", "instance", "job"),
    c("contrast", "pars", "regu")
  )

  ## output:
  results <- lapply(instance, function(x) do.call(process1, c(list(data = x), args)))
  return(cbind(job.id = job$id, dataset = 1:length(instance), do.call(rbind, results)))
}

#' @importFrom stats median
process1 <- function(data,
                     contrast = cases::define_contrast("raw", NA),
                     benchmark = 0.5,
                     alpha = 0.05,
                     alternative = "greater",
                     adjustment = "none",
                     transformation = "none",
                     analysis = "co-primary",
                     regu = c(1, 1 / 2, 1 / 4),
                     pars = list()) {
  ## calc evaluation results:
  res <- evaluate(
    data, contrast, benchmark, alpha, alternative, adjustment,
    transformation, analysis, regu, pars
  )

  ## tau = min(Se, Sp):
  info <- attr(data, "info")
  tau <- pmin(info$se, info$sp)
  tau_lower <- pmin(res[[1]]$lower, res[[2]]$lower)
  js <- argmax(tau_lower, rdm = TRUE)

  ## calc threshold values theta0:
  delta <- seq(-0.05, 0.20, 0.01)
  tau_comp <- max(pmin(info$se, info$sp)) - delta

  ## number of rejections (for both Se & Sp):
  rej <- as.data.frame(t(sapply(tau_comp, function(e) {
    sum(tau_lower > e)
  })))
  names(rej) <- delta

  ## false positives w.r.t. two-sided tests (as a control):
  fp <- sum((covered(tau, res[[1]]$lower, res[[1]]$upper) +
    covered(tau, res[[2]]$lower, res[[2]]$upper)) == 0, na.rm = TRUE)

  ## output:
  data.frame(
    se_est_s = res[[1]]$estimate[js],
    sp_est_s = res[[2]]$estimate[js],
    se_lower_s = res[[1]]$lower[js],
    sp_lower_s = res[[2]]$lower[js],
    tau_lower_s = tau_lower[js],
    tau_max = max(tau),
    tau_med = stats::median(tau),
    tau_min = min(tau),
    cv1 = attr(res, "critval")[1],
    cv2 = attr(res, "critval")[2],
    fp = fp
  ) %>%
    cbind(rej)
}
