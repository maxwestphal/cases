#' @title Generate data sets under least favorable parameter configurations
#'
#' @description Generates a (simulation) instance, a list of multiple datasets to be processed
#' (analyzed) with \link{process_instance}. Ground truth parameters (Sensitvity & Specificity) are
#' least-favorable in the sense that the type-I error rate of the subsequently applied
#' multiple test procedures is maximized.
#'
#' \strong{This function is only needed for simulation via batchtools, not relevant in interactive use!}
#'
#' @param data (NULL) \cr ignored (for batchtools compatibility)
#' @param job (NULL) \cr ignored (for batchtools compatibility)
#' @param nrep (numeric) \cr integer, number of instances
#' @param n (numeric) \cr integer, total sample size
#' @param prev (numeric) \cr disease prevalence
#' @param random (logical) \cr fixed prevalence (FALSE) or simple random sampling (TRUE)
#' @param m (numeric) \cr integer, number of candidates
#' @param se (numeric) \cr sensitivity
#' @param sp (numeric) \cr specificity
#' @param L (numeric) \cr worst alternative is computed under side condition Acc <= L
#' @param rhose (numeric) \cr correlation parameter for sensitivity
#' @param rhosp (numeric) \cr correlation parameter for specificity
#' @param cortype (character) \cr  correlation type ("equi" or "ak1")
#' @param ... (any) \cr further (named) arguments
#'
#' @return (list) \cr a single (LFC) simulation instance of length \code{nrep}
#'
#' @details Utilizes same arguments as \link{draw_data_lfc} unless mentioned otherwise above.
#'
#' @export
generate_instance_lfc <- function(nrep = 10,
                                  n = 100,
                                  prev = 0.5,
                                  random = FALSE,
                                  m = 10,
                                  se = 0.8,
                                  sp = 0.8,
                                  L = 1,
                                  rhose = 0,
                                  rhosp = 0,
                                  cortype = "equi",
                                  ...,
                                  data = NULL,
                                  job = NULL) {
  prev <- c(prev, 1 - prev)
  B <- round(m / 2)
  Rse <- do.call(paste0("cormat_", cortype), list(m = m, rho = rhose))
  Rsp <- do.call(paste0("cormat_", cortype), list(m = m, rho = rhosp))

  args <- preproc_args(
    as.list(environment()),
    c(
      "nrep", "...", "data", "job",
      "rhose", "rhosp", "cortype"
    ),
    NULL
  )

  lapply(1:nrep, function(x) {
    do.call(draw_data_lfc, args)
  })
}


#' @title Generate data sets under realistic parameter configurations
#'
#' @description Generates a (simulation) instance, a list of multiple datasets to be processed
#' (analyzed) with \link{process_instance}. Ground truth parameters (Sensitvity & Specificity) are
#' initially generated according to a generative model whereby multiple decision rules (with
#' different parameter values) are derived by thresholding multiple biomarkers.
#'
#' \strong{This function is only needed for simulation via batchtools, not relevant in interactive use!}
#'
#'
#' @param data (NULL) \cr ignored (for batchtools compatibility)
#' @param job (NULL) \cr ignored (for batchtools compatibility)
#' @param nrep (numeric) \cr integer, number of instances
#' @param n (numeric) \cr integer, total sample size
#' @param prev (numeric) \cr disease prevalence
#' @param random (logical) \cr fixed prevalence (FALSE) or simple random sampling (TRUE)
#' @param m (numeric) \cr integer, number of candidates
#' @param auc (numeric) \cr vector of AUCs of biomarkers
#' @param rhose (numeric) \cr correlation parameter for sensitivity
#' @param rhosp (numeric) \cr correlation parameter for specificity
#' @param dist (character) \cr either "normal" or "exponential" specifying the subgroup biomarker distributions
#' @param e (numeric) \cr emulates better (worse) model selection quality with higher (lower) values of e
#' @param k (numeric) \cr technical parameter which adjusts grid size
#' @param delta (numeric) \cr specify importance between sensitivity and specificity (default 0: equal importance)
#' @param ... (any) \cr further arguments
#'
#' @return (list) \cr a single (ROC) simulation instance of length \code{nrep}
#'
#' @details Utilizes same arguments as \link{draw_data_roc} unless mentioned otherwise above.
#'
#' @export
generate_instance_roc <- function(nrep = 10,
                                  n = 100,
                                  prev = 0.5,
                                  random = FALSE,
                                  m = 10,
                                  auc = "seq(0.85, 0.95, length.out = 5)",
                                  rhose = 0.5,
                                  rhosp = 0.5,
                                  dist = "normal",
                                  e = 10,
                                  k = 100,
                                  delta = 0,
                                  ...,
                                  data = NULL,
                                  job = NULL) {
  prev <- c(prev, 1 - prev)
  B <- round(m / 2)
  rho <- c(rhose, rhosp)
  corrplot <- FALSE

  args <- preproc_args(
    as.list(environment()),
    c(
      "nrep", "...", "data", "job",
      "rhose", "rhosp"
    ),
    c("auc")
  )

  lapply(1:nrep, function(x) {
    do.call(draw_data_roc, args)
  })
}


preproc_args <- function(args, excl, conv) {
  ## eclude specific arguments:
  args <- args[!names(args) %in% excl]
  ## convert string inputs in simulation:
  if (!is.null(conv)) {
    args[names(args) %in% conv] <-
      lapply(args[names(args) %in% conv], function(x) {
        switch(class(x),
          character = eval(parse(text = x)),
          x
        )
      })
  }
  return(args)
}
