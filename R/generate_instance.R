#' @title Generate data sets under least favorable parameter configurations
#' 
#' @description Generates a (simulation) instance, a list of multiple datasets to be processed
#' (analyzed) with \link{process_instance}. Ground truth parameters (Sensitvity & Specificity) are 
#' least-favorable in the sense that the type-I error rate of the subsequently applied 
#' multiple test procedures is maximized.
#'
#' @param data ignored (for batchtools compatibility)
#' @param job ignored (for batchtools compatibility)
#' @param nrep integer, number of instances
#' @param n integer, total sample size
#' @param prev numeric, disease prevalence
#' @param random logical, fixed prevalence (FALSE) or simple random sampling (TRUE)
#' @param m integer, number of candidates
#' @param se numeric
#' @param sp numeric
#' @param L numeric
#' @param rhose numeric
#' @param rhosp numeric
#' @param cortype character, "equi" or "ak1"
#' @param ... further arguments
#' 
#' @return a list, a single (LFC) simulation instance
#' 
#' @details Utilizes same arguments as \link{draw_data_lfc} unless mentioned above.
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
                                  data=NULL,
                                  job=NULL){
  
  prev <- c(prev, 1-prev)
  B <- round(m/2)
  Rse <- do.call(paste0("cormat_", cortype), list(m=m, rho=rhose))
  Rsp <- do.call(paste0("cormat_", cortype), list(m=m, rho=rhosp))
  
  args <- preproc_args(as.list(environment()),
                       c("nrep", "...", "data", "job",
                         "rhose", "rhosp", "cortype"),
                       NULL)
  
  lapply(1:nrep, function(x)
    do.call(draw_data_lfc, args))
}


#' @title Generate data sets under realistic parameter configurations
#' @description Generates a (simulation) instance, a list of multiple datasets to be processed
#' (analyzed) with \link{process_instance}. Ground truth parameters (Sensitvity & Specificity) are 
#' initially generated according to a generative model whereby multiple decision rules (with
#' different parameter values) are derived by thresholding multiple biomarkers.
#' 
#' @param data ignored (for batchtools compatibility)
#' @param job ignored (for batchtools compatibility)
#' @param nrep integer, number of instances
#' @param n integer, total sample size
#' @param prev numeric, disease prevalence
#' @param random logical, fixed prevalence (FALSE) or simple random sampling (TRUE)
#' @param m integer, number of candidates
#' @param auc numeric
#' @param rhose numeric
#' @param rhosp numeric
#' @param dist character
#' @param e numeric
#' @param k numeric
#' @param delta numeric
#' @param ... further arguments
#' 
#' @return a list, a single (ROC) simulation instance
#' 
#' @details Utilizes same arguments as \link{draw_data_roc} unless mentioned above.
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
                                  data=NULL,
                                  job=NULL){
  
  prev <- c(prev, 1-prev)
  B <- round(m/2)
  rho <- c(rhose, rhosp)
  corrplot <- FALSE
  
  args <- preproc_args(as.list(environment()),
                       c("nrep", "...", "data", "job",
                         "rhose", "rhosp"), 
                       c("auc"))
  
  lapply(1:nrep, function(x)
    do.call(draw_data_roc, args))
  
}


preproc_args <- function(args, excl, conv){
  ## eclude specific arguments:
  args <- args[!names(args) %in% excl]
  ## convert string inputs in simulation:
  if(!is.null(conv)){
    args[names(args) %in% conv] <-
      lapply(args[names(args) %in% conv], function(x)
        switch(class(x),
               character = eval(parse(text = x)),
               x))
  }
  return(args)
}
