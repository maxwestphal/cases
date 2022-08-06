#' Visualize evaluation results
#'
#' @param x, a \code{cases_results} object, see \code{\link{evaluate}} 
#' @param ... further arguments (currently ignored)
#'
#' @details +++ early development version (only alternative = "greater" is supported) +++ 
#'
#' @return a ggplot
#' @export
#' @importFrom dplyr arrange
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_linerange
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_colour_manual
visualize <- function(x, ...){
  
  ## testing
  # set.seed(1337)
  # data_roc <- draw_data_roc(n=120, prev=c(0.25, 0.75), m=4,
  #                           delta=0.05, e=10, auc=seq(0.025, 0.975, 0.025), rho=c(0.25, 0.25))
  # x <- data_roc %>% evaluate(regu=c(1,1/2,1/4)*2, alt="greater", alpha=0.025, adj="maxt", nboot=5000)
  
  specificity <- sensitivity <- sp_lower <- se_lower <- reject_all <- NULL
  
  ## checks:
  stopifnot(inherits(x, "cases_results"))
  if(length(x) != 2){stop("visualize currently implemented for two subgroups only!")}
  
  ## derived vars:
  G <- length(x)
  nn <- names(x)
  n <- substr(nn, 1, 2)
  
  ## transform data:
  y <- lapply(1:G, function(g){
    d <- data.frame(x[[g]][,c("estimate", "lower", "upper")])
    names(d) <- c(nn[g], paste0(n[g], "_", c("lower", "upper")))
    return(d)
  }) 
  z <- cbind(parameter=x[[1]]$parameter,
             do.call(cbind, y),
             reject_all=x[[1]]$reject_all) %>% 
    dplyr::arrange(reject_all)
  
   
  benchmark <- attr(x, "benchmark")
  sp0 <- benchmark[1]
  se0 <- benchmark[2]
  
  ## plot parameters:
  alpha_cr <- 0.8
  lwd_cr <- 1.25
  
  lwd1 <- 1.5
  lwd2 <- 1.25
  lty <- 2
  
  xmin <- ymin <- 0.0
  xsep <- ysep <- 0.5
  
  col_bench <-  "#4e9c81" 
  col_roi <- "#8dcea7" 
  cols <- c("#ff8b00", "#00a7e1")
  
  ## construct plot
  ggplot2::ggplot() +
    ## plot theme:
    ggplot2::theme(legend.position = "none",
                   title= ggplot2::element_text(face="bold", size=24),
                   axis.title = ggplot2::element_text(face="bold", size=20),
                   legend.text = ggplot2::element_text(face="plain", size=20),
                   legend.title = ggplot2::element_text(face="bold", size=20),
                   axis.text = ggplot2::element_text(size=16)) +
    ggplot2::scale_y_continuous(nn[1], breaks=seq(round(ysep), 1, 0.1))+
    ggplot2::scale_x_continuous(nn[2], breaks=seq(round(xsep), 1, 0.1))+
    ## region of interest:
    ggplot2::geom_rect(ggplot2::aes(xmin=sp0, xmax=1, ymin=se0, ymax=1), alpha = 0.25, fill=col_roi, inherit.aes=F) +
    ## plot/axis setup: 
    ggplot2::geom_linerange(ggplot2::aes(x=1, ymin = ymin, ymax = 1), lwd=lwd1, inherit.aes=F) +
    ggplot2::geom_linerange(ggplot2::aes(y=1, xmin = xmin, xmax = 1), lwd=lwd1, inherit.aes=F) +
    ggplot2::geom_linerange(ggplot2::aes(x=xsep, ymin = ymin, ymax = 1), lwd=lwd1, lty=3, inherit.aes=F) +
    ggplot2::geom_linerange(ggplot2::aes(y=ysep, xmin = xmin, xmax = 1), lwd=lwd1, lty=3, inherit.aes=F) +
    ## region of interest borders:
    ggplot2::geom_linerange(ggplot2::aes(x=sp0, ymin = 0.0, ymax = 1), lwd=lwd1, inherit.aes=F, color=col_bench, lty=lty) +
    ggplot2::geom_linerange(ggplot2::aes(y=se0, xmin = 0.0, xmax = 1), lwd=lwd1, inherit.aes=F, color=col_bench, lty=lty) +
    ggplot2::geom_linerange(ggplot2::aes(x=1, ymin = se0, ymax = 1), lwd=lwd1, inherit.aes=F, color=col_bench, lty=lty) +
    ggplot2::geom_linerange(ggplot2::aes(y=1, xmin = sp0, xmax = 1), lwd=lwd1, inherit.aes=F, color=col_bench, lty=lty) +
    ## comparison regions:
    ggplot2::geom_linerange(data=z, ggplot2::aes(y=sensitivity, xmin = sp_lower, xmax = specificity, colour=reject_all, alpha=reject_all), lwd=lwd_cr, lty=3) +
    ggplot2::geom_linerange(data=z, ggplot2::aes(x=specificity, ymin = se_lower, ymax = sensitivity, colour=reject_all, alpha=reject_all), lwd=lwd_cr, lty=3) +
    ggplot2::geom_linerange(data=z, ggplot2::aes(x=sp_lower, ymin = se_lower, ymax = 1, colour=reject_all, alpha=reject_all, lty=reject_all), lwd=lwd_cr) +
    ggplot2::geom_linerange(data=z, ggplot2::aes(y=se_lower, xmin = sp_lower, xmax = 1, colour=reject_all, alpha=reject_all, lty=reject_all), lwd=lwd_cr) +
    ggplot2::scale_colour_manual(limits=c(FALSE, TRUE), values=cols) +
    ## point estimates:
    ggplot2::geom_point(data=z, ggplot2::aes(specificity, sensitivity, colour=reject_all, shape=reject_all, alpha=reject_all), size=4) +
    ggplot2::scale_alpha_manual(limits=c(FALSE, TRUE), values=c(alpha_cr, 1)) +
    ggplot2::scale_shape_manual(limits=c(FALSE, TRUE), values=c(16, 18)) +
    ggplot2::scale_linetype_manual(limits=c(FALSE, TRUE), values=c(5, 1)) + 
    ggplot2::coord_cartesian(ylim = c(0.5, 1), xlim=c(0.5, 1))

  }

