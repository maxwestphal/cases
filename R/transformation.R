

get_tf <- function(transformation = c("none", "logit")){
  transformation = match.arg(transformation)
  tf <- list()
  if(transformation == "none"){
    id <- function(x, ...){x}
    tf$est_link = id
    tf$inv = id
    tf$se_link = id
  }
  if(transformation == "logit"){
    tf$est_link = function(x){log(x/(1-x))}
    tf$inv = function(y){1/(1+exp(-y))}
    tf$se_link = function(se, n, est){ 
      sqrt(abs(1/(n*est*(1-est))))}
  }
  return(tf)
}









