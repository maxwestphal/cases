setattr <- function(x, ..., attrlist=list()){
  a <- c(list(...), attrlist)
  n <- names(a)
  for(i in 1:length(a)){
    attr(x, n[i]) <- a[[i]]
  }
  return(x)
}

get_from_pars <- function(name, default, pars=list()){
  if(is.null(pars[[name]])){
    return(default)
  }else{
    return(pars[[name]])
  }
}