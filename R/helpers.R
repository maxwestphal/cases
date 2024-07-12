set_attrs <- function(x, ..., attrs=list()){
  a <- c(list(...), attrs)
  n <- names(a)
  for(i in 1:length(a)){
    attr(x, n[i]) <- a[[i]]
  }
  return(x)
}

get_from_pars <- function(pars=list(), name, default){
  if(is.null(pars[[name]])){
    return(default)
  }else{
    return(pars[[name]])
  }
}

derive_attrs <- function(args){
  list(
    class = c("list", "cases_results"),
    names = names(args$data),
    n_obs = sapply(args$data, nrow),
    n_params = nrow(args$contrast(args$data)),
    n_groups = length(args$data),
    critval = NA,
    alpha_adj = NA,
    call = c(
      list(contrast = attr(args$contrast, "contrast")),
      args[-(1:2)]
    )
  )
}

update_attrs <- function(attrs = list(), ...){
  attrs_new <- list(...)
  attrs[names(attrs_new)] <- attrs_new
  attrs
}
