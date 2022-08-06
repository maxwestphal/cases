setattr <- function(x, ..., attrlist=list()){
  a <- c(list(...), attrlist)
  n <- names(a)
  for(i in 1:length(a)){
    attr(x, n[i]) <- a[[i]]
  }
  return(x)
}