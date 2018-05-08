prescreen_nosqr <- function(Y, X, ...){
  whichVariable <- rep(TRUE, NCOL(X))
  whichVariable <- apply(X, 2, FUN = function(x) {
    if (var(x)==0|sum(x==0)>=(length(x)-1)|sum(x==1)>=(length(x)-1)) return(FALSE) else return(TRUE)
  })
  omit <- grep("sq", colnames(X))
  if(length(omit) > 0){
    whichVariable[omit] <- FALSE
  }
  return(whichVariable)
}