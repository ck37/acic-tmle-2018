# keep covariates with univariate associations
prescreen_uni <- function(Y, A, X, alpha = .05, min = 5, ...){
  pvalues <- rep(NA, ncol(X))
  for (i in 1:ncol(X)){
    x=X[,i]
    if (var(x)==0|sum(x==0)>=(length(x)-1)|sum(x==1)>=(length(x)-1)) pvalues[i]=1 else {
      m <- lm(Y~ A + X[,i])
      p <- try(summary(m)$coef[3,4], silent = TRUE)
      if (class(p) == "try-error") {
        pvalues[i] <- 1
      } else {
        pvalues[i] <- p
      }
    }
  }
  keep <- pvalues <= alpha
  if(sum(keep) < min){
    keep[order(pvalues)[1:min]] <- TRUE
  }
  return(keep)
}


prescreen_uniA <- function(A, X, alpha = .05, min = 5, ...){
  pvalues <- rep(NA, ncol(X))
  for (i in 1:ncol(X)){
    x=X[,i]
    if (var(x)==0|sum(x==0)>=(length(x)-1)|sum(x==1)>=(length(x)-1)) pvalues[i]=1 else {
      m <- lm(A ~ X[,i])
      p <- try(summary(m)$coef[2,4], silent = TRUE)
      if (class(p) == "try-error") {
        pvalues[i] <- 1
      } else {
        pvalues[i] <- p
      }
    }
  }
  keep <- pvalues <= alpha
  if(sum(keep) < min){
    keep[order(pvalues)[1:min]] <- TRUE
  }
  return(keep)
}