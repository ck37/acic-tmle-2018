# Identify covariates meeting a certain threshold for correlation
# with the outcome, after also adjusting for treatment indicator.
# TODO: unify the second version of this function below with this
# so that we don't need two different versions.
#' @return Returns a vector of booleans, with TRUE if the covariate passes the screen.
prescreen_uni <- function(Y, A, X, alpha = .05, min = 5, ...) {
  pvalues <- rep(NA, ncol(X))
  # Loop over each covariate in X.
  for (i in 1:ncol(X)) {
    # Select the current covariate.
    x_i = X[, i]
    # TODO: doesn't assume no missing values.
    if (var(x_i) == 0 |
        # If there is only a single non-0 value
        sum(x_i == 0) >= (length(x_i) - 1) |
        # If there is only a single non-1 value
        sum(x_i == 1) >= (length(x_i) - 1)) {
      pvalues[i] = 1
    } else {
      # OLS regression of Y on treatment + current covariate.
      lm_fit <- lm(Y ~ A + x_i)
      # Try to extract the p-value on the x_i variable.
      p <- try(summary(lm_fit)$coef[3,4], silent = TRUE)
      if (class(p) == "try-error") {
        pvalues[i] <- 1
      } else {
        pvalues[i] <- p
      }
    }
  }
  
  # Select covariates that meet the correlation p-value threshold.
  keep_bools <- pvalues <= alpha
  # Handle NAs even though we should not have any.
  if (sum(keep_bools, na.rm = TRUE) < min) {
    # If we don't have enough covariates ensure that we at least
    # keep the minimum number.
    keep_bools[order(pvalues)[1:min]] <- TRUE
  }
  return(keep_bools)
}

# TODO: please comment this function
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
  # For one dataset we are getting NAs in keep, unclear why.
  if (sum(keep, na.rm = TRUE) < min) {
    # Any NAs will go to the end of order.
    keep[order(pvalues)[1:min]] <- TRUE
  }
  return(keep)
}