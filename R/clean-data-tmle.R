clean_data_tmle =
  function(covars_df,
           outcome_vec,
           treatment_vec,
           prescreen,
           verbose = verbose) {
    
    # Remove constant columns from the covariate file.
    constant_columns = which(apply(covars_df, MARGIN = 2L, var) == 0)
    
    if(length(constant_columns)>0){
      covars_df = covars_df[, -constant_columns, drop = FALSE]
    }
    
    if (verbose) {
      cat("Removed", length(constant_columns), "constant columns from the covariate file.\n")
    }
    
    rm(constant_columns)  
    
  #Remove linearly correlated columns from the covariate file?
  linear_combos = caret::findLinearCombos(covars_df)
  remove_columns = linear_combos$remove
  
  if (length(linear_combos$remove) > 0) {

    if (verbose) {
      cat("Removing", length(linear_combos$remove), "Covariates due to collinearity:\n")
      cat(paste0(colnames(covars_df)[linear_combos$remove - 1L], collapse = ", "), "\n")
      cat("Indices:", paste(linear_combos$remove, collapse = ", "), "\n") 
    }
    
    # Make sure we don't switch to a vector if only 1 column remains.
    # Subtract 1L because y is the first column in the caret analysis.
    covars_df = covars_df[, !colnames(covars_df) %in% colnames(covars_df)[linear_combos$remove - 1L], 
                          drop = FALSE]
  } else {
    cat("No linear duplication found.\n")
  }
  
  rm(linear_combos)
  
  # Compute covariance matrix.
  cov_mat = cov(covars_df)
  
  # Compute QR decomp of covariance matrix.
  qr_cov = qr(cov_mat)
  
  # These need to be equal for the covariance matrix to be full rank.
  if (ncol(cov_mat) != qr_cov$rank && verbose) {
    cat("Warning: covariance of covariate matrix is not full rank.\n")
    cat("Covariance columns:", ncol(cov_mat), "QR rank:", qr_cov$rank, "\n")
  } else {
    cat("Full specification propensity score is full rank.\n")
  }
  rm(cov_mat, qr_cov)
  
  #Optional prescreening
  if(prescreen){
    if (verbose) cat("Keep covariates with univariate associations. \n")
    
    # Identify non-binary variables.
    nonbinary <- apply(covars_df,2,function(x) { length(unique(covars_df))>2 })
    
    #Keep for all variables with default values
    keep <- which(prescreen.uni(outcome_vec, treatment_vec, covars_df))
    
    keep.nonbinary<-data.frame(t(subset(t(nonbinary),select=keep)))
    names(keep.nonbinary)<-"val"
    
    keep.nonbin_sub<-subset(keep.nonbinary, val=="TRUE")
    keep.nonbinary<-names(data.frame(covars_df)) %in% row.names(keep.nonbin_sub)
    
    # Initialize to NULL so that cbind() will still work.
    Wsq = NULL
    
    if (length(which(keep.nonbinary)) > 0) {
      
      new<-cbind.data.frame(names(data.frame(covars_df[, keep.nonbinary])),
                            prescreen.uni(outcome_vec, treatment_vec, covars_df[, keep.nonbinary]^2, 
                                          alpha=prescreen[1], min = 0))
      names(new)<-c("name","val")
      
      keep.nonbin_sub<-subset(new, val=="TRUE")
      keep.sq<-names(data.frame(covars_df)) %in% keep.nonbin_sub$name
      
      if (sum(keep.sq) > 0) {
        Wsq <- covars_df[, keep.sq, drop = FALSE]^2
        colnames(Wsq) <- paste0(colnames(Wsq), "sq")
      }
    }
    
    # Add squared terms to covars_df.
    covars_df <- cbind(covars_df[, keep], Wsq)
    n.columns <- ncol(covars_df)
    
    }else {
      if (verbose) cat("Keep all covariates. \n")
      
      Wsq <- covars_df^2
      colnames(Wsq) <- paste0(colnames(Wsq), "sq")
      
      # Add squared terms to X.
      covars_df<-cbind(covars_df, Wsq)
      n.columns <- ncol(covars_df)
    }

  results = list(
    covariate_df = covars_df
  )
  return(results)
} 

# keep covariates with univariate associations
prescreen.uni <- function(Y, A, X, alpha = .05, min = 5, ...){
  pvalues <- rep(NA, ncol(X))
  for (i in 1:ncol(X)){
    x=X[,i]
    if (var(x)==0|sum(x==0)>=(length(x)-1)|sum(x==1)>=(length(x)-1)) pvalues[i]=1 else {
      m <- lm(Y~ A+ X[,i])
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
