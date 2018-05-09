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
    
  # Remove linearly correlated columns from the covariate file
  linear_combos = caret::findLinearCombos(covars_df)
  remove_columns = linear_combos$remove
  
  if (length(linear_combos$remove) > 0) {

    if (verbose) {
      cat("Removing", length(linear_combos$remove), "covariates due to collinearity:\n")
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
  
  # Optional prescreening
  # Separately for treatment and outcome
  if (prescreen) {
    if (verbose) cat("Keep covariates with univariate associations. \n")
    
    # Identify non-binary variables.
    nonbinary <- apply(covars_df,2,function(x) { length(unique(covars_df))>2 })
    
    # Keep for all variables with default values
    keep <- which(prescreen_uni(outcome_vec, treatment_vec, covars_df))
    keepA <- which(prescreen_uniA(treatment_vec, covars_df))
    
    keep.nonbinary<-data.frame(t(subset(t(nonbinary),select=keep)))
    names(keep.nonbinary)<-"val"
    keep.nonbinaryA<-data.frame(t(subset(t(nonbinary),select=keepA)))
    names(keep.nonbinaryA)<-"val"
    
    keep.nonbin_sub<-subset(keep.nonbinary, val=="TRUE")
    keep.nonbinary<-names(data.frame(covars_df)) %in% row.names(keep.nonbin_sub)
    
    keep.nonbin_subA<-subset(keep.nonbinaryA, val=="TRUE")
    keep.nonbinaryA<-names(data.frame(covars_df)) %in% row.names(keep.nonbin_subA)
    
    #Outcome
    Wsq = NULL
    
    if (length(which(keep.nonbinary)) > 0) {
      
      new<-cbind.data.frame(names(data.frame(covars_df[, keep.nonbinary])),
                            prescreen_uni(outcome_vec, treatment_vec, covars_df[, keep.nonbinary]^2, 
                                          alpha=prescreen[1], min = 0))
      names(new)<-c("name","val")
      
      keep.nonbin_sub<-subset(new, val=="TRUE")
      keep.sq<-names(data.frame(covars_df)) %in% keep.nonbin_sub$name
      
      if (sum(keep.sq) > 0) {
        Wsq <- covars_df[, keep.sq, drop = FALSE]^2
        colnames(Wsq) <- paste0(colnames(Wsq), "sq")
      }
    }
    
    #Treatment
    WsqA = NULL
    
    if (length(which(keep.nonbinaryA)) > 0) {
      
      new<-cbind.data.frame(names(data.frame(covars_df[, keep.nonbinaryA])),
                            prescreen_uniA(treatment_vec, covars_df[, keep.nonbinaryA]^2, 
                                          alpha=prescreen[1], min = 0))
      names(new)<-c("name","val")
      
      keep.nonbin_sub<-subset(new, val=="TRUE")
      keep.sq<-names(data.frame(covars_df)) %in% keep.nonbin_sub$name
      
      if (sum(keep.sq) > 0) {
        WsqA <- covars_df[, keep.sq, drop = FALSE]^2
        colnames(WsqA) <- paste0(colnames(WsqA), "sq")
      }
    }
    
    # Add squared terms to covars_df.
    covars_df <- cbind(covars_df[, keep], Wsq)
    n.columns <- ncol(covars_df)
    
    covars_dfA <- cbind(covars_df[, keepA], WsqA)
    n.columnsA <- ncol(covars_dfA)
    
    }else {
      if (verbose) cat("Keep all covariates. \n")
      
      Wsq <- covars_df^2
      colnames(Wsq) <- paste0(colnames(Wsq), "sq")
      
      # Add squared terms to X.
      covars_df<-cbind(covars_df, Wsq)
      n.columns <- ncol(covars_df)
      
      covars_dfA<-cbind(covars_df, Wsq)
      n.columnsA <- ncol(covars_dfA)
      
    }

  results = list(
    covariate_df = covars_df,
    covariate_dfA = covars_dfA
  )
  return(results)
} 

