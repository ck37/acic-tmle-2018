# This function is called by estimate_ate() in R/estimate-ate.R
# TODO: also handle censoring prescreening.
clean_data_tmle =
  function(covars_df,
           outcome_vec,
           treatment_vec,
           prescreen,
           verbose = verbose) {
    if (verbose) {
      cat("\nclean_date_tmle() - begin processing.\n")
    }
    
    # Remove constant columns from the covariate file.
    # NOTE: this won't work for factors, would need to use length(unique())
    # and possibly also remove NAs.
    constant_columns = which(apply(covars_df, MARGIN = 2L, var) == 0)
    
    if(length(constant_columns) > 0) {
      covars_df = covars_df[, -constant_columns, drop = FALSE]
    }
    
    if (verbose) {
      cat("Removed", length(constant_columns), "constant columns from the covariate file.\n")
      cat("Updated covariate count:", ncol(covars_df), "\n")
    }
    
    rm(constant_columns)  
    
  # Remove linearly correlated columns from the covariate file
  linear_combos = caret::findLinearCombos(covars_df)
  remove_columns = linear_combos$remove
  
  if (length(linear_combos$remove) > 0) {

    if (verbose) {
      cat("Removing", length(linear_combos$remove), "covariates due to collinearity.\n")
      #cat(paste0(colnames(covars_df)[linear_combos$remove - 1L], collapse = ", "), "\n")
      # cat("Indices:", paste(linear_combos$remove, collapse = ", "), "\n") 
    }
    
    # Make sure we don't switch to a vector if only 1 column remains.
    # Subtract 1L because y is the first column in the caret analysis.
    covars_df = covars_df[, !colnames(covars_df) %in% colnames(covars_df)[linear_combos$remove - 1L], 
                          drop = FALSE]
    if (verbose) {
      cat("Updated covariate count:", ncol(covars_df), "\n")
    }
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
  
  original <-covars_df
  
  # Create square terms
  Wsq_all <- covars_df^2
  colnames(Wsq_all) <- paste0(colnames(Wsq_all), "sq")
  
  # Add squared terms to X.
  original <- cbind(original, Wsq_all)
  n.columns <- ncol(original)

  # Optional prescreening
  # Separately for treatment and outcome
  if (prescreen) {
    if (verbose) {
      cat("clean_data_tmle(): running prescreening code.\n")
    }
    
    # Identify non-binary variables (by index not name).
    # The ones that pass screening will have squared terms possibly added.
    nonbinary_vars <- names(covars_df)[apply(covars_df, 2, function(col_data) {
    #nonbinary_vars <- which(apply(covars_df, 2, function(col_data) {
      # Need to remove NAs from list, otherwise binary vars will be seen as
      # having 3 values if they have missing data.
      length(setdiff(unique(col_data), NA)) > 2L
    #}))
    })]
    
    if (verbose) {
      cat("Found", (ncol(covars_df) - length(nonbinary_vars)), "binary and",
          length(nonbinary_vars), "nonbinary covariates.\n")
    }
    
    # Identify covariate indices that meet a univariate correlation threshold
    # with the outcome variable, after adjusting for treatment status.
    #keep <- which(prescreen_uni(outcome_vec, treatment_vec, covars_df))
    keep <- colnames(covars_df)[prescreen_uni(outcome_vec, treatment_vec, covars_df)]
    if (verbose) {
      cat("Outcome correlation screening: selected", length(keep), "covars and",
          "removed", (ncol(covars_df) - length(keep)), "covars.\n")
    }
    # Identify covariates that meet a univariate correlation threshold
    # with the treatment indicator.
    #keepA <- which(prescreen_uniA(treatment_vec, covars_df))
    keepA <- names(covars_df)[prescreen_uniA(treatment_vec, covars_df)]
    if (verbose) {
      cat("Treatment correlation screening: selected", length(keepA), "covars and",
          "removed", (ncol(covars_df) - length(keepA)), "covars.\n")
    }
    
    # TODO: what's going on here?
    # CK: turning this off for now and reimplementing.
    if (F) {
    keep.nonbinary <- data.frame(t(subset(t(nonbinary_vars), select = keep)))
    names(keep.nonbinary) <- "val"
    keep.nonbinaryA <- data.frame(t(subset(t(nonbinary_vars), select = keepA)))
    names(keep.nonbinaryA) <- "val"
    
    
    # TODO: what's going on here?
    keep.nonbin_sub <- subset(keep.nonbinary, val == "TRUE")
    keep.nonbinary <- names(data.frame(covars_df)) %in% row.names(keep.nonbin_sub)
    keep.nonbin_subA <- subset(keep.nonbinaryA, val == "TRUE")
    keep.nonbinaryA <- names(data.frame(covars_df)) %in% row.names(keep.nonbin_subA)
    } else {
      keep.nonbinary = nonbinary_vars[nonbinary_vars %in% keep]
      keep.nonbinaryA = nonbinary_vars[nonbinary_vars %in% keepA]
    }
    
    # Outcome
    WsqY = NULL
    
    #if (length(which(keep.nonbinary)) > 0) {
    # CK: turning this off for now, needs to be reimplemented with comments.
    if (FALSE && length(keep.nonbinary) > 0) {
      
      # TODO: what's going on here?
      new<-cbind.data.frame(names(data.frame(covars_df[, keep.nonbinary])),
                            prescreen_uni(outcome_vec, treatment_vec, covars_df[, keep.nonbinary]^2, 
                                          alpha=prescreen[1], min = 0))
      names(new)<-c("name","val")
      
      keep.nonbin_sub<-subset(new, val=="TRUE")
      keep.sq<-names(data.frame(covars_df)) %in% keep.nonbin_sub$name
      
      if (sum(keep.sq) > 0) {
        WsqY <- covars_df[, keep.sq, drop = FALSE]^2
        colnames(WsqY) <- paste0(colnames(WsqY), "sq")
      }
    }
    
    #Treatment
    WsqA = NULL
    
    #if (length(which(keep.nonbinaryA)) > 0) {
    # CK: turning this off for now, needs to be reimplemented with comments.
    if (FALSE && length(which(keep.nonbinaryA)) > 0) {
      
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
    
    # Add squared terms to covars_df for Y
    # TODO: we should return column names, not copies of the full dataframe.
    if (!is.null(WsqY)) {
      covars_dfY <- cbind(covars_df[, keep], WsqY)
    } else {
      covars_dfY = covars_df[, keep]
    }
    n.columns <- ncol(covars_dfY)
    
    # Add squared terms to covars_df for A
    # TODO: we should return column names, not copies of the full dataframe.
    if (!is.null(WsqA)) {
      covars_dfA <- cbind(covars_df[, keepA], WsqA)
    } else {
      covars_dfA = covars_df[, keepA]
    }
    n.columnsA <- ncol(covars_dfA)
    
    } else {
      if (verbose) cat("Keep all covariates. \n")
      
      # Add squared terms to X.
      # TODO: we should remove collinear terms after this step, because esp. for
      # binary variables this will create duplicate columns.
      # Or just not add squared terms. Should be an argument to the function.
      covars_df<-cbind(covars_df, Wsq_all)
      n.columns <- ncol(covars_df)
      
      covars_dfA<-cbind(covars_df, Wsq_all)
      n.columnsA <- ncol(covars_dfA)
    } 
  
  results = list(
    data = original,
    # TODO: we should return column names, not copies of the full dataframe.
    covariate_dfY = covars_df,
    covariate_dfA = covars_dfA
    # TODO: add censoring covariates
  )
  return(results)
} 

