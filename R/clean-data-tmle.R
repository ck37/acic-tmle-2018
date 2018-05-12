# This function is called by estimate_ate() in R/estimate-ate.R

clean_data_tmle =
  function(covars_df,
           outcome_vec,
           treatment_vec,
           prescreen,
           squared,
           verbose = verbose) {
    if (verbose) {
      cat("\nclean_date_tmle() - begin processing.\n")
    }
    
    # Remove constant columns from the covariate file.
    # NOTE: this won't work for factors, would need to use length(unique())
    # and possibly also remove NAs.
    # From earlier emails by Susan Gruber it sounded like all data will always be numeric, 
    # even if the covariate should actually be a factor (ex, race)
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
  
  #Original data set with squared terms added
  #Contains all covariates other than dupicates and constant 
  original <-covars_df
  
  # Create square terms
  Wsq_all <- covars_df^2
  colnames(Wsq_all) <- paste0(colnames(Wsq_all), "sq")
  
  # Add squared terms to X.
  original <- cbind(original, Wsq_all)
  n.columns <- ncol(original)
  
  #Create censoring variable based on the outcome
  C <- rep(0, length(outcome_vec))
  C[is.na(outcome_vec)]<-1

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
    keepY <- colnames(covars_df)[prescreen_uni(outcome_vec, treatment_vec, covars_df)]
    if (verbose) {
      cat("Outcome correlation screening: selected", length(keepY), "covars and",
          "removed", (ncol(covars_df) - length(keepY)), "covars.\n")
    }
    
    # Identify covariates that meet a univariate correlation threshold
    # with the treatment indicator.
    keepA <- names(covars_df)[prescreen_uniA(treatment_vec, covars_df)]
    if (verbose) {
      cat("Treatment correlation screening: selected", length(keepA), "covars and",
          "removed", (ncol(covars_df) - length(keepA)), "covars.\n")
    }
    
    # Identify covariates that meet a univariate correlation threshold
    # with the censoring indicator.
    keepC <- names(covars_df)[prescreen_uniA(C, covars_df)]
    if (verbose) {
      cat("Censoring correlation screening: selected", length(keepC), "covars and",
          "removed", (ncol(covars_df) - length(keepC)), "covars.\n")
    }
    
    #Optional: add squared terms to Y, A, C
    if(squared){
      
      #Among the variables we keep for Y, A, C separate non-binary covariates 
      keep.nonbinaryY <- keepY[keepY %in% nonbinary_vars]
      keep.nonbinaryA <- keepA[keepA %in% nonbinary_vars]
      keep.nonbinaryC <- keepC[keepC %in% nonbinary_vars]
      
      #Create square terms for non-binary covariates for Y, A, C
      WsqY = NULL
      WsqA = NULL
      WsqC = NULL
      
      #Square non-binary covariates for Y,A,C
      if(length(keep.nonbinaryY) > 0){
          WsqY <- covars_df[, keep.nonbinaryY, drop = FALSE]^2
          colnames(WsqY) <- paste0(colnames(WsqY), "sq")
      } 
      
      if(length(keep.nonbinaryA) > 0){
        WsqA <- covars_df[, keep.nonbinaryA, drop = FALSE]^2
        colnames(WsqA) <- paste0(colnames(WsqA), "sq")
      } 
      
      if(length(keep.nonbinaryC) > 0){
        WsqC <- covars_df[, keep.nonbinaryC, drop = FALSE]^2
        colnames(WsqC) <- paste0(colnames(WsqC), "sq")
      } 
      
      # Add squared terms to covars_df for Y
      if (!is.null(WsqY)) {
        covars_dfY <- cbind(covars_df[, keepY], WsqY)
      } else {
        covars_dfY = covars_df[, keepY]
      }
      n.columnsY <- ncol(covars_dfY)
      
      # Add squared terms to covars_df for A
      if (!is.null(WsqA)) {
        covars_dfA <- cbind(covars_df[, keepA], WsqA)
      } else {
        covars_dfA = covars_df[, keepA]
      }
      n.columnsA <- ncol(covars_dfA)
      
      # Add squared terms to covars_df for C
      # TODO: we should return column names, not copies of the full dataframe.
      if (!is.null(WsqC)) {
        covars_dfC <- cbind(covars_df[, keepC], WsqC)
      } else {
        covars_dfC = covars_df[, keepC]
      }
      n.columnsC <- ncol(covars_dfC)

    }
    
    } else {
      if (verbose) cat("Keep all covariates. \n")
      
      # Add squared terms to X.
      if(squared){
        
        if (verbose) cat("Adding squared terms. \n")
        
        covars_dfY<-cbind(covars_df, Wsq_all)
        n.columns <- ncol(covars_dfY)
        
        covars_dfA<-cbind(covars_df, Wsq_all)
        n.columnsA <- ncol(covars_dfA)
        
        covars_dfC<-cbind(covars_df, Wsq_all)
        n.columnsC <- ncol(covars_dfC)
      }else{
        covars_dfY<-covars_df
        covars_dfA<-covars_df
        covars_dfC<-covars_df
      }
      
    } 
  
  results = list(
    data = original,
    # TODO: no need to create so many copies of the data, bad solution for now
    covariate_dfY = names(covars_dfY),
    covariate_dfA = names(covars_dfA),
    covariate_dfC = names(covars_dfC)
  )
  return(results)
} 

