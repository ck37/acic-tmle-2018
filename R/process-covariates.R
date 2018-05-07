#' This will process the covariate file.
#' TODO: upgrade to more thoroughly handle the different covariates.
process_covariates = function(data, id_field = "sample_id", verbose = FALSE) {
  
  # Check sample IDs exist in the data and extract the sample ids.
  if (!id_field %in% names(data)) {
    stop(paste("ID variable not found:", names(data)))
  }  
  
  id = data[[id_field]] 
  
  # Remove constant columns from the covariate file.
  constant_columns = which(apply(data, MARGIN = 2L, var) == 0)
  
  if(length(constant_columns)>0){
    data = data[, -constant_columns, drop = FALSE]
  }
  
  if (verbose) {
    cat("Removed", length(constant_columns), "constant columns from the covariate file.\n")
  }
  
  rm(constant_columns)
  
  # Remove ID field from our dataset.
  # Actually we need to keep it in for later merging.
  # covariates = data[, setdiff(colnames(data), id_field), drop = FALSE]
  
  # Compile results.
  results = list(
    data = data,
    covariates = setdiff(colnames(data), id_field),
    id = id
  )
  
  return(results)
}