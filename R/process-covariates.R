#' This will process the covariate file.
#' TODO: upgrade to more thoroughly handle the different covariates.
process_covariates = function(data, id_field = "sample_id", verbose = FALSE) {
  # Extract the sample ids.
  id = data[[id_file]] 
  
  # Remove ID field from our dataset.
  covariates = data[, setdiff(colnames(data), id_field)]
  
  # Compile results.
  results = list(
    covariates = covariates,
    id = id
  )
  
  return(results)
}