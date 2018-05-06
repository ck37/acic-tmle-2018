#' This will process the covariate file.
#' TODO: upgrade to more thoroughly handle the different covariates.
process_covariates = function(data, id_field = "sample_id", verbose = FALSE) {
  # Extract the sample ids.
  # TODO: check if id_field exists in this data.
  id = data[[id_field]] 
  
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