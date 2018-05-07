estimate_ate =
  function(data,
           outcome_field = "Y",
           treatment_field = "A",
           id_field = "id",
           verbose = FALSE) {
    
  # Extract outcome variable
  if (!outcome_field %in% names(data)) {
    stop(paste("Outcome not found:", names(data)))
  }  
 
  outcome_vec = data[[outcome_field]]
  
  # Extract treatment variable.
  if (!treatment_field %in% names(data)) {
    stop(paste("Treatment not found:", names(data)))
  }
  
  treatment_vec = data[[treatment_field]]
    
  # Extract covariates - all remaining variables.
  # Make sure sample_id is not in dataframe even though it should already be removed.
  covariate_df = data[, setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field)), drop = FALSE]
  
  # Preprocess covariate data before running TMLE. Remove constant columns, etc.
  # This function is defined in R/clean-data-tmle.R
  covar_result = clean_data_tmle(covars_df = covariate_df,
                                 outcome_vec = outcome_vec,
                                 treatment_vec = treatment_vec,
                                 verbose = verbose)
  covariate_df = covar_result$covariate_df
  
  # TODO: run TMLE, using outcome_vec (Y), treatment_vec (A), and covariate_df (W).
  # input the rest of the stuff from the function revere_cvtmle_basic.  data can be the data
  # as merged with z as treatment, y as oc..  
  # <INSERT CALL TO REVERE TMLE HERE>
    
  # Compile results.
  results =
    list(ate_est = 0,
         ci_left = 0,
         ci_right = 0,
         # Dataframe of individual potential outcomes.
         ipo_df = data.frame())
  
  return(results)
}