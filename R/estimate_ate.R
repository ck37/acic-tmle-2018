estimate_ate =
  function(data,
           outcome_field = "Y",
           treatment_field = "A",
           id_field = "id",
           verbose = FALSE) {
    
  # Extract outcome variable
  # TODO: check if outcome_field actually exists.
  outcome_vec = data[[outcome_field]]
  
  # Extract treatment variable.
  # TODO: check if treatment_field actually exists.
  treatment_vec = data[[treatment_field]]
    
  # Extract covariates - all remaining variables.
  # Make sure sample_id is not in dataframe even though it should already be removed.
  covariate_df = data[, setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field)), drop = FALSE]
  
  # TODO: run TMLE, using outcome_vec (Y), treatment_vec (A), and covariate_df (W).
    
  # Compile results.
  results =
    list(ate_est = 0,
         ci_left = 0,
         ci_right = 0,
         # Dataframe of individual potential outcomes.
         ipo_df = data.frame())
  
  return(results)
}