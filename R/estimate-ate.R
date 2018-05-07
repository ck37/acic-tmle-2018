estimate_ate =
  function(data,
           # tmle_wrapper would be a function that accepts a dataframe plus
           # the outcome field, treatment field, and verbose flag.
           tmle_wrapper = NULL,
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
  
  # Combine elements back into one unified dataframe.
  # TODO: confirm this works correctly.
  data = cbind.data.frame(data[[outcome_field]], data[[treatment_field]],
                          covariate_df)
  
  # Make sure our names stay correct.
  # TODO: confirm this works correctly.
  colnames(data)[1:2] = c(outcome_field, treatment_field)
  
  if (!is.null(tmle_wrapper)) {
    # We could have multiple versions of the tmle_wrapper function to try different approaches.
    tmle_result = do.call(tmle_wrapper,
                          # Arguments to pass into the tmle_wrapper.
                          list(data = data,
                               outcome_field = outcome_field,
                               treatment_field = treatment_field,
                               id_field = id_field,
                               verbose = verbose))
    results =
      list(ate_est = tmle_result$ate_est,
           ci_left = tmle_result$conf_int[1],
           ci_right = tmle_result$conf_int[2],
           # Dataframe of individual potential outcomes.
           ipo_df = tmle_result$potential_outcomes)
  }
  
  if (is.null(tmle_wrapper) || !exists("results")) {
    # Compile blank results.
    results =
      list(ate_est = 0,
         ci_left = 0,
         ci_right = 0,
         # Dataframe of individual potential outcomes.
         ipo_df = data.frame())
  }
  
  return(results)
}