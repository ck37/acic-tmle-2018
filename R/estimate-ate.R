estimate_ate =
  function(data,
           # tmle_wrapper would be a function that accepts a dataframe plus
           # the outcome field, treatment field, and verbose flag.
           tmle_wrapper = NULL,
           outcome_field = "Y",
           treatment_field = "A",
           id_field = "id",
           prescreen = TRUE,
           squared = TRUE,
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
  
  # Preprocess covariate data before running TMLE. 
  # 1) Remove linearly correlated columns
  # 2) Keeps covariates with univariate associations
  # 3) Adds square terms (temporarily disabled; TODO: fix this)
  # 4) Remove constant columns from the covariate file.
  covar_result = clean_data_tmle(covars_df = covariate_df,
                                 outcome_vec = outcome_vec,
                                 treatment_vec = treatment_vec,
                                 prescreen = prescreen,
                                 squared = squared,
                                 verbose = verbose)
  data_new = covar_result$data
  covariate_df = covar_result$covariate_dfY
  covariate_dfA = covar_result$covariate_dfA
  covariate_dfC = covar_result$covariate_dfC
  
  # Combine elements back into one unified dataframe.
  # TODO: confirm this works correctly.
  data_new = cbind.data.frame(data[[outcome_field]], data[[treatment_field]],
                          data_new)
  
  # Make sure our names stay correct.
  # TODO: confirm this works correctly.
  colnames(data_new)[1:2] = c(outcome_field, treatment_field)
  
  # TODO: add censoring covariates.
  covariate_fields = list("outcome" = covariate_df,
                          "treatment" = covariate_dfA,
                          "censoring" = covariate_dfC)
  
  if (!is.null(tmle_wrapper)) {
    # We could have multiple versions of the tmle_wrapper function to try different approaches.
    tmle_result = do.call(tmle_wrapper,
                          # Arguments to pass into the tmle_wrapper.
                          list(data = data_new,
                               outcome_field = outcome_field,
                               treatment_field = treatment_field,
                               id_field = id_field,
                               covariate_fields = covariate_fields,
                               verbose = verbose))
    results =
      list(ate_est = tmle_result$ate_est,
           ci_left = tmle_result$ci_left,
           ci_right = tmle_result$ci_right,
           # Dataframe of individual potential outcomes.
           ipo_df = tmle_result$ipo_df)
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