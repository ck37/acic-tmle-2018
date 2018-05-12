# This will setup the estimator, call it, then send results back to estimate_ate.R
wrapper_tmle_glm = 
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           covariate_fields = NULL,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("Running wrapper_tmle_glm().\n")
  }
  
  ##############
  # Define all the required elements to pass into tmle()
  
  if (is.null(covariate_fields)) {
    covariate_fields = setdiff(colnames(data),
                               c(outcome_field, treatment_field, id_field))
  }
    
  #####
  # Setup SL libraries.
    
  # Placeholder library
  sl_lib = c("SL.mean", "SL.glm")
  
  q_lib = g_lib = sl_lib
  
  #####
  # Define probability family for outcome.
  
  if (all(unique(data[[outcome_field]]) %in% c(0, 1))) {
    family = "binomial"
  } else {
    family = "gaussian"
  }
  
  #####
  # Run estimator
  tmle_result = tmle(Y = data[[outcome_field]],
                     A = data[[treatment_field]],
                     W = data[, covariate_fields, drop = FALSE],
                     Delta = as.integer(!is.na(data[[outcome_field]])),
                     Q.SL.library = q_lib,
                     g.SL.library = g_lib,
                     family = family,
                     verbose = verbose)
  
  # TODO: Create potential outcomes dataframe out of the preds_all dataframe.
  # Which columns should we use for y0 and y1?
  potential_outcomes_df = data.frame()
  
  # Compile results.  
  results =
    list(ate_est = tmle_result$estimates$ATE$psi,
         ci_left = tmle_result$estimates$ATE$CI[1],
         ci_right = tmle_result$estimates$ATE$CI[2],
         # Dataframe of individual potential outcomes.
         potential_outcomes = potential_outcomes_df)
  
  # These results will be processed within R/estimate-ate.R
  return(results)
}