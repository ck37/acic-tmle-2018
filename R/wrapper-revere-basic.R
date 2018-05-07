# This will setup the SuperLearner stuff, call the basic revere, then send results back to estimate_ate.R
wrapper_revere_basic =
  function(data,
           outcome_field,
           treatment_field,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
    
  # TODO: define all the required elements to pass into reverse_cvtmle_basic
    
  # Call revere basic, defined in R/revere_cvtmle_basic.R
  tmle_result =
     revere_cvtmle_basic(data = data,
                         outcome_field = outcome_field,
                         treatment_field = treatment_field,
                         covariates_Q = covariates_Q, 
                         covariates_g = covariates_g,
                         lrnr_stack_Q = lrnr_stack_Q,
                         metalearner_Q = metalearner_Q, 
                         metalearner_eval_Q = metalearner_eval_Q,
                         verbose = verbose)
  
  # TODO: Create potential outcomes dataframe out of the preds_all dataframe.
  potential_outcomes_df = data.frame()
  
  # Compile results.  
  results =
    list(ate_est = tmle_result$ate_est,
         ci_left = tmle_result$conf_int[1],
         ci_right = tmle_result$conf_int[2],
         # Dataframe of individual potential outcomes.
         ipo_df = potential_outcomes_df)
   
  # These results will be processed within R/estimate-ate.R
  return(results)
}