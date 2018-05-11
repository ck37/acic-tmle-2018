# This will setup the SuperLearner stuff, call the basic revere, then send results back to estimate_ate.R
wrapper_revere_basic =
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           covariate_fields = NULL,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("Running wrapper_revere_basic().\n")
  }
    
  ##############
  # Define all the required elements to pass into revere_cvtmle_basic
  # This is based on revere_cvtmle_example.R
    
  lrnr_mean = make_learner(Lrnr_mean)
  lrnr_glm = make_learner(Lrnr_glm)
  # Only exists in Jonathan's fork of sl3 - can't use this yet.
  # lrnr_bayesglm = make_learner(Lrnr_bayesglm)
  lrnr_xgboost = make_learner(Lrnr_xgboost)$initialize(nrounds = 1000, eta = .01, nthread = 4)
  #lrnr_bartMachine = make_learner(Lrnr_bartMachine)
  lrnr_dbarts = make_learner(Lrnr_dbarts)
  lrnr_grf = make_learner(Lrnr_grf)
  
  lrnr_stack_Q = make_learner(Stack, lrnr_glm, lrnr_mean,
                              #lrnr_dbarts,
                              #lrnr_grf,
                              lrnr_xgboost)
  
  # metalearner_eval_Q = metalearner_logistic_binomial
  # metalearnerLogLik <- make_learner(Lrnr_optim)
  # metalearner_Q = metalearnerLogLik$initialize(learner_function = metalearner_logistic_binomial,
  #                                             loss_function = loss_loglik_binomial)
  
  # for nnls metalearner
  metalearner_eval_Q = metalearner_linear
  metalearner_Q = make_learner(Lrnr_nnls)
  
  if (is.null(covariate_fields)) {
    covariate_fieldsY = setdiff(colnames(data),
                               c(outcome_field, treatment_field, id_field))
    covariate_fieldsA = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
  }else{
    covariate_fieldsY = covariate_fields$Y
    covariate_fieldsA = covariate_fields$A
  }
  
  # Include treatment indicator in outcome regression.
  covariates_Q = c(treatment_field, covariate_fieldsY)
  covariates_g = covariate_fieldsA
    
  # Call revere basic, defined in R/revere_cvtmle_basic.R
  tmle_result =
     revere_cvtmle_basic(data = data,
                         outcome_field = outcome_field,
                         treatment_field = treatment_field,
                         covariates_Q = covariates_Q, 
                         covariates_g = covariates_g,
                         # The Q stack will be re-used for g by default.
                         # Otherwise define lrnr_stack_g
                         lrnr_stack_Q = lrnr_stack_Q,
                         metalearner_Q = metalearner_Q, 
                         metalearner_eval_Q = metalearner_eval_Q,
                         verbose = verbose)
  
  # Compile results.  
  results =
    list(ate_est = tmle_result$ate_est,
         ci_left = tmle_result$CI[2],
         ci_right = tmle_result$CI[3],
         # Dataframe of individual potential outcomes.
         ipo_df = tmle_result$potential_oc)
   
  # These results will be processed within R/estimate-ate.R
  return(results)
}