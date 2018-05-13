# This will setup the SuperLearner stuff, call the basic revere, then send results back to estimate_ate.R
wrapper_revere_glmnet =
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           # This is a list with elements "outcome", "treatment", "censoring".
           covariate_fields = NULL,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("\nwrapper_revere_glmnet() - begin.\n")
  }
    
  ##############
  # Define all the required elements to pass into revere_cvtmle_basic
  # This is based on revere_cvtmle_example.R
    
  lrnr_mean = make_learner(Lrnr_mean)
  lrnr_glm = make_learner(Lrnr_glm)
  # TODO: use multiple cores.
  lrnr_glmnet = make_learner(Lrnr_glmnet)
  
  lrnr_stack_Q =
    make_learner(Stack,
                 lrnr_mean,
                 lrnr_glm, 
                 # TODO: fix glmnet, is yielding warnings and not working.
                 lrnr_glmnet)
  
  # metalearner_eval_Q = metalearner_logistic_binomial
  # metalearnerLogLik <- make_learner(Lrnr_optim)
  # metalearner_Q = metalearnerLogLik$initialize(learner_function = metalearner_logistic_binomial,
  #                                             loss_function = loss_loglik_binomial)
  
  # for nnls metalearner
  metalearner_eval_Q = metalearner_linear
  metalearner_Q = make_learner(Lrnr_nnls)
  
  # TODO: work on this section so that fewer variable names are created.
  # It's somewhat duplicative/roundabout at the moment.
  if (is.null(covariate_fields)) {
    # Baseline set of covariates
    covars_outcome = covars_treatment = covars_censor = 
      setdiff(colnames(data), c(outcome_field, treatment_field, id_field))
  } else {
    covars_outcome = covariate_fields$outcome
    covars_treatment = covariate_fields$treatment
    covars_censor = covariate_fields$censoring
  }
  
  # Call revere basic, defined in R/revere_cvtmle_basic.R
  tmle_result =
     revere_cvtmle_basic(data = data,
                         outcome_field = outcome_field,
                         treatment_field = treatment_field,
                         covariates_Q = c(treatment_field, covars_outcome), 
                         covariates_g = covars_treatment,
                         covariates_c = covars_censor,
                         # The Q stack will be re-used for g by default.
                         # Otherwise define lrnr_stack_g
                         lrnr_stack_Q = lrnr_stack_Q,
                         metalearner_Q = metalearner_Q, 
                         metalearner_eval_Q = metalearner_eval_Q,
                         verbose = verbose)
  
  # Compile results.  
  results =
    list(ate_est = tmle_result$ate_est,
         ci_left = tmle_result$conf_int[1],
         ci_right = tmle_result$conf_int[2],
         # Dataframe of individual potential outcomes.
         ipo_df = tmle_result$potential_oc)
   
  # These results will be processed within R/estimate-ate.R
  return(results)
}