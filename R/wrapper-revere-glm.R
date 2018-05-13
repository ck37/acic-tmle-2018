# sl3 + revere cvtmle: only mean & glm learners
wrapper_revere_glm =
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           # This is a list with elements "outcome" and "treatment".
           # TODO: support "censoring".
           covariate_fields = NULL,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("\nwrapper_revere_glm() - begin.\n")
  }
    
  ##############
  # Define all the required elements to pass into revere_cvtmle_basic
  # This is based on revere_cvtmle_example.R
    
  num_cores = RhpcBLASctl::get_num_cores()
    
  lrnr_mean = make_learner(Lrnr_mean)
  lrnr_glm = make_learner(Lrnr_glm)
  
  lrnr_stack_Q =
    make_learner(Stack,
                 lrnr_mean,
                 lrnr_glm)
  
  # metalearner_eval_Q = metalearner_logistic_binomial
  # metalearnerLogLik <- make_learner(Lrnr_optim)
  # metalearner_Q = metalearnerLogLik$initialize(learner_function = metalearner_logistic_binomial,
  #                                             loss_function = loss_loglik_binomial)
  
  # for nnls metalearner
  metalearner_eval_Q = metalearner_linear
  #metalearner_Q = make_learner(Lrnr_nnls)
  metalearner_Q = make_learner(Lrnr_nnls_convex)
  
  # TODO: work on this section so that fewer variable names are created.
  # It's somewhat duplicative/roundabout at the moment.
  if (is.null(covariate_fields)) {
    covariate_fieldsY = setdiff(colnames(data),
                               c(outcome_field, treatment_field, id_field))
    covariate_fieldsA = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
    covariate_fieldsC = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
  }else{
    covariate_fieldsY = covariate_fields$outcome
    covariate_fieldsA = covariate_fields$treatment
    covariate_fieldsC = covariate_fields$censoring
  }
  
  # Include treatment indicator in outcome regression.
  covariates_Q = c(treatment_field, covariate_fieldsY)
  covariates_g = covariate_fieldsA
  covariates_c = covariate_fieldsC
    
  # Call revere basic, defined in R/revere_cvtmle_basic.R
  tmle_result =
     revere_cvtmle_basic(data = data,
                         outcome_field = outcome_field,
                         treatment_field = treatment_field,
                         covariates_Q = covariates_Q, 
                         covariates_g = covariates_g,
                         covariates_c = covariates_c,
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