# This will setup the estimator, call it, then send results back to estimate_ate.R
wrapper_tmle_better = 
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           covariate_fields = NULL,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("Running wrapper_tmle_better().\n")
  }
  
  ##############
  # Define all the required elements to pass into tmle()
  
  if (is.null(covariate_fields)) {
    covariate_fields = setdiff(colnames(data),
                               c(outcome_field, treatment_field, id_field))
  }
    
  #####
  # Copy code from 2017 entry.
    
  conf = list(verbose = TRUE, parallel = FALSE, max_cores = 20L)
    
  # Setup parallelization? Use up to 4 cores.
  num_cores = RhpcBLASctl::get_num_cores()
  
  if (conf$verbose) {
    cat("Cores detected:", num_cores, "\n")
  }
  
  use_cores = min(num_cores, conf$max_cores)
  
  if (conf$verbose) {
    # Check how many parallel workers we are using:
    cat("Cores used:", use_cores, "\n")
  }
  
  # Speed up bartMachine by enabling multicore execution.
  bartMachine::set_bart_machine_num_cores(use_cores)
    
  #####
  # Setup SL libraries.
  
  q_lib = c(list(# speedglm doesn't work :/ just use plain ol' glm.
    c("SL.glm", "All", "screen.corRank8", "prescreen_nosqr")#,
    #c("SL.mgcv", "All", "prescreen.nosq"),
    #c("sg.gbm.2500", "prescreen.nocat"),
    #"SL.xgboost",
    #"SL.xgboost_threads_4"
    # Effect modification learners can't be used with g, only Q.
  ),
  # create.Learner() grids.
  #sl_glmnet_em15$names,
  #sl_xgb$names,
  # Temporarily turn off SVM due to errors Vince is getting.
  #sl_ksvm$names, 
  list(
    #"SL.randomForest_fast",
    "SL.xgboost_fast",
    "SL.ranger_fast",
    c("SL.glmnet_fast", "All", "screen.corRank8"),
    c("SL.nnet", "All", "screen.corRank8"),
    #c("SL.earth", "prescreen.nosq"),
    # Works only if parallel = F. Do not use with mcSuperlearner!
    "SL.bartMachine2",
    "SL.mean"))
  
  # Need a separate g lib that does not include effect modification learners.
  g_lib = c(list(c("SL.glm", "All", "screen.corRank8", "prescreen.nosq"),
                 #c("SL.mgcv", "All", "prescreen.nosq"),
                 #c("sg.gbm.2500", "prescreen.nocat"),
                 #"SL.xgboost",
                 #"SL.xgboost_threads_4",
                 "SL.ranger_fast"#,
  ), # create.Learner() grids.
  sl_xgb$names,
  # Temporarily turn off SVM due to errors Vince is getting.
  #sl_ksvm$names, 
  list(
    c("SL.glmnet_fast", "All","screen.corRank8"),
    c("SL.nnet", "All","screen.corRank8"),
    #c("SL.earth", "prescreen.nosqr"),
    # Works only if parallel = F. Do not use with mcSuperlearner!
    "SL.bartMachine2",
    "SL.mean"))
    
  
  #q_lib = g_lib = sl_lib
  g_lib = q_lib
  
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