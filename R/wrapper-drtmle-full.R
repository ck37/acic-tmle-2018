# This will setup the estimator, call it, then send results back to estimate_ate.R
wrapper_drtmle_full = 
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = NULL,
           covariate_fields = NULL,
           # TODO: re-enable 4 CV-folds before running final analysis.
           #cv_folds = 4,
           # Setting to 1 for testing purposes - should switch back to ~4 when finalizing.
           cv_folds = 1,
           verbose = FALSE) {
  # This function name would be passed into run_analyis() and would be
  # executed within R/estimate-ate.R
  if (verbose) {
    cat("\nwrapper_drtmle_glm() - begin. cv-tmle folds:", cv_folds, "\n")
  }
  
  ##############
  # Define all the required elements to pass into tmle()
  
  # TODO: work on this section so that fewer variable names are created.
  # It's somewhat duplicative/roundabout at the moment.
  if (is.null(covariate_fields)) {
    covariate_fieldsY = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
    covariate_fieldsA = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
    covariate_fieldsC = setdiff(colnames(data),
                                c(outcome_field, treatment_field, id_field))
  } else {
    covariate_fieldsY = covariate_fields$outcome
    covariate_fieldsA = covariate_fields$treatment
    covariate_fieldsC = covariate_fields$censoring
  }
  
  # Include treatment indicator in outcome regression.
  covariates_Q = c(treatment_field, covariate_fieldsY)
  covariates_g = covariate_fieldsA
  covariates_c = covariate_fieldsC
  
  # TODO: need to create screeners for each of these covariate sets
  # that are applied to the SuperLearner libraries for the respective
  # fits(Q, g, c).
    
  #####
  # Copy code from 2017 entry.
    
  conf = list(parallel = FALSE, max_cores = 20L)
    
  # Setup parallelization? Use up to 4 cores.
  num_cores = RhpcBLASctl::get_num_cores()
  
  if (verbose) {
    cat("Cores detected:", num_cores, "\n")
  }
  
  use_cores = min(num_cores, conf$max_cores)
  
  if (verbose) {
    # Check how many parallel workers we are using:
    cat("Cores used:", use_cores, "\n")
  }
  
  # Speed up bartMachine by enabling multicore execution.
  bartMachine::set_bart_machine_num_cores(use_cores)
  
  #####
  # Define probability family for outcome.
  
  if (all(unique(data[[outcome_field]]) %in% c(0, 1))) {
    family = "binomial"
  } else {
    family = "gaussian"
  }
  
  # Lame that we have to use GlobalEnv to pass through this function, maybe
  # there is a cleaner way to do this.
  .GlobalEnv$q_screener = function(...) screen.select_vars(..., vars = covariates_Q)
  .GlobalEnv$g_screener = function(...) screen.select_vars(..., vars = covariates_g)
  .GlobalEnv$c_screener = function(...) screen.select_vars(..., vars = covariates_c)
  
  q_lib = c(list("SL.mean"),
            # Add q_screener to all remaining learners.
    lapply(c("SL.glm",
             #"SL.glmnet_fast",
             "SL.ranger_fast2",
             sl_xgb$names,
             "SL.dbarts_fast",
             # Much slower, tho may be due in part to settings differences:
             #"SL.bartMachine2",
             "SL.nnet"),
           function(learner) c(learner, "q_screener")))
  
  #g_lib = list("SL.mean", c("SL.glm", "g_screener"))
  g_lib = c(list("SL.mean"),
            # Add q_screener to all remaining learners.
    lapply(c("SL.glm",
             #"SL.glmnet_fast",
             "SL.ranger_fast2",
             sl_xgb$names,
             "SL.dbarts_fast",
             # Much slower, tho may be due in part to settings differences:
             # "SL.bartMachine2",
             "SL.nnet"),
           function(learner) c(learner, "g_screener")))
  
  #c_lib = list("SL.mean", c("SL.glm", "c_screener"))
  c_lib = c(list("SL.mean"),
            # Add q_screener to all remaining learners.
    lapply(c("SL.glm",
             #"SL.glmnet_fast",
             "SL.ranger_fast2",
             sl_xgb$names,
             # SL.bartMachine2 or dbarts?
             "SL.dbarts_fast",
             # Much slower, tho may be due in part to settings differences:
             # "SL.bartMachine2",
             "SL.nnet"),
           function(learner) c(learner, "c_screener")))
  
  # Reduced form estimation.
  qr_lib = c("SL.mean", "SL.glm", "SL.npreg")
  gr_lib = c("SL.mean", "SL.glm", "SL.npreg")
  
  #####
  # Run estimator
  est_result =
    drtmle(Y = data[[outcome_field]],
           A = data[[treatment_field]],
           W = data[, unique(c(covariates_Q, covariates_g, covariates_c)),
                    drop = FALSE],
           DeltaY = as.integer(!is.na(data[[outcome_field]])),
           SL_Q = q_lib,
           # Treatment and censoring SL libs are both defined through SL_g argument.
           SL_g = list("A" = g_lib, "DeltaY" = c_lib),
           SL_Qr = qr_lib,
           SL_gr = gr_lib,
           cvFolds = cv_folds,
           # Qn - pass in Q0W and Q1W estimates directly.
           # gn - pass in g0W and g1W estimates directly.
           # Package will handle family automatically.
           verbose = verbose)
  
  inference = drtmle::ci(est_result, contrast = c(-1, 1))
  
  # TODO: Create potential outcomes dataframe out of the preds_all dataframe.
  # Which columns should we use for y0 and y1?
  potential_outcomes_df = data.frame()
  
  # Compile results.  
  results =
    list(ate_est = inference$drtmle[1, "est"],
         ci_left = inference$drtmle[1, "cil"],
         ci_right = inference$drtmle[1, "ciu"],
         # Dataframe of individual potential outcomes.
         potential_outcomes = potential_outcomes_df)
  
  # These results will be processed within R/estimate-ate.R
  return(results)
}