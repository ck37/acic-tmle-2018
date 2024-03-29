# This is the revere function which will output the first pass revere
# not fitting again on full data, so IC equation here is certainly solved

# Relies on Lrnr_cv2 in R/lrnr_cv2.R

revere_cvtmle_basic1 =
  function(data,
           outcome_field = "y",
           treatment_field = "z",
           id_field = "sample_id",
           censor_field = "C",
           covariates_Q,
           covariates_c = NULL,
           covariates_g,
           lrnr_stack_Q,
           lrnr_stack_c = NULL,
           lrnr_stack_g = NULL,
           metalearner_Q, 
           metalearner_c = NULL,
           metalearner_g = NULL,
           metalearner_eval_Q,
           metalearner_eval_c = NULL, 
           metalearner_eval_g = NULL,
           verbose = FALSE) {  
    
  if (is.null(metalearner_c)) metalearner_c = metalearner_Q
  if (is.null(metalearner_g)) metalearner_g = metalearner_Q
  if (is.null(metalearner_eval_c)) metalearner_eval_c = metalearner_eval_Q
  if (is.null(metalearner_eval_g)) metalearner_eval_g = metalearner_eval_Q
  
  if (is.null(lrnr_stack_c)) lrnr_stack_c = lrnr_stack_Q 
  if (is.null(lrnr_stack_g)) lrnr_stack_g = lrnr_stack_Q 
  if (is.null(covariates_c)) covariates_c = covariates_g
  
  cv_lrnr_Q = Lrnr_cv2$new(lrnr_stack_Q)
  cv_lrnr_c = Lrnr_cv2$new(lrnr_stack_c)
  cv_lrnr_g = Lrnr_cv2$new(lrnr_stack_g)
  
  n = nrow(data) 
  data[[censor_field]] = as.integer(is.na(data[[outcome_field]]))
  
  # this is to not mess up sl3 with its outcome type
  data[[outcome_field]][is.na(data[[outcome_field]])] = 1
  
  # create the censoring strata to balance--this is optional but might be helpful in 
  # the make folds below
  strata_ids = rep("C1", n)
  strata_ids[data[[censor_field]] == 0] = "C0"
  
  # make the balanced folds as per censoring
  folds <- origami::make_folds(n, strata_ids = strata_ids)
  
  # subset index the folds because we can only fit on uncensored
  subset_index <- which(data[[censor_field]] == 0)
  subsetted_folds <- origami::cross_validate(subset_fold_training, folds, subset_index, 
                                    use_future = FALSE)$fold
  
  # make the task to train Qbar on uncensored data
  QAW_task_sub = make_sl3_Task(data = data,
                               covariates = covariates_Q,
                               outcome = outcome_field,
                               folds = subsetted_folds)
  
  # We predict on the whole stacked validation sets (this task) because we can
  QAW_task = make_sl3_Task(data = data.table::copy(data),
                           covariates = covariates_Q,
                           outcome = outcome_field,
                           folds = folds)
  
  # make tasks for the A=1 and A=0 subsets for prediction
  dataQ1W = dataQ0W = data
  dataQ1W[[treatment_field]] = 1
  dataQ0W[[treatment_field]] = 0
  
  Q1W_task = make_sl3_Task(data = dataQ1W,
                           covariates = covariates_Q,
                           outcome = outcome_field,
                           folds = folds)
  Q0W_task = make_sl3_Task(data = dataQ0W,
                           covariates = covariates_Q,
                           outcome = outcome_field,
                           folds = folds)
  
  # train Qbar on the folds using uncensored 
  if (verbose) {
    cat("Fitting outcome regression...")
  }
  time_start = proc.time()
  cv_lrnr_fit = cv_lrnr_Q$train(QAW_task_sub)
  time_end = proc.time()
  time_outcome_reg = (time_end - time_start)["elapsed"]
  if (verbose) {
    cat(" done. Time elapsed:", round(time_outcome_reg / 60, 1), "minutes.\n")
    # TODO: display more diagnostics like best learner & estimated risk.
  }
  
  # predict on uncensored validation sets to feed to the metalearner, no overfitting here!
  QAW_stack_sub = cv_lrnr_fit$predict(QAW_task_sub)
  
  # bookeeping indices
  inds = unlist(lapply(folds, FUN = function(x) x$validation_set))
  subsetted_inds = unlist(lapply(subsetted_folds, FUN = function(x) x$validation_set))
  
  y = data[[outcome_field]]
  C = data[[censor_field]]
  z = data[[treatment_field]]
  y_sub = y[subset_index]
  
  # fit the metalearner and get coefs to be used later
  Z_Q = make_sl3_Task(data = cbind(y = y_sub, QAW_stack_sub), 
                      covariates = names(QAW_stack_sub),
                      outcome = outcome_field)
  Qfit = metalearner_Q$train(Z_Q)
  coefQ = Qfit$coefficients
  
  # predict on whole of stacked val sets using coefs
  QAW = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(QAW_task)))
  Q1W = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(Q1W_task)))
  Q0W = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(Q0W_task)))
  
  # setting censored outcomes to NA because that's how they should be coded in reality'
  y[C == 1] = NA
  
  # fit and predict on folds for g and c
  
  #first fit the pscores
  g1W_task = make_sl3_Task(data = data.table::copy(data),
                           covariates = covariates_g,
                           outcome = treatment_field,
                           folds = folds)
  
  # fit on training folds
  if (verbose) {
    cat("Fitting treatment regression...")
  }
  time_start = proc.time()
  cv_lrnr_fitg = cv_lrnr_g$train(g1W_task)
  time_end = proc.time()
  time_treatment_reg = (time_end - time_start)["elapsed"]
  if (verbose) {
    cat(" done. Time elapsed:", round(time_treatment_reg / 60, 1), "minutes.\n")
    # TODO: display more diagnostics like best learner & estimated risk.
  }
  
  # predict on stacked val sets--no overfitting here!
  g1W_stack = cv_lrnr_fitg$predict()
  
  # fit the metalearner
  Z_g = make_sl3_Task(data = cbind(z = z, g1W_stack), 
                      covariates = names(g1W_stack),
                      outcome = treatment_field)
  gfit = metalearner_g$train(Z_g)
  coefg = gfit$coefficients
  
  # stacked val set preds on very close to new data
  g1W = metalearner_eval_g(coefg, as.matrix(g1W_stack))
  
  if (any(C==1)) {
    # fit on folds and predict on subsetted folds for g and c
    c1W_task = make_sl3_Task(data = data.table::copy(data), covariates = covariates_c,
                             outcome = censor_field,
                             folds = folds)
    
    # need to fit for A=1 and for A=0 so we make these tasks
    c1W_taskA1 = make_sl3_Task(data = data.table::copy(dataQ1W), covariates = covariates_c,
                               outcome = censor_field,
                               folds = folds)
    c1W_taskA0 = make_sl3_Task(data = data.table::copy(dataQ0W), covariates = covariates_c,
                               outcome = censor_field,
                               folds = folds)
    
    # fit on the training sets
    if (verbose) {
      cat("Fitting censoring regression...")
    }
    time_start = proc.time()
    cv_lrnr_fitc = cv_lrnr_c$train(c1W_task)
    time_end = proc.time()
    time_censor_reg = (time_end - time_start)["elapsed"]
    if (verbose) {
      cat(" done. Time elapsed:", round(time_censor_reg / 60, 1), "minutes.\n")
      # TODO: display more diagnostics like best learner & estimated risk.
    }
    
    # predict on stacked val sets--no overfitting here!
    c1W_stack = cv_lrnr_fitc$predict()
    # these are prob of cens scores for A=0  and A=1
    c1W_stackA1 = cv_lrnr_fitc$predict(c1W_taskA1)
    c1W_stackA0 = cv_lrnr_fitc$predict(c1W_taskA0)
    
    # fit the metalearner
    Z_c = make_sl3_Task(data = cbind(C = C, c1W_stack), 
                        covariates = names(c1W_stack),
                        outcome = censor_field)
    cfit = metalearner_c$train(Z_c)
    coefc = cfit$coefficients
    
    # 1 minus to get probability of being observed for both A=1 and A=0 obs
    c1W_A1 = 1 - metalearner_eval_c(coefc, as.matrix(c1W_stackA1))
    c1W_A0 = 1 - metalearner_eval_c(coefc, as.matrix(c1W_stackA0))
    
    pDelta1 = matrix(c(c1W_A0, c1W_A1), ncol = 2)
    
    Q = matrix(c(Q0W, Q1W), ncol = 2)
    
    # jury-rigging because data.table is obtuse
    data = as.data.frame(data)
    W = data[, covariates_Q, drop = FALSE]
    
    # Confirm length of z is equal to nrow of Q.
    # stopifnot(length(z) == nrow(Q))
    
    # feeding into susan's package (tmle) to target only--very fast
    # Suppress the warnings about recycling array of length 1 due to sqrt(ATE).
    suppressWarnings({
        tmle_info =
          tmle::tmle(Y = y,
                     A = z,
                     W = W,
                     Delta = 1 - C,
                     Q = Q,
                     pDelta1 = pDelta1,
                     g1W = g1W,
                     family = "gaussian",
                     fluctuation = "logistic")
    })
  } else {
    
    Q = matrix(c(Q0W, Q1W), ncol = 2)
    
    # jury-rigging because data.table is obtuse
    data = as.data.frame(data)
    W = data[, covariates_Q, drop = FALSE]
    
    # Confirm length of z is equal to nrow of Q.
    # stopifnot(length(z) == nrow(Q))
    
    # feeding into susan's package (tmle) to target only--very fast
    
    # Suppress the warnings about recycling array of length 1 due to sqrt(ATE).
    suppressWarnings({
      tmle_info =
        tmle::tmle(Y = y,
                   A = z,
                   W = W,
                   Q = Q,
                   g1W = g1W,
                   family = "gaussian",
                   fluctuation = "logistic") 
    })
  }
  # grab the definitive epsilon

  y0 = tmle_info$Qinit$Q[,1] 
  y1 = tmle_info$Qinit$Q[,2]
  
  CI = c(tmle_info$estimates$ATE$psi, tmle_info$estimates$ATE$CI)
  
  # Compile individual predictions.
  potential_oc = data.frame(y0 = y0, y1 = y1)
  
  # Compile results.
  results =
    list(potential_oc = potential_oc,
         ate_est = tmle_info$estimates$ATE$psi,
         conf_int = tmle_info$estimates$ATE$CI,
         CI = CI)
  
  return(results)
}
