# This is the revere function which will output the first pass revere and the full
# revere, which does not solve the IC equation but comes from a model which does 
# and is slightly more efficient

revere_cvtmle_basic = function(data,covariates_Q, covariates_c = NULL, covariates_g, lrnr_stack_Q,
                         lrnr_stack_c = NULL, lrnr_stack_g = NULL, metalearner_Q, 
                         metalearner_c = NULL, metalearner_g = NULL,
                         metalearner_eval_Q, metalearner_eval_c = NULL, 
                         metalearner_eval_g = NULL)
{  
  if (is.null(metalearner_c)) metalearner_c = metalearner_Q
  if (is.null(metalearner_g)) metalearner_g = metalearner_Q
  if (is.null(metalearner_eval_c)) metalearner_eval_c = metalearner_eval_Q
  if (is.null(metalearner_eval_g)) metalearner_eval_g = metalearner_eval_Q
  
  if (is.null(lrnr_stack_c)) lrnr_stack_c = lrnr_stack_Q 
  if(is.null(lrnr_stack_g)) lrnr_stack_g = lrnr_stack_Q 
  if (is.null(covariates_c)) covariates_c = covariates_Q
  
  cv_lrnr_Q = Lrnr_cv$new(lrnr_stack_Q)
  cv_lrnr_c = Lrnr_cv$new(lrnr_stack_c)
  cv_lrnr_g = Lrnr_cv$new(lrnr_stack_g)
  
  n = nrow(data) 
  
  # create the censoring strata to balance--this is optional but might be helpful in 
  # the make folds below
  strata_ids = rep("C1", n)
  strata_ids[data$C==0] = "C0"
  
  # make the balanced folds as per censoring
  folds <- make_folds(n, strata_ids = strata_ids)
  
  # subset index the folds because we can only fit on uncensored
  subset_index <- which(data$C == 0)
  subsetted_folds <- cross_validate(subset_fold_training, folds, subset_index, 
                                    use_future=FALSE)$fold
  
  # make the task to train Qbar on uncensored data
  QAW_task_sub = make_sl3_Task(data = data, covariates = covariates_Q, outcome = "Y",
                               folds = subsetted_folds)
  
  # We predict on the whole stacked validation sets (this task) because we can
  QAW_task = make_sl3_Task(data = data, covariates = covariates_Q, outcome = "Y",
                           folds = folds)
  
  # make tasks for the A=1 and A=0 subsets for prediction
  dataQ1W = dataQ0W = data
  dataQ1W$A = 1
  dataQ0W$A = 0
  
  Q1W_task = make_sl3_Task(data = dataQ1W, covariates = covariates_Q, outcome = "Y",
                           folds = folds)
  Q0W_task = make_sl3_Task(data = dataQ0W, covariates = covariates_Q, outcome = "Y",
                           folds = folds)
  
  # train Qbar on the folds using uncensored 
  cv_lrnr_fit = cv_lrnr_Q$train(QAW_task_sub)
  
  # predict on uncensored validation sets to feed to the metalearner, no overfitting here!
  QAW_stack_sub = cv_lrnr_fit$predict(QAW_task_sub)
  
  # bookeeping indices
  inds = unlist(lapply(folds, FUN = function(x) x$validation_set))
  subsetted_inds = unlist(lapply(subsetted_folds, FUN = function(x) x$validation_set))
  
  Y = data$Y
  C = data$C
  A = data$A
  Y_sub = Y[subset_index]
  
  # fit the metalearner and get coefs to be used later
  Z_Q = make_sl3_Task(data = cbind(Y = Y_sub, QAW_stack_sub), 
                      covariates = names(QAW_stack_sub), outcome = "Y")
  Qfit = metalearner_Q$train(Z_Q)
  coefQ = Qfit$coefficients
  
  # predict on whole of stacked val sets using coefs
  QAW = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(QAW_task)))
  Q1W = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(Q1W_task)))
  Q0W = metalearner_eval_Q(coefQ, as.matrix(cv_lrnr_fit$predict(Q0W_task)))
  
  # setting censored outcomes to NA because that's how they should be coded in reality'
  Y[C==1] = NA
  
  # fit and predict on folds for g and c
  
  #first fit the pscores
  g1W_task = make_sl3_Task(data = data, covariates = covariates_g, outcome = "A",
                           folds = folds)
  
  # fit on training folds
  cv_lrnr_fitg = cv_lrnr_g$train(g1W_task)
  
  # predict on stacked val sets--no overfitting here!
  g1W_stack = cv_lrnr_fitg$predict()
  
  # fit the metalearner
  Z_g = make_sl3_Task(data = cbind(A = A, g1W_stack), 
                      covariates = names(g1W_stack), outcome = "A")
  gfit = metalearner_g$train(Z_g)
  coefg = gfit$coefficients
  
  # stacked val set preds on very close to new data
  g1W = metalearner_eval_g(coefg, as.matrix(g1W_stack))
  
  # fit on folds and predict on subsetted folds for g and c
  c1W_task = make_sl3_Task(data = data, covariates = covariates_Q, outcome = "C",
                           folds = folds)
  
  # need to fit for A=1 and for A=0 so we make these tasks
  c1W_taskA1 = make_sl3_Task(data = dataQ1W, covariates = covariates_Q, outcome = "C",
                             folds = folds)
  c1W_taskA0 = make_sl3_Task(data = dataQ0W, covariates = covariates_Q, outcome = "C",
                             folds = folds)
  
  # fit on the training sets
  cv_lrnr_fitc = cv_lrnr_c$train(c1W_task)
  
  # predict on stacked val sets--no overfitting here!
  c1W_stack = cv_lrnr_fitc$predict()
  # these are prob of cens scores for A=0  and A=1
  c1W_stackA1 = cv_lrnr_fitc$predict(c1W_taskA1)
  c1W_stackA0 = cv_lrnr_fitc$predict(c1W_taskA0)
  
  # fit the metalearner
  Z_c = make_sl3_Task(data = cbind(C = C, c1W_stack), 
                      covariates = names(c1W_stack), outcome = "C")
  cfit = metalearner_c$train(Z_c)
  coefc= cfit$coefficients
  
  # 1 minus to get probability of being observed for both A=1 and A=0 obs
  c1W_A1 = 1 - metalearner_eval_c(coefc, as.matrix(c1W_stackA1))
  c1W_A0 = 1 - metalearner_eval_c(coefc, as.matrix(c1W_stackA0))
  
  # feeding into susan's package to target only--very fast
  pDelta1 = matrix(c(c1W_A0, c1W_A1), ncol = 2)
  Q = matrix(c(Q0W, Q1W), ncol = 2)
  W = data[,3:6]
  tmle_info = tmle(Y=Y,A=A,W=W, Delta = 1-C,Q = Q, pDelta1 = pDelta1, g1W = g1W, family = 'binomial',
               fluctuation = "logistic")
  
  # grab the definitive epsilon
  eps = tmle1$eps
  
  preds_star = tmle_info[,1]*(1-A) + tmle_info[,2]*A
  CATE_star = tmle_info[,2] - tmle_info[,1]
  preds_init = Q[,1]*(1-A) + Q[,2]*A
  CATE_init = Q[,2] - Q[,1]
  
  CI = c(tmle_info$estimates$ATE$psi,tmle_info$estimates$ATE$CI)
  
  preds_all = data.frame(preds_star = preds_star, CATE_star = CATE_star, preds_init = preds_init,
                   CATE_init = CATE_init)
  res = list(preds_all = preds_all, CI = CI)
  
  return(res)
}


# a function that saves our lives here--jeremy genius
subset_fold_training <- function(fold, subset_index){
  new_training <- intersect(training(),subset_index)
  new_validation <- intersect(validation(),subset_index)
  list(fold=make_fold(fold_index(),new_training,new_validation))
}