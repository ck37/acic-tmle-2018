library(data.table)
library(sl3)
# devtools::install_github("jlstiles/sl3")
# devtools::install_github("jeremyrcoyle/gentmle2")
library(origami)
library(R6)
library(SuperLearner)
library(gentmle2)
library(tmle)

# Below we will perform a revere CV-TMLE
# generate the data
gendata = function (n, g0, c0, Q0) {
  W1 = runif(n, -3, 3)
  W2 = rnorm(n)
  W3 = runif(n)
  W4 = rnorm(n)
  A = rbinom(n, 1, g0(W1, W2, W3, W4))
  C = rbinom(n, 1, c0(A, W1, W2, W3, W4))
  Y = rbinom(n, 1, Q0(A, W1, W2, W3, W4))
  data.frame(A, C, W1, W2, W3, W4, Y)
}

g0_linear = function (W1, W2, W3, W4) {
  plogis(0.5 * (-0.8 * W1 + 0.39 * W2 + 0.08 * W3 - 0.12 * 
                  W4 - 0.15))
}

c0= function (A, W1, W2, W3, W4) {
  plogis(0.5 * (-0.4 * W1 + 0.3 * W2 + 0.06 * W3 - 0.30 * 
                  W4 - 1-.2*A))
}

Q0_1 = function (A, W1, W2, W3, W4) {
  plogis(0.14 * (2 * A + 3 * A * W1 + 6 * A * W3 * W4 + W2 * 
                   W1 + W3 * W4 + 10 * A * cos(W4)))
}


# make learners, mean does not work
lrnr_step = make_learner(Lrnr_step)
lrnr_glm = make_learner(Lrnr_glm)

metalearnerLogLik <- make_learner(Lrnr_optim)
metalearnerQ = metalearnerLogLik$initialize(learner_function = metalearner_logistic_binomial,
                             loss_function = loss_loglik_binomial)
# metalearnerQ =  make_learner(Lrnr_nnls)
lrnr_stack = make_learner(Stack, lrnr_step, lrnr_glm)

cv_lrnr = Lrnr_cv$new(lrnr_stack)

# one weird trick to train on subset with cv, but predict on whole set
subset_fold_training <- function(fold, subset_index){
  new_training <- intersect(training(),subset_index)
  new_validation <- intersect(validation(),subset_index)
  list(fold=make_fold(fold_index(),new_training,new_validation))
}

# metalearner <- make_learner(Lrnr_nnls)
###
###
###
# This is not a function yet but will shortly be so
# revereTMLE = function(df, cv_lrnrQ, learner_stackG, learner_stackC, metalearnerQ,
#                       metalearnerG, covariatesQ, covariatesG) {

# example of revere cv-tmle

# generate data and declare covs
  n=1000
  data = gendata(n, g0_linear, c0, Q0_1)
  df = data
  covariates_Q = covariates_c = colnames(df)[c(1,3:6)]
  covariates_g = colnames(df)[3:6]
  
  
  # create the censoring strata to balance--this is optional but might be helpful in 
  # the make folds below
  strata_ids = rep("C1", n)
  strata_ids[df$C==0] = "C0"
  
  # make the balanced folds--these will be used later!
  folds <- make_folds(n, strata_ids = strata_ids)
  
  # subset index the folds because we can only fit on uncens
  subset_index <- which(df$C == 0)
  # mean(df$C)
  
  # one weird trick to train on subset with cv, but predict on whole set. 
  # These are same as before but cut out the C=1 observations
  subsetted_folds <- cross_validate(subset_fold_training, folds, subset_index, 
                                    use_future=FALSE)$fold
  
  # make the task to train Qbar on uncensored data
  QAW_task_sub = make_sl3_Task(data = df, covariates = covariates_Q, outcome = "Y",
                               folds = subsetted_folds)
  
  # We will predict on the whole validation set eventually
  QAW_task = make_sl3_Task(data = df, covariates = covariates_Q, outcome = "Y",
                                folds = folds)
  
  # make tasks for the A=1 and A=0 subsets
  dataQ1W = dataQ0W = df
  dataQ1W$A = 1
  dataQ0W$A = 0
  
  Q1W_task = make_sl3_Task(data = dataQ1W, covariates = covariates_Q, outcome = "Y",
                       folds = folds)
  Q0W_task = make_sl3_Task(data = dataQ0W, covariates = covariates_Q, outcome = "Y",
                       folds = folds)
  # train Qbar on the folds using uncensored 
  cv_lrnr_fit = cv_lrnr$train(QAW_task_sub)
  # predict on uncensored to fit the metalearner
  QAW_stack_sub = cv_lrnr_fit$predict(QAW_task_sub)
  # predict on censored as well for use as placeholders--might be superfluous
  QAW_stack = cv_lrnr_fit$predict(QAW_task)

  # bookeeping indices
  inds = unlist(lapply(folds, FUN = function(x) x$validation_set))
  subsetted_inds = unlist(lapply(subsetted_folds, FUN = function(x) x$validation_set))
  
  Y = df$Y[inds]
  C = df$C[inds]
  A = df$A[inds]
  Y_sub = df$Y[subsetted_inds]
  C_sub = df$C[subsetted_inds]
  A_sub = df$A[subsetted_inds]

  # fit the metalearner and get coefs to be used later
  Z_Q = make_sl3_Task(data = cbind(Y = Y_sub, Qk_stack_sub), 
                         covariates = names(QAW_stack), outcome = "Y")
  Qfit = metalearnerQ$train(Z_cols)
  coefQ = Qfit$coefficients
  coefQ
  # predict on whole val fold using coefs
  Qk = metalearner_logistic_binomial(coefQ, as.matrix(cv_lrnr_fit$predict(dataQ)))
  Q1k = metalearner_logistic_binomial(coefQ, as.matrix(cv_lrnr_fit$predict(newQ1)))
  Q0k = metalearner_logistic_binomial(coefQ, as.matrix(cv_lrnr_fit$predict(newQ0)))
  # setting Y equal to Qk for censored--probably unnecessary and dangerous but not fucking
  # anything right now
  Y[C==1] = NA

  # fit and predict on folds for g and c
  data_g = make_sl3_Task(data = df, covariates = covariates_g, outcome = "A",
                        folds = folds)

  cv_lrnr_fitg = cv_lrnr$train(data_g)
  gk_stack = cv_lrnr_fitg$predict()
  Zg_cols = make_sl3_Task(data = cbind(A = A, gk_stack), 
                         covariates = names(gk_stack), outcome = "A")
  gfit = metalearnerQ$train(Zg_cols)
  coefg = gfit$coefficients
  
  # coefs to be used later
  coefg
  gk = metalearner_logistic_binomial(coefg, as.matrix(gk_stack))
  
  # fit on folds and predict on subsetted folds for g and c
  data_c = make_sl3_Task(data = df, covariates = covariates_Q, outcome = "C",
                         folds = folds)
  
  cv_lrnr_fitc = cv_lrnr$train(data_c)
  
  # these are prob of cens scores for A=0  and A=1
  ck_stack1 = cv_lrnr_fitc$predict(newQ1)
  ck_stack0 = cv_lrnr_fitc$predict(newQ0)
  ck_stack = cv_lrnr_fitc$predict()
  Zc_cols = make_sl3_Task(data = cbind(C = C, ck_stack), 
                          covariates = names(ck_stack), outcome = "C")
  cfit = metalearnerQ$train(Zc_cols)
  coefc= cfit$coefficients
  # sl coefs to be used later
  coefc
  # 1 minus to get probability of being observed
  ck1 = 1 - metalearner_logistic_binomial(coefc, as.matrix(ck_stack1))
  ck0 = 1 - metalearner_logistic_binomial(coefc, as.matrix(ck_stack0))
  
  # feeding into susan's package to target'
  pDelta1 = matrix(c(ck0, ck1), ncol = 2)
  Q = matrix(c(Q0k, Q1k), ncol = 2)
  head(pDelta1)
  head(Q)
  W = df[,3:6]
  tmle1 = tmle(Y=Y,A=A,W=W, Delta = 1-C,Q = Q, pDelta1 = pDelta1, g1W = gk, family = 'binomial',
               fluctuation = "logistic")
  tmle1
  # grab the definitive epsilon
  eps = tmle1$eps
  eps
  
  # now that we have epsilon and the SL coefs, we take advantage of the full fit and retrace
  # our steps
  lrnr_stack = make_learner(Stack, lrnr_step, lrnr_glm)
  dataQ = make_sl3_Task(data = df, covariates = covariates_Q, outcome = "Y")
  newQ1 = make_sl3_Task(data = dataQ1, covariates = covariates_Q, outcome = "Y")
  newQ0 = make_sl3_Task(data = dataQ0, covariates = covariates_Q, outcome = "Y")
  
  Qfit = lrnr_stack$train(dataQ)
  coefQ
  
  QAW = metalearner_logistic_binomial(coefQ, as.matrix(Qfit$predict()))
  Q1W = metalearner_logistic_binomial(coefQ, as.matrix(Qfit$predict(newQ1)))
  Q0W = metalearner_logistic_binomial(coefQ, as.matrix(Qfit$predict(newQ0)))
  
  data_g = make_sl3_Task(data = df, covariates = covariates_g, outcome = "A")
  gfit = lrnr_stack$train(data_g)
  g1W = metalearner_logistic_binomial(coefg, as.matrix(gfit$predict()))
  
  newQ1 = make_sl3_Task(data = dataQ1, covariates = covariates_Q, outcome = "C",
                        folds = folds)
  newQ0 = make_sl3_Task(data = dataQ0, covariates = covariates_Q, outcome = "C",
                        folds = folds)
  data_c = make_sl3_Task(data = df, covariates = covariates_Q, outcome = "C")
  cfit = lrnr_stack$train(data_c)
  c1_stack = as.matrix(cfit$predict(newQ1))
  c0_stack = as.matrix(cfit$predict(newQ0))
  cA1 = 1-metalearner_logistic_binomial(coefc, c1_stack)
  cA0 = 1-metalearner_logistic_binomial(coefc, c0_stack)
  
  A = df$A
  C = df$C
  Y = df$Y
  
  H1W = (1/g1W)/cA1
  H0W =  (1/(1 - g1W))/cA0
  HAW = A*H1W - (1-A)*H0W

  # funal update
  Q1star = plogis(qlogis(Q1W) + eps[2]*H1W)
  Q0star = plogis(qlogis(Q0W) + eps[1]*H0W)
  Qstar = A*Q1star + (1-A)*Q0star
  psi = mean(Q1star - Q0star)
  
  # is not that the correct IC
  Dstar = (1-C)*(HAW)*(Y - Qstar) + Q1star - Q0star - psi
  se = sd(Dstar)/sqrt(n)
  se^2
  
  # comparing with Gruber's initial estimate--should be close'
  c(psi = psi, se = se , left = psi - 1.96*se, right = psi + 1.96*se)
  tmle1$estimates$ATE
  
  # The truth 
  truth = mean(with(df, Q0_1(1,W1,W2,W3,W4) - Q0_1(0,W1,W2,W3,W4)))
  truth
  
  # check the mean of Dstar--should be approx 0 but really is not!
  mean(Dstar)

# }



