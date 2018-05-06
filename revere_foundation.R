library(data.table)
library(sl3)
library(origami)
library(R6)
library(SuperLearner)

# Below we will perform a revere CV-TMLE
# generate the data
gendata = function (n, g0, Q0) {
  W1 = runif(n, -3, 3)
  W2 = rnorm(n)
  W3 = runif(n)
  W4 = rnorm(n)
  A = rbinom(n, 1, g0(W1, W2, W3, W4))
  Y = rbinom(n, 1, Q0(A, W1, W2, W3, W4))
  data.frame(A, W1, W2, W3, W4, Y)
}

g0_linear = function (W1, W2, W3, W4) {
  plogis(0.5 * (-0.8 * W1 + 0.39 * W2 + 0.08 * W3 - 0.12 * 
                  W4 - 0.15))
}

Q0_1 = function (A, W1, W2, W3, W4) {
  plogis(0.14 * (2 * A + 3 * A * W1 + 6 * A * W3 * W4 + W2 * 
                   W1 + W3 * W4 + 10 * A * cos(W4)))
}

n=100
data = gendata(n, g0_linear, Q0_1)
head(data)

# make the task
test = make_sl3_Task(data = data, covariates = colnames(data)[1:5], outcome = "Y")

# make learners, mean does not work
lrnr_glmnet = make_learner(Lrnr_glmnet)
lrnr_glm = make_learner(Lrnr_glm_fast)

metalearner <- make_learner(Lrnr_nnls)
lrnr_stack = make_learner(Stack, lrnr_glmnet, lrnr_glm)

cv_lrnr = Lrnr_cv$new(lrnr_stack)
cv_lrnr_fit = cv_lrnr$train(test)

# from there you can predict on cv-data setting A to 1 and A to 0
newdata = test$data 
newdata1 = newdata0 = newdata
newdata1$A = 1
newdata0$A = 0

new = make_sl3_Task(data = newdata1, covariates = colnames(data)[1:5], outcome = "Y")

cv_lrnr_fit$predict(new)[1:10]
cv_lrnr_fit$predict()[1:10]

folds <- make_folds(100)
subset_index <- 31:100

# one weird trick to train on subset with cv, but predict on whole set
fold = folds[[1]]
subset_fold_training <- function(fold, subset_index){
  new_training <- intersect(training(),subset_index)
  new_validation <- validation()
  list(fold=make_fold(fold_index(),new_training,new_validation))
}

subsetted_folds <- cross_validate(subset_fold_training, folds, subset_index, 
                                  use_future=FALSE)$fold
subsetted_folds
# define task
# covariates=c("pnz")
# covariates=c("rainfall","rainfall7","rainfall30","rainfall60")
covariates=c("pnz","mean","max","sd","median","rainfall","rainfall7","rainfall30",
             "rainfall60","length")
status_task <- make_sl3_Task(summary[!is.na(label)], outcome="label", covariates = covariates, 
                             folds = subsetted_folds)
to_pred <- make_sl3_Task(summary[!is.na(label)], outcome="label", covariates = covariates, 
                         folds = folds)  

# load in the task, the stacks
metalearner <- make_learner(Lrnr_nnls)
revereTMLE = function(df, lrnr_stackQ, learner_stackG, learner_stackC, metalearnerQ,
                      metalearnerG, covariatesQ, covariatesG) {
  
  # create the task of uncensored data
  task_Qtrain = make_sl3_Task(data = df[df$C==0,], 
                              covariates = covariatesQ, outcome = "Y")
  
  cv_lrnr = Lrnr_cv$new(lrnr_stack)
  cv_lrnr_fit = cv_lrnr$train(task_Qtrain)  
  
  # use the cross val preds and the metalearner to find the coeffs, which we will use later
  ml_fit <- metalearner$train(cv_lrnr_fit$chain())
  coeffs = ml_fit$coefficients
  
  # run the tmle for ATE using gentmle2
  newdata = test$data 
  newdata1 = newdata0 = newdata
  newdata1$A = 1
  newdata0$A = 0
  Y = newdata$Y
  A = newdata$A
  Qk = cv_lrnr_fit$predict()
  Q1k = cv_lrnr_fit$predict()
  Q0k = cv_lrnr_fit$predict()
  
  initdata = data.frame(A = A, Y = Y, Qk = Qk, Q1k = Q1k, Q0k = Q0k, gk = gk)
  tmle_info = gentmle2::gentmle(initdata, params = param_ATE, submodel = submodel_logit, loss = loss_loglik, 
            depsilon = 1e-04, approach = "line", max_iter = 100, g.trunc = 1e-04, 
            Q.trunc = 1e-04, simultaneous.inference = FALSE) 
  
  lrnr_stack = make_learner(Stack, lrnr_glmnet, lrnr_glm)
  
  
}
