library(data.table)
library(sl3)
# devtools::install_github("jlstiles/sl3")
# devtools::install_github("jeremyrcoyle/gentmle2")
library(origami)
library(R6)
library(SuperLearner)
library(gentmle2)
library(tmle)
library(here)

# Requires reverse_cvtmle.R
source("R/revere_cvtmle_basic.R")
debug(revere_cvtmle_basic)
# Below we will perform a revere CV-TMLE
# generate the data
gendata = function (n, g0, c0, Q0) {
  W1 = runif(n, -3, 3)
  W2 = rnorm(n)
  W3 = runif(n)
  W4 = rnorm(n)
  z = rbinom(n, 1, g0(W1, W2, W3, W4))
  C = rbinom(n, 1, c0(z, W1, W2, W3, W4))
  y = rbinom(n, 1, Q0(z, W1, W2, W3, W4))
  data.frame(z, C, W1, W2, W3, W4, y)
}

g0_linear = function (W1, W2, W3, W4) {
  plogis(0.5 * (-0.8 * W1 + 0.39 * W2 + 0.08 * W3 - 0.12 * 
                  W4 - 0.15))
}

c0= function (z, W1, W2, W3, W4) {
  plogis(0.5 * (-0.4 * W1 + 0.3 * W2^2 + 0.06 * abs(W3) - 0.30 * 
                  .3*z*W4 -.5*W4 + 1-.2*z))
}

Q0_1 = function (z, W1, W2, W3, W4) {
  plogis(0.14 * (2 * z + 3 * z * W1 + 6 * z * W3 * W4 + W2 * 
                   W1 + W3 * W4 + 10 * z * cos(W4)))
}

# metalearnerQ =  make_learner(Lrnr_nnls)
if (F) {
  # Run manually.
  
  n=1000
  data = gendata(n, g0_linear, c0, Q0_1)
  
  lrnr_mean = make_learner(Lrnr_mean)
  lrnr_glm = make_learner(Lrnr_glm)
  lrnr_bayesglm = make_learner(Lrnr_bayesglm)
  lrnr_xgboost = make_learner(Lrnr_xgboost)$initialize(nrounds = 1000, eta = .01, nthread = 4)
  lrnr_stack_Q = make_learner(Stack, lrnr_glm, lrnr_mean)
  
  # for log-lik metalearner
  # metalearner_eval_Q = metalearner_logistic_binomial
  # metalearnerLogLik <- make_learner(Lrnr_optim)
  # metalearner_Q = metalearnerLogLik$initialize(learner_function = metalearner_logistic_binomial,
  #                                             loss_function = loss_loglik_binomial)

  # for nnls metalearner
  metalearner_eval_Q = metalearner_linear
  metalearner_Q = make_learner(Lrnr_nnls)
  
  set.seed(10)
  n=1000
  data = gendata(n, g0_linear, c0, Q0_1)
  
  data$y[data$C==1] = NA
  data$C = NULL
  
  covariates_Q = colnames(data)[c(1:5)]
  covariates_g = colnames(data)[2:5]
  data[1:10,]
  res_revere = revere_cvtmle_basic(data = data,covariates_Q = covariates_Q, 
                             covariates_g = covariates_g, lrnr_stack_Q = lrnr_stack_Q,
                             metalearner_Q = metalearner_Q, 
                             metalearner_eval_Q = metalearner_eval_Q)
  
  res_revere$cover
  res_revere$CI
  
  res_revere$SLcoefs
}

