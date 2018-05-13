SL.xgb = function(Y, X, newX, family, obsWeights, id, ntrees = 1000,
                      max_depth=4, shrinkage=0.1, minobspernode=10, params = list(),
                      nthread = 1, verbose = 0,
                      ...) {
  SuperLearner:::.SL.require("xgboost")
  if(packageVersion("xgboost") < 0.6) stop("SL.xgboost requires xgboost version >= 0.6, try help(\'SL.xgboost\') for details")
  # X needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(X)) {
    X = model.matrix(~ . - 1, X)
  }
  
  # Convert to an xgboost compatible data matrix, using the sample weights.
  xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
  
  # TODO: support early stopping, which requires a "watchlist". See ?xgb.train
  
  if (family$family == "gaussian") {
    model = xgboost::xgboost(data = xgmat, objective="reg:linear", nrounds = ntrees,
                             max_depth = max_depth, minchildweight = minobspernode, eta = shrinkage,
                             verbose = verbose, nthread = nthread, params = params,
                             save_period = NULL)
  }
  if (family$family == "binomial") {
    model = xgboost::xgboost(data = xgmat, objective="binary:logistic", nrounds = ntrees,
                             max_depth = max_depth, minchildweight = minobspernode, eta = shrinkage,
                             verbose = verbose, nthread = nthread, params = params,
                             save_period = NULL)
  }
  if (family$family == "multinomial") {
    # TODO: test this.
    model = xgboost::xgboost(data = xgmat, objective="multi:softmax", nrounds = ntrees,
                             max_depth = max_depth, minchildweight = minobspernode, eta = shrinkage,
                             verbose = verbose, num_class = length(unique(Y)), nthread = nthread,
                             params = params, save_period = NULL)
  }
  
  # Newdata needs to be converted to a matrix first, then an xgb.DMatrix.
  if (!is.matrix(newX)) {
    newX = model.matrix(~ . - 1, newX)
  }
  
  pred = predict(model, newdata = newX)
  
  fit = list(object = model)
  class(fit) = c("SL.xgboost")
  out = list(pred = pred, fit = fit)
  return(out)
}

# Multiple versions of XGBoost if we can afford the extra computation.
# Keep the grid pretty small: 6 learners.
sl_xgb = create.Learner("SL.xgb", detailed_names = T,
                        params = list(nthread = RhpcBLASctl::get_num_cores(),
                                      ntrees = 1000L),
                        tune = list(max_depth = c(2, 4, 8),
                                    shrinkage = c(0.001, 0.01)))
length(sl_xgb$names)

# Small SVM grid for cost parameter and kernel.
sl_ksvm = create.Learner("SL.ksvm", detailed_names = T,
                         tune = list(
                            # Regularization parameter, could be 2^-5 to 2^15.
                            C = 2^c(-3, 0, 4),
                            kernel = c("rbfdot", "laplacedot"))) 
length(sl_ksvm$names)
                             

# Multithreaded version of XGBoost when using sequential SuperLearner.
SL.xgboost_fast = function(...) SL.xgboost(..., nthread = RhpcBLASctl::get_num_cores())

# Faster glmnet.
# TODO: use multicore via foreach for glmnet.
SL.glmnet_fast = function(...) SL.glmnet(..., nlambda = 20, nfolds = 5)

# Faster ranger (itself a faster version of RF).
SL.ranger_fast = function(...) SL.ranger(..., num.trees = 200, num.threads = RhpcBLASctl::get_num_cores())

# Restrict to top 4 variables based on univariate correlation.
# TODO: keep treatment indicator in if we're running outcome regression.
screen.corRank4 = function(...) {
  screen.corRank(..., rank = 4)
}

# Restrict to top 8 variables based on univariate correlation.
# TODO: keep treatment indicator in if we're running outcome regression.
screen.corRank8 = function(...) {
  screen.corRank(..., rank = 8)
}