# This is the revere function which will output the first pass revere
# not fitting again on full data, so IC equation here is certainly solved

revere_cvtmle_basic =
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
  QAW_task = make_sl3_Task(data = data,
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
  cv_lrnr_fit = cv_lrnr_Q$train(QAW_task_sub)
  
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
  g1W_task = make_sl3_Task(data = data,
                           covariates = covariates_g,
                           outcome = treatment_field,
                           folds = folds)
  
  # fit on training folds
  cv_lrnr_fitg = cv_lrnr_g$train(g1W_task)
  
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
  
  # fit on folds and predict on subsetted folds for g and c
  c1W_task = make_sl3_Task(data = data, covariates = covariates_Q,
                           outcome = censor_field,
                           folds = folds)
  
  # need to fit for A=1 and for A=0 so we make these tasks
  c1W_taskA1 = make_sl3_Task(data = dataQ1W, covariates = covariates_Q,
                             outcome = censor_field,
                             folds = folds)
  c1W_taskA0 = make_sl3_Task(data = dataQ0W, covariates = covariates_Q,
                             outcome = censor_field,
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
  tmle_info = tmle(Y = y,
                   A = z,
                   W = W,
                   Delta = 1 - C,
                   Q = Q,
                   pDelta1 = pDelta1,
                   g1W = g1W,
                   family = "gaussian",
                   fluctuation = "logistic")
  
  # grab the definitive epsilon

  preds_star = tmle_info$Qstar[,1]*(1-z) + tmle_info$Qstar[,2]*z
  CATE_star = tmle_info$Qstar[,2] - tmle_info$Qstar[,1]
  preds_init = tmle_info$Qinit$Q[,1]*(1-z) + tmle_info$Qinit$Q[,2]*z
  CATE_init = tmle_info$Qinit$Q[,2] - tmle_info$Qinit$Q[,1]
  
  CI = c(tmle_info$estimates$ATE$psi, tmle_info$estimates$ATE$CI)
  
  # Compile individual predictions.
  preds_all = data.frame(preds_star = preds_star,
                         CATE_star = CATE_star,
                         preds_init = preds_init,
                         CATE_init = CATE_init)
  
  # Compile results.
  results =
    list(preds_all = preds_all,
         ate_est = tmle_info$estimates$ATE$psi,
         conf_int = tmle_info$estimates$ATE$CI,
         CI = CI)
  
  return(results)
}


# a function that saves our lives here--jeremy genius
subset_fold_training <- function(fold, subset_index){
  new_training <- intersect(training(),subset_index)
  new_validation <- intersect(validation(),subset_index)
  list(fold=make_fold(fold_index(),new_training,new_validation))
}

# sort n-dimensional array (for multinomial/multivariate SL support)
aorder <- function(mat, index, along = 1) {
  dims <- safe_dim(mat)
  args <- ifelse(along == seq_along(dims), "index", "")
  indexer <- paste(c(args, "drop=F"), collapse = ",")
  call <- sprintf("mat[%s]", indexer)
  result <- eval(parse(text = call))
  
  return(result)
}

Lrnr_cv2 <- R6Class(
  classname = "Lrnr_cv2",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learner, folds = NULL, ...) {
      # check if learner is a list of learners, and if so, make a stack
      if (is.list(learner) && all(sapply(learner, inherits, "Lrnr_base"))) {
        learner <- Stack$new(learner)
      }
      params <- list(learner = learner, folds = folds, ...)
      super$initialize(params = params, ...)
    },
    
    cv_risk = function(loss) {
      preds <- self$predict()
      task <- self$training_task
      risks <- apply(preds, 2, risk, task$Y, loss, task$weights)
    },
    
    print = function() {
      print("Lrnr_cv")
      print(self$params$learner)
      # todo: check if fit
    }
  ),
  
  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
    }
  ),
  
  private = list(
    .properties = c("wrapper"),
    
    .train_sublearners = function(task) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        # TODO: this breaks if task is delayed
        folds <- task$folds
      }
      learner <- self$params$learner
      
      train_task <- function(task, fold) {
        return(task[fold$training_set])
      }
      
      delayed_train_task <- delayed::delayed_fun(train_task)
      
      delayed_cv_train <- function(fold, learner, task) {
        training_task <- delayed_train_task(task, fold)
        training_task$sequential <- TRUE
        fit_object <- delayed_learner_train(learner, training_task)
        return(fit_object)
      }
      
      # TODO: maybe write delayed_cross_validate (as it'd be a neat thing to
      # have around anyway)
      cv_results <- lapply(folds, delayed_cv_train, learner, task)
      result <- bundle_delayed(cv_results)
      return(result)
    },
    
    .train = function(task, trained_sublearners) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        folds <- task$folds
      }
      
      fold_fits <- trained_sublearners
      learner <- self$params$learner
      ever_error <- NULL
      
      if (inherits(learner, "Stack")) {
        # if we're cross-validating a stack, check for learner errors in any
        # folds and then drop for all folds
        errored_learners <- sapply(fold_fits, function(fold_fit) {
          fold_fit$fit_object$is_error
        })
        if (is.vector(errored_learners)) {
          errored_learners <- matrix(
            errored_learners,
            ncol = length(errored_learners)
          )
        }
        ever_error <- apply(errored_learners, 1, any)
        if (any(ever_error)) {
          # drop errored learners from all folds
          for (fold_fit in fold_fits) {
            fold_fit$update_errors(ever_error)
          }
          # warn about dropping
          errored_learners <- learner$params$learners[ever_error]
          errored_names <- sapply(errored_learners, `[[`, "name")
          all_names <- paste(errored_names, collapse = ", ")
          warning(paste(
            "The following learners failed for one or more folds",
            "and will be dropped from all folds: ", all_names
          ))
        }
      }
      
      fit_object <- list(
        folds = folds, fold_fits = fold_fits,
        is_error = ever_error
      )
      return(fit_object)
    },
    
    .predict = function(task) {
      # if(!identical(task,private$.training_task)){
      #   stop("task must match training task for Lrnr_cv")
      # }
      # doing train and predict like this is stupid, but that's the paradigm
      # (for now!)
      folds <- task$folds
      fold_fits <- private$.fit_object$fold_fits
      
      cv_predict <- function(fold, fold_fits, task) {
        validation_task <- validation(task)
        index <- validation()
        fit <- fold_index(fold_fits)[[1]]
        predictions <- fit$base_predict(validation_task)
        list(index = index, predictions = predictions)
      }
      
      # fold_predictions = cross_validate(cv_predict, folds, fold_fits, task,
      # future.globals = FALSE)
      # don't use cross_validate as it will call future_lapply
      fold_predictions <- lapply(folds, cv_predict, fold_fits, task)
      index <- unlist(lapply(fold_predictions, `[[`, "index"))
      predictions <- data.table::rbindlist(lapply(
        fold_predictions, `[[`,
        "predictions"
      ))
      predictions <- aorder(predictions, order(index))
      return(predictions)
    },
    .required_packages = c("origami")
  )
)

