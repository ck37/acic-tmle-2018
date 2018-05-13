#' Non-negative Linear Least Squares (convex combo)
#'
#' This learner provides fitting procedures for models with non-negative linear
#' least squares, internally using the \code{nnls} package and
#' \code{\link[nnls]{nnls}} function.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for both training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Not used.}
#' }
#'
#' @template common_parameters
#
Lrnr_nnls_convex <- R6Class(
  classname = "Lrnr_nnls_convex", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params, ...)
    },
    print = function() {
      print(self$name)
      print(self$fits)
    }
  ),
  
  active = list(
    fits = function() {
      fit_object <- private$.fit_object
      if (!is.null(fit_object)) {
        data.table::data.table(lrnrs = fit_object$lrnrs, weights = fit_object$x)
      } else {
        data.table::data.table(lrnrs = character(0), weights = numeric(0))
      }
    }
  ),
  
  private = list(
    .properties = c("continuous"),
    .train = function(task) {
      x <- task$X
      y <- task$Y
      fit_object <- nnls::nnls(as.matrix(x), y)
      fit_object$lrnrs <- names(task$X)
      return(fit_object)
    },
    
    .predict = function(task = NULL) {
      coefs = coef(private$.fit_object)
      coefs[is.na(coefs)] = 0
      
      verbose <- getOption("sl3.verbose")
      if (sum(coefs) > 1) {
        if (verbose) {
          cat("Lrnr_nnls_convex: rescaling coefficients because original sum =",
              round(sum(coefs), 2))
        }
        # Rescale coefficients so they they sum to 1, ensuring that predictions
        # remain within the space defined by the original learners (convex combo).
        coefs = coefs / sum(coefs)
        
        if (verbose) {
          cat("Revised sum:", sum(coefs), "\n")
        }
      }
      predictions <- as.matrix(task$X) %*% coefs
      return(predictions)
    },
    .required_packages = c("nnls")
  ),
)
