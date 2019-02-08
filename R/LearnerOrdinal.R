#' @title Ordinal Regression Learner
#'
#' @name LearnerOrdinal
#' @format [R6Class] object inheriting from [Learner].
#' @description
#' This Learner specializes [Learner] for ordinal regression tasks.
#'
#' @section Usage:
#' See [Learner].
#'
#' @family Learner
#' @examples
#' library(mlr3)
#' ids = mlr_learners$ids("^ordinal")
#' lrns = mlr_learners$mget(ids)
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("ordinal.clm")
#' print(lrn)
NULL

#' @export
LearnerOrdinal = R6Class("LearnerOrdinal", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_type = "response", predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "ordinal", feature_types = feature_types, predict_type = predict_type, predict_types = predict_types, packages = packages,
        param_set = param_set, param_vals = param_vals, properties = properties)
      assert_subset(self$properties, mlr_reflections$learner_properties$ordinal)
      private$.predict_type = predict_types[1L]
    }
  )
)
