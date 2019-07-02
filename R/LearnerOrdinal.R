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
#' ids = mlr_learners$keys("^ordinal")
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
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {
      super$initialize(id = id, task_type = "ordinal", param_set = param_set, param_vals = param_vals,
        predict_types = predict_types, feature_types = feature_types, properties = properties,
        data_formats = data_formats, packages = packages)
    },

    new_prediction = function(row_ids, truth, response = NULL, prob = NULL) {
      row_ids = assert_row_ids(row_ids)
      n = length(row_ids)
      lvls = levels(truth)

      if (!is.null(response)) {
        response = as_factor(response, levels = lvls)
        assert_factor(response, len = n)
      }

      if (!is.null(prob)) {
        assert_matrix(prob, nrows = n, ncols = length(lvls))
        assert_numeric(prob, lower = 0, upper = 1)
        assert_names(colnames(prob), permutation.of = lvls)
        if (!is.null(rownames(prob))) {
          rownames(prob) = NULL
        }

        if (is.null(response)) {
          # calculate response from prob
          i = max.col(prob, ties.method = "random")
          response = factor(colnames(prob)[i], levels = lvls, ordered = TRUE)
        }
      }

      PredictionOrdinal$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
    }
  )
)
