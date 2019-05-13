#' @title Prediction Object for Ordinal Regression
#'
#' @name PredictionOrdinal
#' @format [R6Class] object inheriting from [Prediction].
#' @description
#' This object stores the predictions returned by a learner of class [LearnerOrdinal].
#'
#' @section Usage:
#' Inherits from [Prediction]
#' ```
#' # Construction
#' p = PredictionOrdinal$new(task, response, prob)
#'
#' # Members
#' p$predict_types
#' p$row_ids
#' p$truth
#'
#' # S3 methods
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case.
#' * `response` (`factor()` | `ordered()`): Vector of predicted class labels.
#' * `prob` (`matrix`):
#'   Numeric matrix of rank probabilities with one column for each rank in `task$class_names`
#'   and one row for each observation in the test set.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Prediction].
#' * `$predict_types` ([character]) stores the predict types available: a subset of `c("response", "prob")`.
#' * `$response` stores the predicted values.
#' * `row_ids` stores the row IDs.
#' * `$truth` stores the true rank vector.
#' * The prediction object can be transformed to a simple [data.table()]
#'   with [data.table::as.data.table].
#' @export
#' @family Prediction
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("wine")
#' learner = mlr_learners$get("ordinal.clm")
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' head(as.data.table(p))
NULL

PredictionOrdinal = R6Class("PredictionOrdinal",
  inherit = Prediction,
  cloneable = FALSE,
  public = list(
    prob = NULL,
    initialize = function(task = NULL, response = NULL, prob = NULL) {
      predictionordinal_initialize(self, task, response, prob)
    }),

  active = list(
    threshold = function(rhs) {
      if (missing(rhs)) {
        return(private$.threshold)
      }
      if (!is.matrix(self$prob) && !is.numeric(self$response)) {

      if (!is.null(self$prob)) {
        if (!is.matrix(self$prob)) {
          stopf("Cannot set threshold, no probabilities available")
        }
        lvls = colnames(self$prob)

        if (length(rhs) == 1L) {
          if (length(lvls) != 2L) {
            stopf("Setting a single threshold only supported for binary classification problems")
          }
          assert_number(rhs, lower = 0, upper = 1)
          ind = max.col(cbind(self$prob[, 1L], rhs), ties.method = "random")
        } else {
          assert_numeric(rhs, any.missing = FALSE, lower = 0, upper = 1, len = length(lvls))
          assert_names(names(rhs), permutation.of = lvls)
          rhs = rhs[lvls] # reorder rhs so it is in the same order as levels

          # multiply all rows by threshold, then get index of max element per row
          w = ifelse(rhs > 0, 1 / rhs, Inf)
          ind = max.col(self$prob %*% diag(w), ties.method = "random")
        }
        private$.threshold = rhs
        self$response = factor(lvls[ind], levels = lvls)
      } else if (is.numeric(self$response)) {

      }
      private$.threshold = rhs
    },

    confusion = function() {
      table(response = self$response, truth = self$truth, useNA = "ifany")
    }),

  private = list(
    set_ranks_ordinal = function(response) {
      t = c(-Inf, private$.threshold, Inf)
      as.numeric(cut(response, breaks = t))
    },
    .threshold = NULL,
  )
)

predictionordinal_initialize = function(self, task, response, prob) {
  self$task_type = "ordinal"
  if (!is.null(task)) {
    self$row_ids = row_ids = task$row_ids
    self$truth = task$truth()
    n = length(row_ids)
    ranks = task$rank_names

    if (!is.null(response)) {
      if (is.character(response)) {
        response = factor(response, levels = ranks)
      }
      assert_factor(response, len = n, levels = ranks, any.missing = FALSE)
    }

    if (!is.null(prob)) {
      assert_matrix(prob, nrows = n, ncols = length(ranks))
      assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
      assert_names(colnames(prob), permutation.of = ranks)
      if (is.null(rownames(prob))) {
        rownames(prob) = row_ids
      }
      self$prob = prob[, match(colnames(prob), ranks), drop = FALSE]
    }

    if (is.null(response) && !is.null(prob)) {
      # calculate response from prob
      response = factor(colnames(prob)[unname(apply(prob, 1L, which_max))], levels = ranks)
    }
  } else {
    if (!is.null(response) && is.character(response)) {
      response = factor(response)
    }
    assert_factor(response, any.missing = FALSE, null.ok = TRUE)
    assert_matrix(prob, null.ok = TRUE)
    assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1, null.ok = TRUE)
  } else if (!is.null(response) && is.numeric(response)) {
    ranks = self$set_ranks_ordinal(resonse)
    response = factor(ranks, levels = )
  }

  self$predict_types = c("response", "prob")[c(!is.null(response), !is.null(prob))]
  self$response = response
  self$prob = prob
}

#' @export
as.data.table.PredictionOrdinal = function(x, ...) {
  tab = data.table(row_id = x$row_ids, response = x$response, truth = x$truth)
  if (!is.null(x$prob)) {
    tab[, paste0("prob.", colnames(x$prob)) := as.data.table(x$prob)]
  }
  tab
}
