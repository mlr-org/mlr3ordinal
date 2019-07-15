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
#' task = mlr_tasks$get("winerating")
#' learner = mlr_learners$get("ordinal.clm")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
NULL

PredictionOrdinal = R6Class("PredictionOrdinal",
  inherit = Prediction,
  cloneable = FALSE,
  public = list(
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, prob = NULL) {
      self$data$row_ids = assert_atomic_vector(row_ids)
      self$data$truth = assert_factor(truth, ordered = TRUE)
      self$data$prob = assert_matrix(prob, null.ok = TRUE)
      self$data$response = response
      self$task_type = "ordinal"
    },

    set_threshold = function(threshold, ranks = NULL) {
      # if (!is.matrix(self$data$prob) && !is.numeric(self$data$response)) {
      #   stopf("Cannot set threshold, no probabilities available or response is not numeric")
      # }

      if (!is.null(self$prob)) {
        if (!is.matrix(self$prob)) {
          stopf("Cannot set threshold, no probabilities available")
        }
        ranks = colnames(self$data$prob)

        if (length(threshold) == 1L) {
          assert_number(threshold, lower = 0, upper = 1)
          if (length(ranks) != 2L) {
            stopf("Setting a single threshold only supported for binary ordinal problems")
          }
          prob = cbind(self$data$prob[, 1L], threshold)
        } else {
          assert_numeric(threshold, any.missing = FALSE, lower = 0, upper = 1, len = length(ranks))
          assert_names(names(threshold), permutation.of = ranks)
          threshold = threshold[ranks] # reorder thresh so it is in the same order as levels

          # multiply all rows by threshold, then get index of max element per row
          w = ifelse(threshold > 0, 1 / threshold, Inf)
          prob = self$prob %*% diag(w)
        }

        ind = max.col(prob, ties.method = "random")
        self$data$response = factor(ranks[ind], levels = ranks, ordered = TRUE)
      } else if (is.numeric(self$data$response)) {
        if (is.null(ranks)) {
          stopf("For numeric response, original rank labels are needed")
        }
        res = private$set_ranks_ordinal(self$data$response, threshold)
        self$data$response = factor(res, levels = ranks, ordered = TRUE)
      }
      self
    }
  ),

  active = list(
    response = function() self$data$response %??% factor(rep(NA, length(self$data$row_ids)), levels(self$data$truth), ordered = TRUE),
    prob = function() self$data$prob,
    confusion = function() {table(response = self$response, truth = self$truth, useNA = "ifany")},
    missing = function() {
      miss = logical(length(self$data$row_ids))
      if (!is.null(self$data$response))
        miss = miss | is.na(self$data$response)
      if (!is.null(self$data$prob))
        miss = miss | apply(self$data$prob, 1L, anyMissing)

      self$data$row_ids[miss]
    }
  ),


  private = list(
    # .threshold = NULL,
    set_ranks_ordinal = function(response, threshold) {
      t = c(-Inf, threshold, Inf)
      as.numeric(cut(response, breaks = t))
    }
  )
)

#' @export
as.data.table.PredictionOrdinal = function(x, ...) {
  data = x$data
  if (is.null(data$row_ids)) {
    return(data.table())
  }
  tab = data.table(row_id = data$row_ids, truth = data$truth, response = data$response)
  if (!is.null(data$prob)) {
    prob = as.data.table(data$prob)
    setnames(prob, names(prob), paste0("prob.", names(prob)))
    tab = rcbind(tab, prob)
  }

  tab
}

#' @export
c.PredictionOrdinal = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionOrdinal")
  assert_flag(keep_duplicates)

  x = map_dtr(dots, function(p) {
    list(row_ids = p$row_ids, truth = p$truth, response = p$response)
  }, .fill = FALSE)

  prob = discard(map(dots, "prob"), is.null)
  if (length(prob) > 0L && length(prob) < length(dots)) {
    stopf("Cannot rbind predictions: Probabilities for some predictions, not all")
  }

  prob = Reduce(x = prob, f = function(x, y) {
    assert_set_equal(colnames(x), colnames(y))
    rbind(x, y[, match(colnames(x), colnames(y)), drop = FALSE])
  })

  if (!keep_duplicates) {
    keep = !duplicated(x$row_ids, fromLast = TRUE)
    x = x[keep]
    prob = prob[keep,, drop = FALSE]
  }

  PredictionOrdinal$new(row_ids = x$row_ids, truth = x$truth, response = x$response, prob = prob)
}


