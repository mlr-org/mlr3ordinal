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
      assert_row_ids(row_ids)
      n = length(row_ids)

      truth = assert_factor(truth, len = n, null.ok = TRUE, ordered = TRUE)
      ranks = levels(truth)

      # if (!is.null(response)) {
      #   response = assert_factor(as_factor(response, levels = lvls), len = n)
      # }

      if (!is.null(prob)) {
        assert_matrix(prob, nrows = n, ncols = length(ranks))
        assert_numeric(prob, lower = 0, upper = 1)
        assert_names(colnames(prob), permutation.of = ranks)
        if (!is.null(rownames(prob))) {
          rownames(prob) = NULL
        }

        if (is.null(response)) {
          # calculate response from prob
          i = max.col(prob, ties.method = "random")
          response = factor(colnames(prob)[i], levels = ranks, ordered = TRUE)
        }
      }

      self$task_type = "ordinal"
      self$predict_types = c("response", "prob")[c(!is.null(response), !is.null(prob))]
      self$data$tab = data.table(
        row_id = row_ids,
        truth = truth,
        response = response
      )
      self$data$prob = prob
    },

    set_threshold = function(threshold, ranks = NULL) {
      # if (!is.matrix(self$data$prob) && !is.numeric(self$data$response)) {
      #   stopf("Cannot set threshold, no probabilities available or response is not numeric")
      # }
      if (!is.null(self$data$prob)) {
        if (!is.matrix(self$data$prob)) {
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
      } else if (is.numeric(self$data$tab$response)) {
        if (is.null(ranks)) {
          stopf("For numeric response, original rank labels are needed")
        }
        res = private$set_ranks_ordinal(self$data$tab$response, threshold)
        if (is.na(as.numeric(ranks)[1])) {
          self$data$tab$response = factor(res, labels = ranks, ordered = TRUE)
        } else {
          self$data$tab$response = factor(res, levels = ranks, ordered = TRUE)
        }
      }
      self
    }
  ),

  active = list(
    truth = function() {
      self$data$tab$truth
    },

    response = function() {
      self$data$tab$response %??% factor(rep(NA, length(self$data$row_ids)), levels(self$data$tab$truth), ordered = TRUE)
    },

    prob = function() {
      self$data$prob
    },

    confusion = function() {
      self$data$tab[, table(response, truth, useNA = "ifany")]
    },

    missing = function() {
      miss = logical(nrow(self$data$tab))
      if ("response" %in% self$predict_types) {
        miss = is.na(self$data$tab$response)
      }
      if ("prob" %in% self$predict_types) {
        miss = miss | apply(self$data$prob, 1L, anyMissing)
      }

      self$data$tab$row_id[miss]
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
tab = copy(x$data$tab)
  if ("prob" %in% x$predict_types) {
    prob = as.data.table(x$data$prob)
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

  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Probabilities for some predictions, not all")
  }

  tab = map_dtr(dots, function(p) p$data$tab, .fill = FALSE)
  prob = do.call(rbind, map(dots, "prob"))

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_id", fromLast = TRUE)
    tab = tab[keep]
    prob = prob[keep,, drop = FALSE]
  }

  PredictionOrdinal$new(row_ids = tab$row_id, truth = tab$truth, response = tab$response, prob = prob)
}
