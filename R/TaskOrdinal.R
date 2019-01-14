#' @title Ordinal Task
#'
#' @name TaskOrdinal
#' @format [R6Class] object inheriting from [mlr3::Task]/[mlr3::TaskSupervised].
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for ordinal regression problems.
#'
#' @section Usage:
#' Inherits from [mlr3::Task]/[mlr3::TaskSupervised].
#' ```
#' # Construction
#' t = TaskSupervised$new(id, backend, target)
#'
#' @examples
#' library(mlr3)
#' #...
NULL

#' @export
TaskOrdinal = R6Class("TaskOrdinal",
  inherit = TaskSupervised,
  public = list(

    initialize = function(id, backend, target) {
      super$initialize(id = id, task_type = "ordinal", backend = backend, target = target)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("ordered", "factor"))
        stopf("Target column '%s' must be an ordered factor", target)
      # if (info$type != "ordered")
        # stopf("Target column '%s' must be an ordered factor", target)
      if (length(levels) < 2L)
        stopf("Target column '%s' must have at least two levels", target)

      self$measures = list(mlr_measures$get("ordinal.mmce"))
    },

    truth = function(row_ids = NULL) {
      res = self$data(row_ids, cols = self$target_names)[[1L]]
    }
  ),

  active = list(
    rank_names = function() as.character(unique(self$truth())),

    rank_n = function() uniqueN(self$truth()),

    all_ranks = function() {
      # TODO: this operation is slow for small data, and we do this quite often
      # we might want to optimize here in the future
      self$col_info[list(self$target_names), "levels", on = "id", nomatch = 0L, with = FALSE][[1L]][[1L]]
    }
  )
)
