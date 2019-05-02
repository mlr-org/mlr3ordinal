#' @title Ordinal Regression Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [TaskRegr]/[Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for ordinal regression problems.
#' If the target column is an ordered factor, it is transformed to a numeric vector.
#' The original orderd target is stored in the [DataBackend] with [col_role = "target_ordinal"]
#' Predefined tasks are stored in [mlr_tasks].
#'
#' The `task_type` is set to `"regr"`.
#'
#' @section Construction:
#' ```
#' t = TaskOrdinalRegr$new(id, backend, target, target_ordinal)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: ([DataBackend] | `data.frame()` | ...)\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' * `target_ordinal` :: `character(1)`\cr
#'   Name of the original ordered target column.
#'
#' @section Fields:
#' @inheritSection Task Fields
#'
#' @section Methods:
#' @inheritSection Task Methods
#'
#' @family Task
#' @export
#' @examples
#' task = TaskOrdinalRegr$new("wine", backend = iris, target = "rating")
#' task$task_type
#' task$formula()
#' task$truth()
#'
#' # possible properties:
#' mlr_reflections$task_properties$ordinalregr
TaskOrdinalRegr = R6Class("TaskOrdinalRegr",
  inherit = TaskRegr,
  public = list(
    initialize = function(id, backend, target) {
      assert_string(target)
      if (is.factor(backend$data(rows = backend$rownames, cols = target)) {
        data = backend$data(rows = backend$rownames, cols = backend$colnames)
        target_ordinal = data[[target]]
        data[[target]] = as.numeric(data[[target]])
        data = cbind(data, target_ordinal)
        backend = as_data_backend(data)
      }

      super$initialize(id = id, task_type = "regr", backend = backend, target = target)
      type = self$col_info[id == target]$type
      self$set_col_role("target_ordinal", "target_ordinal")
      if (type %nin% c("integer", "numeric"))
        stopf("Target column '%s' must be numeric", target)
      self$measures = list(mlr_measures$get("regr.mse"))
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)[[1L]]
    }
  )
)
