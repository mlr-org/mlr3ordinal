#' @title Ordinal Regression Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [TaskRegr]/[Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for ordinal regression problems.
#' If the target column is an ordered factor, it is transformed to a numeric vector.
#' The original orderd target is stored in the [mlr3::DataBackend] with [col_role = "target_ordinal"]
#' Predefined tasks are stored in [mlr3::mlr_tasks].
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
#' @section Methods:
#' * `target_ordinal(row_ids = NULL)` \cr
#'   (`integer()`) -> `self`\cr
#'   If applicable, the original ordered ordinal target variable is returned.
#'
#' @family TaskRegr
#' @export
#' @examples
#' task = TaskOrdinalRegr$new("wine", backend = iris, target = "rating")
#' task$task_type
#' task$formula()
#' task$truth()
#'
#' # possible properties:
#' mlr_reflections$task_properties$regr
TaskOrdinalRegr = R6Class("TaskOrdinalRegr",
  inherit = TaskRegr,
  public = list(
    initialize = function(id, backend, target) {
      assert_string(target)
      if (is.factor(backend$data(rows = backend$rownames, cols = target)[[1L]])) {
        data = backend$data(rows = backend$rownames, cols = backend$colnames)
        target_ordinal = data[[target]]
        data[[target]] = as.numeric(data[[target]])
        data = cbind(data, target_ordinal)
        backend = as_data_backend(data)
      }

      super$initialize(id = id, backend = backend, target = target)
      self$set_col_role("target_ordinal", "target_ordinal")
    },

    target_ordinal = function(row_ids = NULL) {
      row_ids = ifelse(is.null(row_ids), self$row_ids, row_ids)
      self$backend$data(rows = self$row_ids, cols = self$col_roles$target_ordinal)[[1L]]
    }),

  active = list(
    rank_names = function() {
      self$col_info[list("target_ordinal"), "levels", with = FALSE][[1L]][[1L]]
    }, #  as.character(levels(self$target_ordinal())),

    rank_n = function() uniqueN(self$target_ordinal())
  )
)
