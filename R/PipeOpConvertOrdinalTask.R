#' @title PipeOpConvertOrdinalTask
#'
#' @name mlr_pipeop_convertordinaltask
#' @format [`R6Class`] object inheriting from [mlr3pipelines::PipeOpTaskPreproc].
#'
#' @description
#' Converts an [TaskOrdinal] to an [mlr3::TaskRegr] or [mlr3::TaskClassif] one, depending on `type`.
#'
#' @family PipeOps
#' @export
PipeOpConvertOrdinalTask = R6Class("PipeOpConvertOrdinalTask",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "convertordinaltask", param_vals = list(), type = NULL) {
      super$initialize(id = id, param_vals = param_vals)
      self$type = type
    },
    train_task = function(task) {
      private$convert_task(task)
    },
    predict_task = function(task) {
      private$convert_task(task)
    },
    type = NULL
  ),
  private = list(
    convert_task = function(task) {
      if (self$type == "regr") {
        private$convert_task_to_regression(task)
      } else if (self$type == "classif") {
        private$convert_task_to_classification(task)
      }
    },
    convert_task_to_regression = function(task) {
      d = task$data()
      d[[task$target_names]] = as.numeric(d[[task$target_names]])
      TaskRegr$new(id = task$id, backend = as_data_backend(d), target = task$target_names)
    },
    convert_task_to_classification = function(task) {
      d = task$data()
      TaskClassif$new(id = task$id, backend = as_data_backend(d), target = task$target_names)
    }
  )
)
