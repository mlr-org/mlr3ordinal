#' @title PipeOpConvertOrdinalTask
#'
#' @name mlr_pipeop_convertordinaltask
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Converts an ordinal task to a regression one.
#'
#' @family PipeOps
#' @export
PipeOpConvertOrdinalTask = R6Class("PipeOpConvertOrdinalTask",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "convertordinaltask", param_vals = list()) {
      super$initialize(id = id, param_vals = param_vals)
    },
    train_task = function(task) {
      private$convert_task_to_regression(task)
    },
    predict_task = function(task) {
      private$convert_task_to_ordinal(task)
    }
  ),
  private = list(
    convert_task_to_regression = function(task) {
      d = task$data()
      TaskOrdinalRegr$new(id = "threshold_task", backend = as_data_backend(d), target = task$target_names)
    },
    convert_task_to_ordinal = function(task) {
      d = task$data()
      d[[task$target_names]] = task$target_ordinal()
      TaskOrdinal$new(id = "ordinal_task", backend = as_data_backend(d), target = task$target_names)
    }
  )
)
