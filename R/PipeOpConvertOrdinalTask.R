#' @title PipeOpConvertOrdinalTask
#'
#' @name mlr_pipeop_convertordinaltask
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Converts a task of type [Ordinal] to an [OrdinalRegr] one and vice versa.
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
      private$convert_task(task)
    },
    predict_task = function(task) {
      private$convert_task(task)
    }),
  private = list(
    convert_task = function(task) {
      if ("TaskOrdinal" %in% class(task)) {
        private$convert_task_to_regression(task)
      } else if ("TaskOrdinalRegr" %in% class(task)) {
        private$convert_task_to_ordinal(task)
      }
    },
    convert_task_to_regression = function(task) {
      d = task$data()
      TaskOrdinalRegr$new(id = "threshold_task", backend = as_data_backend(d), target = task$target_names)
    },
    convert_task_to_ordinal = function(task) {
      d = task$data()
      d[[task$target_names]] = task$target_ordinal()
      TaskOrdinal$new(id = "ordinal_task", backend = as_data_backend(d), target = task$target_names)
    })
)
