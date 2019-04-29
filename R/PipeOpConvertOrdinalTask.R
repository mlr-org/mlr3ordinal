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
      private$convert_task(task)
    },
    predict_task = function(task) {
      private$convert_task(task)
    }
  ),
  private = list(
    convert_task = function(task) {
      d = task$data()
      target_ordinal = d[[task$target_names]]
      d[[task$target_names]] = as.integer(d[[task$target_names]])
      d = cbind(d, target_ordinal)
      new_task = TaskRegr$new(id = "threshold_task", backend = as_data_backend(d), target = task$target_names)
      new_task$col_roles$feature = setdiff(new_task$col_roles$feature, "target_ordinal")
      new_task$col_roles$target_ordinal = "target_ordinal"
      new_task
    }
  )
)
