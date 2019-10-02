#' @title Ordinal Regression Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Measure].
#'
#' @description
#' This measure specializes [mlr3::Measure] for ordinal regression problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' The `task_type` is set to `"ordinal"`.
#'
#' @section Construction:
#' ```
#' m = MeasureOrdinal$new(id, range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
#' ```
#' For a description of the arguments, see [mlr3::Measure].
#' The `task_type` is set to `"ordinal"`.
#' Possible values for `predict_types` is "response" and "prob".
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @seealso Example ordinal measure: [`ordinal.ce`][mlr_measures_ordinal.ce].
#' @export
MeasureOrdinal = R6Class("MeasureOrdinal", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "ordinal", range = range, minimize = minimize, aggregator = aggregator,
        properties = properties, predict_type = predict_type, task_properties = task_properties, packages = packages)
    }
  )
)
