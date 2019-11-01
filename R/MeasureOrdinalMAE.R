#' @title Mean Absolute Error Measure
#'
#' @name mlr_measures_ordinal.mae
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [mlr3measures::mae].
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalMAE = R6Class("MeasureOrdinalMAE",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "ordinal.mae") {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        packages = "mlr3measures"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      mlr3measures::mae(
        as.integer(prediction$truth),
        as.integer(prediction$response)
      )
    }
  )
)
