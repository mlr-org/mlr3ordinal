#' @title Mean Misclassification Error Measure
#'
#' @name mlr_measures_ordinal.ce
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [mlr3measures::ce].
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalCE = R6Class("MeasureOrdinalCE",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "ordinal.ce") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = TRUE,
        packages = "mlr3measures"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      truth = factor(prediction$truth, levels = l, ordered = TRUE)
      mlr3measures::ce(truth, prediction$response)
    }
  )
)
