#' @title Mean Misclassification Error Measure
#'
#' @name mlr_measures_ordinal.ce
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce].
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
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      Metrics::ce(actual = factor(as.character(prediction$truth), levels = l, ordered = TRUE), predicted = prediction$response)
    }
  )
)
