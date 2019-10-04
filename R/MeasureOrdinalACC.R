#' @title Mean Accuracy Measure
#'
#' @name mlr_measures_ordinal_acc
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce] for calculating the classification error. Resulting ACC = 1 - CE.
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalACC = R6Class("MeasureOrdinaACC",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "ordinal.acc") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      Metrics::accuracy(actual = factor(prediction$truth, levels = l, ordered = TRUE), predicted = prediction$response)
    }
  )
)
