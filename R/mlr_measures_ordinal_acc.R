#' @title Mean Accuracy Measure
#'
#' @name mlr_measures_ordinal_acc
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce] for calculating the mean missclassification error. Resulting ACC = 1 - MMCE.
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
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      l = levels(p$truth)
      1 - Metrics::ce(actual = factor(as.character(p$truth), levels = l), predicted = p$response)
    }
  )
)
