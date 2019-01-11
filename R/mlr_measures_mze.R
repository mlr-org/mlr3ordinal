#' @title Mean Zero-One Error Measure
#'
#' @name mlr_measures_mze
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce].
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalMZE = R6Class("MeasureOrdinalMZE",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "mze") {
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
      Metrics::ce(actual = factor(as.character(p$truth), levels = l), predicted = p$response)
    }
  )
)
