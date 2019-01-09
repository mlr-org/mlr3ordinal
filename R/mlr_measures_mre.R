#' @title Mean Rank Error Measure
#'
#' @name mlr_measures_mre
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce].
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalMRE = R6Class("MeasureOrdinalMRE",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "mre") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      Metrics::ce(actual = p$truth, predicted = p$response)
    }
  )
)
