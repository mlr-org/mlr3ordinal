#' @title Mean Misclassification Error Measure
#'
#' @name mlr_measures_ordinal_mmce
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::ce].
#'
#' @export
#' @include MeasureOrdinal.R
MeasureOrdinalMMCE = R6Class("MeasureOrdinaMMCE",
  inherit = MeasureOrdinal,
  public = list(
    initialize = function(id = "ordinal.mmce") {
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
