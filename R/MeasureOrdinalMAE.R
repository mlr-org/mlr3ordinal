#' @title Mean Absolute Error Measure
#'
#' @name mlr_measures_ordinal.mae
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [Metrics::mae].
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
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      Metrics::mae(
        actual = as.integer(factor(as.character(prediction$truth), levels = l, ordered = TRUE)),
        predicted = as.integer(prediction$response)
      )
    }
  )
)