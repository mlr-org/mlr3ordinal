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
      if ("Experiment" %in% class(e))
        levels = e$task$all_ranks
      else
        levels = e$levels

      response = ordered(e$prediction$response, levels = levels)
      truth = ordered(e$prediction$truth, levels = levels)
      Metrics::ce(actual = truth, predicted = response)
    }
  )
)
