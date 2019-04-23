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

    calculate = function(experiment, prediction = experiment$prediction) {
      if ("Experiment" %in% class(experiment))
        levels = experiment$task$all_ranks
      else
        levels = experiment$levels

      response = ordered(prediction$response, levels = levels)
      truth = ordered(prediction$truth, levels = levels)
      Metrics::ce(actual = truth, predicted = response)
    }
  )
)
