#' @title Mean Misclassification Error Measure
#'
#' @name mlr_measures_ordinal_ce
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

    calculate = function(experiment, prediction = experiment$prediction) {
      if ("Experiment" %in% class(experiment)) {
        levels = experiment$task$rank_names
      } else {
        levels = experiment$levels
      }

      if (!is.ordered(prediction$response) && !is.null(levels)) {
        response = ordered(prediction$response, levels = levels)
      } else {
        response = prediction$response
      }
      if (!is.ordered(prediction$truth) && !is.null(levels)) {
        truth = ordered(prediction$truth, levels = levels)
      } else {
        truth = prediction$truth
      }
      Metrics::ce(actual = truth, predicted = response)
    })
)
