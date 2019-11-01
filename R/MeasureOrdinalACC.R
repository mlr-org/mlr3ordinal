#' @title Mean Accuracy Measure
#'
#' @name mlr_measures_ordinal_acc
#' @format [R6::R6Class()] inheriting from [MeasureOrdinal].
#'
#' @description
#' Calls [mlr3measures::ce] for calculating the classification error. Resulting ACC = 1 - CE.
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
        packages = "mlr3measures"
      )
    },

    score_internal = function(prediction, ...) {
      l = levels(prediction$truth)
      truth = factor(prediction$truth, levels = l, ordered = TRUE)
      mlr3measures::acc(truth, prediction$response)
    }
  )
)
