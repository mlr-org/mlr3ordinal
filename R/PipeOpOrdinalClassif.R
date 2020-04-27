#' @title PipeOpOrdinalClassif
#'
#' @format [R6Class] PipeOpOrdinalClassif
#'
#' @name mlr_pipeop_ordinalclassif
#' @format [`R6::R6Class`] inheriting from [`mlr3pipelines::PipeOpTaskPreproc`].
#'
#' @description
#' This PipeOp works for any classification learner.
#' The idea is to discard the ordinal structure of the target variable and consider the whole task as a [`Classification Task`][mlr3::TaskClassif].
#' Returns a single [`PredictionOrdinal`].
#' As a default, optimizes [`MeasureOrdinalCE`].
#'
#' @family PipeOps
#' @examples
#' library(mlr3pipelines)
#' op = po("ordinalclassif")
#' @export
PipeOpOrdinalClassif = R6Class("PipeOpOrdinalClassif",
  inherit = PipeOp,

  public = list(
    measure = NULL,
    threshold = NULL,
    initialize = function(innum, id = "ordinalclassif", param_vals = list()) {
      assert_int(innum, lower = 1)
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL, tags = "train")
      ))
      super$initialize(id, param_vals = param_vals, param_set = ps,
        input = data.table(name = mlr3pipelines:::rep_suffix("input", innum), train = "NULL", predict = c("PredictionClassif", "Task")),
        output = data.table(name = "output", train = "NULL", predict = "PredictionOrdinal")
      )
    },

    train = function(inputs) {
      return(list(NULL))
    },

    predict = function(inputs) {
      pred = private$make_prediction_ordinal(inputs)
      return(list(pred))
    }),

  private = list(
    make_prediction_ordinal = function(inputs) {
      pred = inputs[[1]]
      task = inputs[[2]]
      l = task$rank_names
      p = PredictionOrdinal$new(
        row_ids = pred$row_ids,
        truth = factor(pred$truth, levels = l, ordered = TRUE),
        response = factor(pred$response, levels = l, ordered = TRUE)
      )
      return(p)
    }
  )
)
