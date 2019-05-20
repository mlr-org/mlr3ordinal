#' @title PipeOpOrdinalThresholds
#'
#' @format [R6Class] PipeOpOrdinalThresholds
#'
#' @name mlr_pipeop_ordinalthresholds
#' @format [`R6::R6Class`] inheriting from [`mlr3pipelines::PipeOpPredPostproc`].
#'
#' @description
#' This PipeOp works for any regression learner.
#' The idea is to predict numeric target values and optimizing ordinal class thresholds afterwards to label the predictions.
#' Here, optimal thresholds are searched over different [`PredictionRegr`]s.
#' Ordinal thresholds for each regression learner are optimized using (GenSA)[GenSA::GenSA].
#' Returns a single [`PredictionOrdinal`].
#' As a default, optimizes [`MeasureOrdinalCE`].
#' Used for regression [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpOrdinalThresholds$new(2)
#' @export
PipeOpOrdinalThresholds = R6Class("PipeOpOrdinalThresholds",
  inherit = PipeOp,

  public = list(
    measure = NULL,
    threshold = NULL,
    initialize = function(innum, id = "ordinalregression", param_vals = list()) {
      assert_int(innum, lower = 1)
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("algorithm", default = "GenSA", levels = c("GenSA")),
        ParamInt$new("maxit", default = 10000L, lower = 1L, upper = Inf),
        ParamDbl$new("threshold.stop", default = NULL, lower = -Inf, upper = Inf, special_vals = list(NULL)),
        ParamInt$new("nb.stop.improvement", default = NULL, lower = 0L, upper = Inf, special_vals = list(NULL)),
        ParamLgl$new("smooth", default = FALSE),
        ParamInt$new("max.call", default = 3000L, lower = 1L, upper = Inf),
        ParamDbl$new("max.time", default = NULL, lower = 1L, upper = Inf, special_vals = list(NULL)),
        ParamInt$new("temperature", default = 250L, lower = 1L, upper = Inf),
        ParamDbl$new("visiting.param", default = 2.5, lower = 0L, upper = Inf),
        ParamDbl$new("acceptance.param", default = -15, lower = -Inf, upper = Inf),
        ParamLgl$new("verbose", default = FALSE),
        ParamLgl$new("simple.function", default = TRUE)
        # FIXME: Possibly implement more params, currently not important
      ))
      ps$values = list(measure = NULL, algorithm = "GenSA", smooth = FALSE,
        max.call = 3000L, temperature = 250, visiting.param = 2.5,
        acceptance.param = -15, simple.function = TRUE)
      super$initialize(id, param_vals = param_vals, param_set = ps, packages = "GenSA",
        input = data.table(name = mlr3pipelines:::rep_suffix("input", innum), train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },
    train = function(inputs) {
      # browser()
      pred = private$make_prediction_ordinal(inputs)
      assert_class(pred, "PredictionOrdinal")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure))
        self$measure = mlr_measures$get("ordinal.ce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "ordinal")
      th = private$optimize_objfun_gensa(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    predict = function(inputs) {
      #
      pred = private$make_prediction_ordinal(inputs[[1L]])
      pred$threshold = self$state$threshold
      return(list(pred))
    }),
  private = list(
    objfun = function(threshold, pred) {
      pred$threshold(threshold)
      e = list("prediction" = pred)
      res = self$measure$calculate(e)
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun_gensa = function(pred) {
      #
      requireNamespace("GenSA")
      pv = self$param_set$values
      ctrl = pv[which(!(names(pv) %in% c("measure", "algorithm")))]
      or = GenSA::GenSA(fn = private$objfun, pred = pred, control = ctrl,
        lower = rep(min(pred$truth), nlevels(pred$truth) - 1), upper = rep(max(pred$truth), nlevels(pred$truth) - 1))
      th = or$par
      return(th)
    },
    set_ranks_ordinal = function(response, threshold) {
      # if (any(diff(threshold, lag = 1) <= 0))
      #   threshold = sort(threshold)
      t = c(-Inf, threshold, Inf)
      as.numeric(cut(response, breaks = t))
    },
    # FIXME This is ugly, but currently the best way
    # make_prediction_ordinalregr = function(input) {
      # p = PredictionRegr$new()
      # p$response = input$data(cols = input$target_names)
      # p$truth = input$truth()
      # p$predict_types = "response"
      # p$row_ids = input$row_ids
      # return(p)
    # },
    make_prediction_ordinal = function(inputs, threshold = NULL) {
      p = PredictionOrdinal$new(
        task = inputs[[2]],
        response = inputs[[1L]]$data(cols = inputs[[1L]]$feature_names)[[1]]
        )
      # p$response = private$set_ranks_ordinal(response, threshold)
      # p$truth = inputs[[2L]]$truth()
      # p$predict_types = "response"
      # p$row_ids = inputs[[2L]]$row_ids
      return(p)
    })
)
