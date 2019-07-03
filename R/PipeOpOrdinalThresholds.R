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
#' Ordinal thresholds for each regression learner are optimized using (nloptr)[nloptr::nloptr].
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
        ParamFct$new("algorithm", default = "nloptr", levels = c("nloptr"))
      ))
      ps$values = list(measure = NULL, algorithm = "nloptr")
      super$initialize(id, param_vals = param_vals, param_set = ps, packages = "nloptr",
        input = data.table(name = mlr3pipelines:::rep_suffix("input", innum), train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },
    train = function(inputs) {
      pred = private$make_prediction_ordinal(inputs)
      assert_class(pred, "PredictionOrdinal")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure))
        self$measure = mlr_measures$get("ordinal.ce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "ordinal")
      th = private$optimize_objfun(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    predict = function(inputs) {
      pred = private$make_prediction_ordinal(inputs)
      pred$set_threshold(self$state$threshold, inputs[[2]]$rank_names)
      return(list(pred))
    }),
  private = list(
    objfun = function(x0, pred) {
      pred$set_threshold(x0, levels(pred$truth))
      res = pred$score()
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun = function(pred) {
      requireNamespace("nloptr")
      pv = self$param_set$values
      # ctrl = pv[which(!(names(pv) %in% c("measure", "algorithm")))]
      ranks = levels(pred$truth)
      nranks = length(ranks)
      start = c(as.numeric(ranks)[- nranks] + 0.5)
      constr_fun = function(t) {
        t[-1] - diff(t)
      }
      opts = list("algorithm"="NLOPT_LN_COBYLA",
             "xtol_rel"=1.0e-8, "maxeval"= 2000)
      or = nloptr::nloptr(
        x0 = start, eval_f = private$objfun, eval_g_ineq = constr_fun,
        lb = as.numeric(rep(min(pred$truth), nlevels(pred$truth) - 1)),
        ub = as.numeric(rep(max(pred$truth), nlevels(pred$truth) - 1)),
        opts = opts, pred = pred
      )
      th = or$par
      browser()
      return(th)
    },
    set_ranks_ordinal = function(response, threshold) {
      # if (any(diff(threshold, lag = 1) <= 0))
      #   threshold = sort(threshold)
      t = c(-Inf, threshold, Inf)
      as.numeric(cut(response, breaks = t))
    },
    make_prediction_ordinal = function(tasks, threshold = NULL) {
      # browser()
      p = PredictionOrdinal$new(
        row_ids = tasks[[2]]$row_ids,
        truth = tasks[[2]]$truth(),
        response = tasks[[1]]$data(cols = tasks[[1]]$feature_names)[[1]]
      )
      return(p)
    })
)
