#' @title PipeOpOrdinalRegr
#'
#' @format [R6Class] PipeOpOrdinalRegr
#'
#' @name mlr_pipeop_ordinalregr
#' @format [`R6::R6Class`] inheriting from [`mlr3pipelines::PipeOpTaskPreproc`].
#'
#' @description
#' This PipeOp works for any regression learner.
#' The idea is to consider the whole task as a [`Regression Task`][mlr3::TaskRegr].
#' Hence, a numeric response vector is predicted for which crossvalidated rank thresholds are found using (nloptr)[nloptr::nloptr] to rank the predictions.
#' Returns a single [`PredictionOrdinal`].
#' As a default, optimizes [`MeasureOrdinalCE`].
#' Used for regression [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' library(mlr3pipelines)
#' op = po("ordinalregr", 2)
#' @export
PipeOpOrdinalRegr = R6Class("PipeOpOrdinalRegr",
  inherit = PipeOp,

  public = list(
    measure = NULL,
    threshold = NULL,
    initialize = function(innum, id = "ordinalregr", param_vals = list()) {
      assert_int(innum, lower = 1)
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL, tags = "train"),
        ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
          levels = c("NLOPT_LN_COBYLA"), tags = "train"),
        ParamDbl$new("xtol_rel", default = 1.0e-8, lower = 0L, tags = "train"),
        ParamInt$new("maxeval", default = 2000L, lower = 1L, upper = Inf, tags = "train")
      ))
      ps$values = list(measure = NULL, algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1.0e-8, maxeval = 5000L)
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
    objfun = function(x, pred) {
      pred$set_threshold(x, levels(pred$truth))
      res = pred$score()
      if (!self$measure$minimize) res = -res
      res
    },

    optimize_objfun = function(pred) {
      requireNamespace("nloptr")
      pv = self$param_set$values
      opts = pv[which(!(names(pv) %in% c("measure")))]
      ranks = levels(pred$truth)
      nranks = length(ranks)
      ordered_ranks = factor(ranks, ordered = TRUE)
      start = c(as.numeric(ordered_ranks)[- nranks] + 0.5)
      constr_fun = function(x, pred) {
        - diff(x)
      }
      or = nloptr::nloptr(
        x0 = start, eval_f = private$objfun, pred = pred, eval_g_ineq = constr_fun,
        lb = as.numeric(rep(1, nranks - 1)),
        ub = as.numeric(rep(nranks, nranks - 1)),
        opts = opts
      )
      th = or$solution
      return(th)
    },

    set_ranks_ordinal = function(response, threshold) {
      # if (any(diff(threshold, lag = 1) <= 0))
      #   threshold = sort(threshold)
      t = c(-Inf, threshold, Inf)
      as.numeric(cut(response, breaks = t))
    },

    make_prediction_ordinal = function(tasks, threshold = NULL) {
      p = PredictionOrdinal$new(
        row_ids = tasks[[2]]$row_ids,
        truth = tasks[[2]]$truth(),
        response = tasks[[1]]$data(cols = tasks[[1]]$feature_names)[[1]]
      )
      return(p)
    }
  )
)
