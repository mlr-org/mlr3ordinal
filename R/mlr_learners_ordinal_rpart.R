#' @title Ordinal Tree Learner
#'
#' @name mlr_learners_ordinal.rpart
#' @format [R6::R6Class] inheriting from [LearnerOrdinal].
#' @description
#' A learner for a regression tree implemented in [rpart::rpart()].
#' Thresholds on predictions are optimized by [GenSA::GenSA()].
#' @include LearnerOrdinal.R
#' @export
LearnerOrdinalRpart = R6Class("LearnerOrdinalRpart", inherit = LearnerOrdinal,
  public = list(
    initialize = function(id = "ordinal.rpart", param_vals = list(), predict_type = "response") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_type = predict_type,
        predict_types = "response",
        packages = "rpart",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
            ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = "train"),
            ParamInt$new(id = "threshold_resample_folds", default = 5, lower = 1L, upper = 10L, tags = c("train")),
            ParamInt$new(id = "threshold_resample_reps", default = 5, lower = 1L, upper = 10L, tags = c("train"))
          )
        ),
        param_vals = param_vals,
        properties = "missings"
      )
    },

    train = function(task) {
      pars = self$params("train")

      # extra resampling loop for threshold optimization
      threshold_lrn = mlr_learners$get("regr.rpart")
      threshold_lrn$param_set$values = pars
      self$threshold = optimize_ordinal_threshold(threshold_lrn, task)

      d = task$data()
      d[[task$target_names]] = as.integer(d[[task$target_names]])
      self$model = invoke(rpart::rpart, formula = task$formula, data = d, .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data()
      response = predict(self$model, newdata = newdata)
      response = set_ranks_ordinal(response, learner$threshold)
      PredictionOrdinal$new(task, response = response)
    }
  )
)
