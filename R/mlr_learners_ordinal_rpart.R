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
    initialize = function(id = "ordinal.rpart") {
      super$initialize(
        id = id,
        packages = "rpart",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "response",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = c("train", "threshold")),
            ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = c("train", "threshold")),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = c("train", "threshold")),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = c("train", "threshold")),
            ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = c("train", "threshold")),
            ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = c("train", "threshold")),
            ParamInt$new(id = "threshold_resample_folds", default = 5, lower = 1L, upper = 10L, tags = c("threshold")),
            ParamInt$new(id = "threshold_resample_reps", default = 5, lower = 1L, upper = 10L, tags = c("threshold"))
          )
        ),
        properties = "missings"
      )
    },

    threshold = NULL,

    train = function(task) {

      pars = self$params("threshold")

      # extra resampling loop for threshold optimization
      threshold_lrn = mlr_learners$get("regr.rpart")
      # threshold_lrn$param_set$values = pars
      self$threshold = optimize_ordinal_threshold(threshold_lrn, task,
        ifelse(is.null(pars$threshold_resample_folds), 5L, pars$threshold_resample_folds),
        ifelse(is.null(pars$threshold_resample_reps), 5L, pars$threshold_resample_reps)
      )

      pars = self$params("train")
      d = task$data()
      d[[task$target_names]] = as.integer(d[[task$target_names]])
      self$model = invoke(rpart::rpart, formula = task$formula(), data = d, .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data()
      response = predict(self$model, newdata = newdata)
      response = set_ranks_ordinal(response, self$threshold)
      ranks = task$all_ranks
      lvl = ordered(ranks)
      response = ordered(response, levels = as.integer(lvl), labels = ranks)
      PredictionOrdinal$new(task, response = response)
    }
  )
)
