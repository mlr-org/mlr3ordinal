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
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
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
            ParamFct$new(id = "resample_train", default = "10CV", values = c("10CV", "5CV", "3CV", "Bootstrap"), tags = c("train")),
            ParamFct$new(id = "resample_threshold", default = "10CV", values = c("10CV", "5CV", "3CV", "Bootstrap"), tags = c("train"))
            # ParamDbl$new(id = "treshold", default = 0, lower = 0, upper = 1, tags = "test")
          )
        ),
        properties = "missings",
        ordinal_threshold = NULL
      )
    },

    train = function(task) {
      pars = self$params("train")
      d = task$data()
      d[[task$target_names]] = as.integer(d[[task$target_names]])
      # resampling loop for threshold determination
      self$model = invoke(rpart::rpart, formula = task$formula, data = d, .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data()
      response = predict(self$model, newdata = newdata)
      PredictionOrdinal$new(task, response = response)
    }
  )
)
