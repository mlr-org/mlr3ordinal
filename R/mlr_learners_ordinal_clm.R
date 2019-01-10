#' @title Cumulative Link Model Learner
#'
#' @name mlr_learners_ordinal.clm
#' @format [R6::R6Class] inheriting from [LearnerOrdinal].
#' @description
#' A learner for Cumulative Link Models implemented in [ordinal::clm].
#' @include LearnerOrdinal.R
#' @export
LearnerOrdinalClm = R6Class("LearnerOrdinalClm", inherit = LearnerOrdinal,
  public = list(
    initialize = function(id = "ordinal.clm") {
      super$initialize(
        id = id,
        packages = "ordinal",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "link", default = "logit",
              values = c("logit", "probit", "cloglog", "loglog", "cauchit"), tags = "train"),
            ParamFct$new(id = "threshold", default = "flexible",
              values = c("flexible", "symmetric", "symmetric2", "equidistant"), tags = "train")
          )
        ),
        properties = c("weights")
      )
    },

    train = function(task) {
      pars = self$params_train
      self$model = invoke(ordinal::clm,
        formula = task$formula,
        data = task$data(),
        .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      response = prob = NULL

      if (self$predict_type == "response") {
        r = predict(self$model, newdata = newdata, type = "class")
        response = as.character(r$fit)
      } else if (self$predict_type == "prob") {
        p = predict(self$model, newdata = newdata, type = "prob")
        prob = p$fit
      }

      PredictionOrdinal$new(task, response, prob)
    }
  )
)
