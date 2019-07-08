#' @title PipelineOrdinalThreshold
#'
#' @name PipelineOrdinalThreshold
#' @format [`R6Class`] object inheriting from [`mlr3pipelines::Graph`].
#'
#' @description
#' A pipeline that that takes any [`Regression Learner`][mlr3::LearnerRegr] to be used for [`TaskOrdianl`][TaskOrdianl].
#'
#' The result is a [`Graph`][mlr3pipelines::Graph] which works as regular learner when put into [`GraphLearner`][mlr3pipelines::GraphLearner].
#' @export
PipelineOrdinalThreshold = R6Class("PipelineOrdinalThreshold",
  inherit = Graph,
  public = list(
    initialize = function(learner) {
      pipeline = PipeOpCopy$new(2) %>>%
        gunion(
          graphs = list(
            PipeOpConvertOrdinalTask$new() %>>% PipeOpLearnerCV$new(learner),  # convertierung und regression
            PipeOpNULL$new()
          ) # nichts passiert in branch 2
        ) %>>%
        PipeOpOrdinalThresholds$new(2)  # thresholded mit dem feature das in PipeOpLearnerCV erstellt wurde, und potentiell auch mit anderen feats
        self$pipeops = pipeline$pipeops
        self$edges = pipeline$edges
        self$keep_results = pipeline$keep_results
    }
  )
)
