#' @title PipelineOrdinalThreshold
#'
#' @name PipelineOrdinalThreshold
#' @format [`R6Class`] object inheriting from [`mlr3pipelines::Graph`].
#'
#' @description
#' A pipeline that that takes any [`Regression Learner`][mlr3::LearnerRegr] to be used for [`TaskOrdinal`][TaskOrdinal].
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
            PipeOpConvertOrdinalTask$new(type = learner$task_type) %>>%
              PipeOpLearnerCV$new(learner),  # convert task and crossvalidated predictions
            PipeOpNOP$new()
          ) # nichts passiert in branch 2
        ) %>>%
        PipeOpOrdinalThresholds$new(2)  # thresholding on cv predictions
      self$pipeops = pipeline$pipeops
      self$edges = pipeline$edges
      self$keep_results = pipeline$keep_results
    }
  )
)
