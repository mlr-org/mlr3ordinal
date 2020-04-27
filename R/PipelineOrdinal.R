#' @title Ordinal Regression Pipeline
#'
#' @description
#' Creates a graph of an ordinal regression pipeline for a regression or classification learner
#'
#' @param learner :: [`mlr3::LearnerClassif`] or [`mlr3::LearnerRegr`].
#'
#' @return [`Graph`][`mlr3pipelines::Graph`].
#' The resulting graph can used as [`GraphLearner`][`mlr3pipelines::GraphLearner`]
#'
#' @export
#'
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#' task = tsk("winerating")
#' learner = lrn("regr.rpart")
#' graph = PipelineOrdinal(learner)
#' glearner = GraphLearner$new(graph, task_type = "ordinal")
#' resampling = rsmp("cv")
#'
#' # explicitly instantiate the resampling for this task for reproduciblity
#' set.seed(123)
#' resampling$instantiate(task)
#'
#' rr = resample(task, glearner, resampling)
#' print(rr)
#'
#' # retrieve performance
#' rr$aggregate(msr("ordinal.ce"))
PipelineOrdinal = function(learner) {
  assert_learner(learner)
  if (learner$task_type == "regr") {
    pipeline = po("copy", 2) %>>%
      gunion(
        graphs = list(
          po("update_target",
            param_vals = list(
              trafo = function(x) {map_dtc(x, as.numeric)},
              new_task_type = learner$task_type,
              new_target_name = "target_regr"
              )
            ) %>>% po("learner_cv", learner),  # convert task and crossvalidated predictions
          po("nop")
        ) # nichts passiert in branch 2
      ) %>>%
      po("ordinalregr", 2) # thresholding on cv predictions
  } else if (learner$task_type == "classif") {
    pipeline =  po("update_target",
        param_vals = list(
          trafo = function(x) {map_dtc(x, function(x) {factor(x , ordered = FALSE, levels = levels(x))})},
          new_task_type = learner$task_type,
          new_target_name = "target_classif"
        )
      ) %>>% po("learner", learner) %>>%
      po("ordinalclassif")
  }

  graph = Graph$new()
  graph$pipeops = pipeline$pipeops
  graph$edges = pipeline$edges
  graph$keep_results = pipeline$keep_results

  return(graph)
}
