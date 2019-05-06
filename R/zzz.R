#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @import ordinal
#' @import mlr3pipelines
#' @import GenSA
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # let mlr3 know about ordinal
  mlr_reflections$task_types = union(mlr_reflections$task_types, c("ordinal"))
  mlr_reflections$task_col_roles$ordinal = c("feature", "target", "order", "groups", "weights")
  mlr_reflections$learner_properties$ordinal = c("missings", "weights", "parallel", "importance") # FIXME for ordinal
  mlr_reflections$learner_predict_types$ordinal = c("response", "prob")
  mlr_reflections$task_col_roles$regr = union(mlr_reflections$task_col_roles$regr, "target_ordinal")

  # tasks
  mlr_tasks$add("wine", load_wine)

  # learners
  mlr_learners$add("ordinal.clm", LearnerOrdinalClm)
  mlr_learners$add("ordinal.rpart", LearnerOrdinalRpart)


  # measures
  mlr_measures$add("ordinal.ce", MeasureOrdinalCE)
  mlr_measures$add("ordinal.acc", MeasureOrdinalACC)

  # pipeops
  mlr_pipeops$add("PipeOpOrdinalThresholds", PipeOpOrdinalThresholds)
  mlr_pipeops$add("convertordinaltask", PipeOpConvertOrdinalTask)
}
