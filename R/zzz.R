#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @import ordinal
#' @importFrom R6 R6Class
NULL

.onLoad = function(libname, pkgname) {
  # let mlr3 know about ordinal
  mlr_reflections$task_types = union(mlr_reflections$task_types, "ordinal")
  mlr_reflections$task_col_roles$ordinal = c("feature", "target", "order", "groups", "weights")
  mlr_reflections$learner_properties$ordinal = c("missings", "weights", "parallel", "importance") # FIXME for ordinal
  mlr_reflections$predict_types$ordinal = c("response", "prob")

  # tasks
  mlr_tasks$add("wine", load_wine)

  # learners
  mlr_learners$add("ordinal.clm", LearnerOrdinalClm)

  # measures
  mlr_measures$add("ordinal.mmce", MeasureOrdinalMMCE)
}

if (FALSE) {
}
