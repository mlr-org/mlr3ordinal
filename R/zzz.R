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

register_mlr3 = function() {
  # let mlr3 know about ordinal
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = union(x$task_types, "ordinal")
  x$task_col_roles$ordinal = c("feature", "target", "order", "groups", "weights")
  x$learner_properties$ordinal = c("missings", "weights", "parallel", "importance") # FIXME for ordinal
  x$learner_predict_types$ordinal = c("response", "prob")

  # tasks
  x = getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("wine", load_wine)

  # learners
  x = getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("ordinal.clm", LearnerOrdinalClm)
  x$add("ordinal.rpart", LearnerOrdinalRpart)

  # measures
  x = getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("ordinal.ce", MeasureOrdinalCE)
  x$add("ordinal.acc", MeasureOrdinalACC)
}

register_mlr3 = function() {
  # pipeops
  x = getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("PipeOpOrdinalThresholds", PipeOpOrdinalThresholds)
  x$add("convertordinaltask", PipeOpConvertOrdinalTask)
}

.onLoad = function(libname, pkgname) { # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  register_mlr3pipelines()
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(), action = "append")
} # nocov end

.onUnload = function(libpath) { # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3ordinal"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3ordinal"], action = "replace")
} # nocov end
