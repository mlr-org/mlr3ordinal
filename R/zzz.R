#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @import ordinal
#' @import mlr3pipelines
#' @import nloptr
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function() {
  # let mlr3 know about ordinal
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,     ~package,      ~task,         ~learner,         ~prediction,         ~measure,
    "ordinal", "mlr3ordinal", "TaskOrdinal", "LearnerOrdinal", "PredictionOrdinal", "MeasureOrdinal"
  )), "type")
  x$task_col_roles$ordinal = c("feature", "target", "order", "groups", "weights")
  x$learner_properties$ordinal = x$learner_properties$classif
  x$task_properties$ordinal = c("weights")
  x$learner_properties$ordinal = c("missings", "weights", "parallel", "importance") # FIXME for ordinal
  x$learner_predict_types$ordinal = list(response = "response", prob = c("response", "prob"))
  x$task_col_roles$regr = union(x$task_col_roles$regr, "target_ordinal")
  x$measure_properties$ordinal = x$measure_properties$regr
  x$default_measures$ordinal = "ordinal.ce"

  # tasks
  x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("winerating", load_task_winerating)

  # learners
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("ordinal.clm", LearnerOrdinalClm)

  # measures
  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("ordinal.ce", MeasureOrdinalCE)
  x$add("ordinal.acc", MeasureOrdinalACC)
  x$add("ordinal.mae", MeasureOrdinalMAE)
}

register_mlr3pipelines = function() {
  # pipeops
  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("ordinalregr", PipeOpOrdinalRegr)
  x$add("ordinalclassif", PipeOpOrdinalClassif)
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
