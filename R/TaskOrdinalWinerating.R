#' @title Wine Rating Ordinal Task
#'
#' @usage NULL
#' @name mlr_tasks_winerating
#' @format [R6::R6Class] inheriting from [TaskOrdinal].
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("winerating")
#' ```
#'
#' @description
#' An ordinal task for the [ordinal::wine] data set.
load_task_winerating = function(id = "winerating") {
  d = load_dataset("wine", "ordinal")
  d$rating = ordered(d$rating)
  b = as_data_backend(d)
  b$hash = "_mlr3_tasks_winerating_"
  TaskOrdinal$new("winerating", b, target = "rating")
}
