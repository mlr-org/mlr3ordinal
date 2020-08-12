#' @title PipeOpUpdateTarget
#' @usage NULL
#' @name mlr_pipeops_updatetarget
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`][`mlr3pipelines::PipeOp`].
#'
#' @description
#' FOR INTERNAL USE IN MLR3ORDINAL ONLY!
#' EXPERIMENTAL, API SUBJECT TO CHANGE
#'
#' Handles target transformation operations that do not need explicit inversion.
#' In case the new target is required during predict, creates a vector of `NA`.
#' Works similar to [`PipeOpTargetTrafo`][`mlr3pipelines::PipeOpTargetTrafo`] and
#' [`PipeOpTargetMutate`][`mlr3pipelines::PipeOpTargetMutate`], but forgoes the
#' inversion step.
#' In case target after the `trafo` is a factor, levels are saved to `$state`.\cr
#'
#' During prediction: Sets all target values to `NA` before calling the `trafo` again.
#' In case target after the `trafo` is a factor, levels saved in the `state` are
#' set during prediction.
#'
#' As a special case when `trafo` is `identity` and `new_target_name` matches an existing column
#' name of the data of the input [`Task`][mlr3::Task], this column is set as the new target. Depending on
#' `drop_original_target` the original target is then either dropped or added to the features.
#'
#' @section Construction:
#' ```
#' PipeOpUpdateTarget$new(id, param_set = ParamSet$new(),
#'   param_vals = list(), packages = character(0))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`][`mlr3pipelines::PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings given in `param_set`.
#'   The subclass should have its own `param_vals` parameter and pass it on to `super$initialize()`.
#'   Default `list()`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTargetTrafo`][`mlr3pipelines::PipeOpTargetTrafo`], as well as:
#' * `trafo` :: `function`\cr
#'   Transformation function for the target. Should only be a function of the target, i.e., taking a
#'   single argument. Default is `identity`.
#'   Note, that the data passed on to the target is a `data.table` consisting of all target column.
#' * `new_target_name` :: `character(1)`\cr
#'   Optionally give the transformed target a new name. By default the original name is used.
#' * `new_task_type` :: `character(1)`\cr
#'   Optionally a new task type can be set. Legal types are listed in
#'   `mlr_reflections$task_types$type`.
#' #' `drop_original_target` :: `logical(1)`\cr
#'   Whether to drop the original target column. Default: `TRUE`.
#'
#' @section State:
#' The `$state` is a list of class levels for each target after trafo.
#' `list()` if none of the targets have levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`][`mlr3pipelines::PipeOp`].
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @examples
#' \dontrun{
#' # Create a binary class task from iris
#' library(mlr3)
#' trafo_fun = function(x) {factor(ifelse(x$Species == "setosa", "setosa", "other"))}
#' po = PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "setosa"))
#' po$train(list(tsk("iris")))
#' po$predict(list(tsk("iris")))
#' }
#' @export
PipeOpUpdateTarget = R6Class("PipeOpUpdateTarget",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "update_target", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("trafo", default = identity, tags = c("train", "predict"), custom_check = function(x) check_function(x, nargs = 1L)),
        ParamUty$new("new_target_name", tags = c("train", "predict"), custom_check = function(x) check_character(x, any.missing = FALSE, len = 1L)),
        ParamUty$new("new_task_type", tags = c("train", "predict"), custom_check = function(x) check_choice(x, choices = mlr_reflections$task_types$type)),
        ParamLgl$new("drop_original_target", default = TRUE, tags = c("train", "predict"))
        )
      )
      ps$values = list(trafo = identity, drop_original_target = TRUE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      pv = self$param_set$values
      if (identical(pv$trafo, identity) && (pv$new_target_name %in% unlist(intask$col_roles, use.names = FALSE))) {
        self$state = list()
        return(list(private$.update_target(intask, drop_levels = TRUE)))  # early exit
      }
      if (!identical(pv$trafo, identity) || !is.null(pv$new_target_name)) {
        # Apply fun to target, rename, cbind and convert task if required
        new_target = data.table(pv$trafo(intask$data(cols = intask$target_names)))
        if (!is.null(pv$new_target_name)) {
          setnames(new_target, colnames(new_target), pv$new_target_name)
        }
        self$state = keep(map(new_target, levels), Negate(is.null))
        intask$cbind(new_target)
      } else {
        self$state = list()
      }
      list(private$.update_target(intask, drop_levels = TRUE))
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      pv = self$param_set$values
      if (identical(pv$trafo, identity) && (pv$new_target_name %in% unlist(intask$col_roles, use.names = FALSE))) {
        return(list(private$.update_target(intask, drop_levels = FALSE)))  # early exit
      }
      if (!identical(pv$trafo, identity) || !is.null(pv$new_target_name)) {
        new_target = intask$data(cols = intask$target_names)
        if (!pv$drop_original_target)
          # During predict, if original target is not dropped, set the new target to NA and then call the trafo
          new_target = set(new_target, j = intask$target_names, value = NA)
        new_target = data.table(pv$trafo(new_target))
        # Rename, cbind and convert
        setnames(new_target, colnames(new_target), self$param_set$values$new_target_name)
        # Make sure levels match target levels
        if (length(self$state))
          new_target = imap_dtc(new_target, function(x, nms) {
            if(nms %in% names(self$state))
              levels(x) = self$state[[nms]]
            return(x)
        })
        intask$cbind(new_target)
      }
      list(private$.update_target(intask, drop_levels = FALSE))
    },

    # Updates the target of a task and also the task_type (if needed), uses convert_task
    .update_target = function(task, drop_levels) {
      pv = self$param_set$values
      convert_task(task, new_target = pv$new_target_name,
        new_type = pv$new_task_type, drop_original_target = pv$drop_original_target, drop_levels = drop_levels)
    }
  )
)