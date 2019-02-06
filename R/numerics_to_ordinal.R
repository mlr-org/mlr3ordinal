#' @title Optimization loop for obtaining optimal ordinal regression threshold
optimize_ordinal_threshold = function(learner, task) {
  pars = learner$params("train")

  if (is.null(pars$threshold_resample_folds))
    pars$threshold_resample_folds = 5L
  if (is.null(pars$threshold_resample_reps))
    pars$threshold_resample_reps = 5L

  rt = mlr_resamplings$get("repeated_cv")
  rt$param_vals = list(
    repeats = pars$threshold_resample_reps,
    folds = pars$threshold_resample_folds
  )

  task_regr = convert_ordinal_task_to_regression(task)
  rr = resample(task_regr, learner, rt)
  rr
}

#' @title Transformation of numeric response to ranks
#'
#' @name RegrToOrdinal.R
#' @description
#' A funtion which uses predictions of any regression learner und tunes the threshold to get ordinal ranks.
#' Thresholding is done by [GenSA::GenSA()].
#' @export
numerics_to_ranks = function(response, task) {
  ranks = task$all_ranks
  k = length(ranks)

  fitn = function(x) {
    x = order(x)
    names(x) = ranks
    score_ordinal(set_ranks_ordinal(response, x), task)
  }

  requirePackages("GenSA", why = "numerics_to_ranks", default.method = "load")
  start = seq(1.5, (k-0.5))

  ctrl = list(smooth = FALSE, simple.function = TRUE, max.call = 3000L, temperature = 250,
    visiting.param = 2.5, acceptance.param = -15)
  or = GenSA::GenSA(par = start, fn = fitn, lower = rep(0, k),
    upper = rep(1, k), control = ctrl)
  th = or$par / sum(or$par)
  names(th) = cls
  perf = or$value
  return(list(th = th, perf = perf))
}

set_ranks_ordinal = function(response, threshold) {
  if (any(diff(threshold, lag = 1) <= 0))
    stop("Threshold needs to be an ascending vector!")
  t = c(-Inf, threshold, Inf)
  as.numeric(cut(response, breaks = t))
}

score_ordinal = function(task, prediction, truth) {
  measures = task$measures
  pkgs = unique(unlist(map(measures, "packages")))
  e = list(prediction = prediction, truth = truth)
  # call m$score with local encapsulation
  score = function() { set_names(lapply(measures, function(m) m$calculate(e)), ids(measures)) }
  enc = mlr3:::encapsulate("none")
  res = enc(score, list(), pkgs, seed = e$seeds[["score"]])
  return(res$result)
}


convert_ordinal_task_to_regression = function(task) {
  d = task$data()
  d[[task$target_names]] = as.integer(d[[task$target_names]])

  TaskRegr$new(id = "threshold_task", backend = as_data_backend(data), target = task$target_names)
}