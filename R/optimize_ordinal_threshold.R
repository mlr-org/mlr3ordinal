# @title Optimization loop for obtaining optimal ordinal regression threshold
#
# @name optimize_ordinal_threshold.R
# @description
# A funtion which uses predictions of any regression learner und tunes the threshold to get ordinal ranks.
# Thresholding is done by [GenSA::GenSA()].
#' @export
optimize_ordinal_threshold = function(learner, task, threshold_resample_folds = 5L, threshold_resample_reps = 5L) {

  pars = learner$params("train")

  folds = threshold_resample_folds
  reps = threshold_resample_reps

  rdesc = mlr_resamplings$get("repeated_cv")
  rdesc$param_set$values = list(
    folds = folds,
    repeats = reps
  )

  task_regr = convert_ordinal_task_to_regression(task)
  rr = resample(task_regr, learner, rdesc, ctrl = list(store_prediction = TRUE))
  t = list()
  for (i in 1:reps) { #FIXME: lapply
    e = rr$experiments(((i-1)*folds + 1):(folds*i))
    response = unname(unlist(lapply(e, function(x) {x$prediction$response})))
    truth = unlist(lapply(e, function(x) {x$prediction$truth}))
    t[[i]] = optimize_ordinal_threshold_iteration(response, truth, task)
  }
  t = matrix(unlist(t), nrow = reps, byrow = TRUE)
  return(apply(t, 2, mean))
}


convert_ordinal_task_to_regression = function(task) {
  d = task$data()
  d[[task$target_names]] = as.integer(d[[task$target_names]])

  TaskRegr$new(id = "threshold_task", backend = as_data_backend(d), target = task$target_names)
}

optimize_ordinal_threshold_iteration = function(response, truth, task) {
  ranks = task$all_ranks
  k = length(ranks)

  fitn = function(x) {
    x = sort(x)
    return(score_ordinal(task, set_ranks_ordinal(response, x), truth)[[1]])
  }

  require_namespaces("GenSA")
  start = seq(1.5, (k-0.5))

  ctrl = list(smooth = FALSE, simple.function = TRUE, max.call = 3000L, temperature = 250,
    visiting.param = 2.5, acceptance.param = -15)
  lower = rep(min(response), k - 1)
  upper = rep(max(response), k - 1)
  or = GenSA::GenSA(par = start, fn = fitn, lower = lower,
    upper = upper, control = ctrl)
  th = sort(or$par)
  # perf = or$value
  return(th)
}

score_ordinal = function(task, prediction, truth) {
  measures = task$measures
  pkgs = unique(unlist(map(measures, "packages")))
  e = list(
    prediction = list(response = prediction, truth = truth),
    levels = task$all_ranks
  )
  # call m$score with local encapsulation
  score = function() { set_names(lapply(measures, function(m) m$calculate(e)), mlr3:::ids(measures)) }
  enc = mlr3:::encapsulate("none")
  res = enc(score, list(), pkgs)#, seed = e$seeds[["score"]]
  return(res$result)
}

set_ranks_ordinal = function(response, threshold) {
  if (any(diff(threshold, lag = 1) <= 0))
    threshold = sort(threshold)
  t = c(-Inf, threshold, Inf)
  as.numeric(cut(response, breaks = t))
}




