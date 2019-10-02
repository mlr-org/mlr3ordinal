context("resampling")

test_that("resampling works for ordinal learners", {
  rr = resample(tsk("winerating"), lrn("ordinal.clm"), rsmp("cv"))
  expect_resample_result(rr)
})

test_that("resampling works for regression learners", {
  learner = lrn("regr.rpart")
  graph = PipelineOrdinalThreshold$new(learner)
  glearner = GraphLearner$new(graph, task_type = "ordinal")
  rr = resample(tsk("winerating"), glearner, rsmp("cv"))
  expect_resample_result(rr)
})

# test_that("resampling works for classification learners", {
#   task = mlr_tasks$get("winerating")
#   learner = mlr_learners$get("classif.rpart")
#   graph = PipelineOrdinalThreshold$new(learner)
#   glearner = GraphLearner$new(graph, task_type = "ordinal")
#   resampling = mlr_resamplings$get("cv")
#   rr = resample(task, glearner, resampling)
#   expect_resample_result(rr)
# })
