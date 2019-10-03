context("resampling")

test_that("resampling works for ordinal learners", {
  rr = resample(tsk("winerating"), lrn("ordinal.clm"), rsmp("cv"))
  expect_resample_result(rr)
})

test_that("resampling works for regression learners", {
  learner = lrn("regr.rpart")
  graph = PipelineOrdinal(learner)
  glearner = GraphLearner$new(graph, task_type = "ordinal")
  rr = resample(tsk("winerating"), glearner, rsmp("cv"))
  expect_resample_result(rr)
})

test_that("resampling works for classification learners", {
  learner = lrn("classif.rpart")
  graph = PipelineOrdinal(learner)
  glearner = GraphLearner$new(graph, task_type = "ordinal")
  rr = resample(tsk("winerating"), glearner, rsmp("cv"))
  expect_resample_result(rr)
})
