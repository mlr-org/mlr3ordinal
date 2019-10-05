context("benchmark")

test_that("benchmark works", {
  clrn = GraphLearner$new(PipelineOrdinal(lrn("classif.rpart")), task_type = "ordinal")
  rlrn = GraphLearner$new(PipelineOrdinal(lrn("regr.rpart")), task_type = "ordinal")
  learners = list(lrn("ordinal.clm"), clrn, rlrn)
  task = tsk("winerating")
  bmr = benchmark(benchmark_grid(task, learners, rsmp("cv", folds = 3)))
  expect_benchmark_result(bmr)
})
