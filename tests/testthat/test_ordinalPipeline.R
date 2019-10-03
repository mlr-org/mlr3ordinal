context("OrdinalPipeline")

test_that("regression learner", {
  task = tsk("winerating")
  lrn = lrn("regr.rpart")
  graph = PipelineOrdinal(lrn)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  glrn$train(task)

  expect_prediction_ordinal({
    graphpred = glrn$predict(task)
  })
})

test_that("classification learner", {
  task = tsk("winerating")
  lrn = lrn("classif.rpart")
  graph = PipelineOrdinal(lrn)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  glrn$train(task)

  expect_prediction_ordinal({
    graphpred = glrn$predict(task)
  })
})
