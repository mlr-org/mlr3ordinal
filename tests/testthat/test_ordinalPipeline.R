context("OrdinalPipeline")

test_that("regression learner", {
  task = mlr_tasks$get("winerating")

  lrn = mlr_learners$get("regr.rpart")
  graph = PipelineOrdinalThreshold$new(lrn)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  glrn$train(task)

  expect_prediction_ordinal({
    graphpred = glrn$predict(task)
  })
})
