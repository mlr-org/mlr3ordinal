context("OrdinalPipeline")

test_that("regression learner", {
  task = tsk("winerating")
  learner = lrn("regr.rpart")
  graph = ppl("ordinal", learner)

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
  learner = lrn("classif.rpart")
  graph = ppl("ordinal", learner)
  graph$keep_results = TRUE

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(graph, task_type = "ordinal")
  glrn$train(task)

  expect_prediction_ordinal({
    graphpred = glrn$predict(task)
  })

  # test on subset with less levels
  expect_prediction_ordinal({
    graphpred = glrn$predict(task$clone()$filter(rows = c(7:9, 11)))
  })
})
