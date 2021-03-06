context("PredictionOrdinal")

test_that("Construction", {
  task = tsk("winerating")
  p = PredictionOrdinal$new(row_ids = task$row_ids, truth = task$truth(), response = sample(task$truth()))
  expect_prediction_ordinal(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("winerating")
  lrn = lrn("ordinal.clm")
  p = lrn$train(task)$predict(task)
  expect_prediction_ordinal(p)
})

test_that("c", {
  task = tsk("winerating")
  lrn = lrn("ordinal.clm")
  rsmp = rsmp("cv", folds = 3)
  rr = resample(task, lrn, rsmp)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_ordinal(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 3L, any.missing = FALSE)
})

test_that("probs", {
  task = tsk("winerating")
  learner = lrn("regr.rpart")
  graph = ppl("ordinal", learner)
  glearner = GraphLearner$new(graph, task_type = "ordinal")
  glearner$predict_type = "prob"
  })