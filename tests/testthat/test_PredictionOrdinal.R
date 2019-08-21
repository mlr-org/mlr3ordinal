context("PredictionOrdinal")

test_that("Construction", {
  task = mlr_tasks$get("winerating")
  p = PredictionOrdinal$new(row_ids = task$row_ids, truth = task$truth(), response = sample(task$truth()))
  expect_prediction_ordinal(p)
})

test_that("Internally constructed Prediction", {
  task = mlr_tasks$get("winerating")
  lrn = mlr_learners$get("ordinal.clm")
  p = lrn$train(task)$predict(task)
  expect_prediction_ordinal(p)
})

test_that("c", {
  task = mlr_tasks$get("winerating")
  lrn = mlr_learners$get("ordinal.clm")
  rsmp = rsmp("cv", folds = 3)
  rr = resample(task, lrn, rsmp)

  preds = rr$predictions

  pred = do.call(c, preds)
  expect_prediction_ordinal(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 3L, any.missing = FALSE)
})
