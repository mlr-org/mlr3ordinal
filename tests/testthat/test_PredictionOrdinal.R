test_that("Construction", {
  task = mlr_tasks$get("wine")
  p = PredictionOrdinal$new(row_ids = task$row_ids,
  truth = task$truth(), response = sample(task$truth()))
  expect_prediction_ordinal(p)
})