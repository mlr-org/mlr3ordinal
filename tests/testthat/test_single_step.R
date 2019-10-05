context("single-step")

test_that("single-step", {
  task = tsk("winerating")
  learner = lrn("ordinal.clm")

  set.seed(123)
  train_ids = sort(sample(1:72, 50))
  test_ids = setdiff(1:72, train_ids)

  learner$train(task, row_ids = train_ids)
  p = learner$predict(task, row_ids = test_ids)

  p = as.data.table(p)
  expect_data_table(p, nrow = length(test_ids))
  expect_set_equal(p$row_id, test_ids)
})
