context("single-step")

test_that("single-step", {
  task = tsk("winerating")
  learner = mlr_learners$get("ordinal.clm")

  set.seed(1)
  train_ids = sample(1:72, 50)
  test_ids = setdiff(1:72, train_ids)
  train_task = task$clone()$filter(train_ids)
  learner$train(train_task)
  newdata = remove_named(task$clone()$filter(test_ids)$data(), task$target_names)
  p = learner$predict_newdata(task = train_task, newdata = newdata)

  p = as.data.table(p)
  expect_data_table(p, nrow = 22)
  expect_true(allMissing(p$truth))
  # expect_set_equal(p$row_id, test_ids)
})
