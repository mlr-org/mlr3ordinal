context("single-step")

test_that("single-step", {
  task = mlr_tasks$get("winerating")
  learner = mlr_learners$get("ordinal.clm")

  train_task = task$clone()$filter(1:50)
  learner$train(train_task)
  newdata = remove_named(task$clone()$filter(51:72)$data(), task$target_names)
  p = learner$predict_newdata(train_task, newdata = newdata)

  p = as.data.table(p)
  expect_data_table(p, nrow = 22)
  expect_true(allMissing(p$time))
  expect_true(allMissing(p$status))
  expect_set_equal(p$row_id, 51:72)
})
