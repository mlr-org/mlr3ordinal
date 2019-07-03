context("TaskOrdinal")

test_that("Task duplicates rows", {
  task = mlr_tasks$get("winerating")
  expect_task(task)
  expect_task_supervised(task)

  expect_is(task$truth(), "ordered")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
  expect_set_equal(extract_vars(f)$rhs, ".")
})
