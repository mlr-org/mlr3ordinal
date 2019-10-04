context("TaskOrdinal")

test_that("Task duplicates rows", {
  task = tsk("winerating")
  expect_task(task)
  expect_task_supervised(task)

  expect_is(task$truth(), "ordered")
  expect_number(task$rank_n)

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
  expect_set_equal(extract_vars(f)$rhs, ".")
})

test_that("Task errors", {
  expect_error(TaskOrdinal$new(id = "iris", backend = iris, target = "Species"), "must be an ordered factor")
  iris$Species = ordered(rep(1, 150))
  expect_error(TaskOrdinal$new(id = "iris", backend = iris, target = "Species"), "must have at least two levels")
})
