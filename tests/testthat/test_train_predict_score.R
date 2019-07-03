context("resampling")

test_that("train, predict, score", {
  task = mlr_tasks$get("winerating")
  learner = mlr_learners$get("ordinal.clm")

  perf = learner$train(task)$predict(task)$score()
  expect_number(perf)
})
