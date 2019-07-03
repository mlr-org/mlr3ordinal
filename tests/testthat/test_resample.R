context("resampling")

test_that("resampling works", {
  task = mlr_tasks$get("winerating")
  learner = mlr_learners$get("ordinal.clm")
  resampling = mlr_resamplings$get("cv")
  rr = resample(task, learner, resampling)
  expect_resample_result(rr)
})
