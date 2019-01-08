context("resampling")

test_that("resampling works", {
  task = mlr_tasks$get("wine")
  learner = mlr_learners$get("ordinal.clm")
  rr = resample(task, learner, mlr_resamplings$get("cv"))
  expect_resample_result(rr)
})
