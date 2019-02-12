context("mlr_learners_ordinal_rpart")

test_that("autotest", {
  learner = mlr_learners$get("ordinal.rpart")
  learner$param_set$values = list(threshold_resample_folds = 2L, threshold_resample_reps = 1L)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
