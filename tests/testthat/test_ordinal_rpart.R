context("mlr_learners_ordinal_rpart")

test_that("autotest", {
  learner = mlr_learners$get("ordinal.rpart")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
