context("mlr_learners_ordinal_clm")

test_that("autotest", {
  learner = mlr_learners$get("ordinal.clm")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
