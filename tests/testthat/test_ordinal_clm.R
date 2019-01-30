context("ordinal.clm")

test_that("autotest", {
  learner = LearnerOrdinalClm$new()
  result = run_autotest(learner, exclude = "(sanity)")
  expect_true(result, info = result$error)
})
