context("ordinal.rpart")

test_that("autotest", {
  learner = LearnerOrdinalRpart$new()
  result = run_autotest(learner, exclude = "(sanity)")
  expect_true(result, info = result$error)
})
