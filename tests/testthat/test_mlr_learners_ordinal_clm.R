context("mlr_learners_ordinal_clm")

test_that("autotest", {
  learner = mlr_learners$get("ordinal.clm")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("Predict with prob", {
  task = tsk("winerating")
  learner = lrn("ordinal.clm", predict_type = "prob")
  expect_learner(learner, task)

  p = learner$train(task)$predict(task)
  expect_matrix(p$prob, nrows = 72L, ncols = 5L)
  expect_names(colnames(p$prob), permutation.of = levels(task$truth()))
})
