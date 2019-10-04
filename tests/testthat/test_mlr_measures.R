context("mlr_measures")

test_that("mlr_measures", {
  task = tsk("winerating")
  keys = mlr_measures$keys("^ordinal")

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)

    perf = lrn("ordinal.clm")$train(task)$predict(task)$score(m)
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})
