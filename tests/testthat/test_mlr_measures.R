context("mlr_measures")

test_that("mlr_measures", {
  task = mlr_tasks$get("winerating")
  keys = mlr_measures$keys("^ordinal")

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)

    perf = mlr_learners$get("ordinal.clm")$train(task)$predict(task)$score()
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})
