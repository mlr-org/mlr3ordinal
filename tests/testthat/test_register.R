context("populate dictionaries")

test_that("re-populate learners", {
  rm("ordinal.clm", envir = mlr_learners$items)
  expect_disjunct("ordinal.clm", mlr_learners$keys())
  register_mlr3()
  expect_subset("ordinal.clm", mlr_learners$keys())
})

test_that("re-populate pipelines", {
  rm("ordinalregr", envir = mlr_pipeops$items)
  expect_disjunct("ordinalregr", mlr_pipeops$keys())
  register_mlr3pipelines()
  expect_subset("ordinalregr", mlr_pipeops$keys())
})

test_that("re-populate reflections", {
  mlr_reflections$task_types = mlr_reflections$task_types[!unlist(mlr_reflections$task_types[, 1]) == "ordinal",]
  expect_false("ordinal" %in% unlist(mlr_reflections$task_types[,1]))
  register_mlr3()
  expect_true("ordinal" %in% unlist(mlr_reflections$task_types[,1]))
})
