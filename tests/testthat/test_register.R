context("populate dictionaries")

test_that("re-populate dictionaries", {
  rm("ordinal.clm", envir = mlr_learners$items)
  expect_disjunct("ordinal.clm", mlr_learners$keys())
  register_mlr3()
  expect_subset("ordinal.clm", mlr_learners$keys())
})
