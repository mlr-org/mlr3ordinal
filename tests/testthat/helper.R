lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

generate_tasks.LearnerOrdinal = function(learner, N = 20L) {

  tasks = list()
  target = ordered(factor(rep_len(head(LETTERS, 3L), N)))
  data = cbind(data.table::data.table(target = target), generate_data(learner, N))
  task = TaskOrdinal$new("proto", mlr3::as_data_backend(data), target = "target")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = ordered(rep(as.factor(c("A", "B")), each = 100)))
    data$unimportant = runif(nrow(data))
  })
  task = mlr3misc::set_names(list(mlr3ordinal::TaskOrdinal$new("sanity", mlr3::as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)

  tasks
}
registerS3method("generate_tasks", "LearnerOrdinal", generate_tasks.LearnerOrdinal)

sanity_check.PredictionOrdinal = function(e) {
  prediction$score() <= 0.3
}
registerS3method("sanity_check", "PredictionOrdinal", sanity_check.PredictionOrdinal)

expect_prediction_ordinal = function(p) {
  expect_prediction(p)
  expect_is(p, "PredictionOrdinal")
}
