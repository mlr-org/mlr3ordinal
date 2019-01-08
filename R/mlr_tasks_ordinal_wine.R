load_wine = function() {
  d = load_dataset("wine", "ordinal")
  d$rating = ordered(d$rating)
  b = as_data_backend(d)
  TaskOrdinal$new("wine", b, target = "rating")
}
