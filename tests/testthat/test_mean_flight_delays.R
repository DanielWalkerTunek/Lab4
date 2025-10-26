
test_that("visualize_airport_delays returns a ggplot object", {
  plot_obj <- visualize_airport_delays()
  expect_s3_class(plot_obj, "ggplot")
})
