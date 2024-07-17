test_that("draw_data_roc: general tests", {
  data <- draw_data_roc()
  expect_equal(length(data), 2)

  data <- draw_data_roc(dist = "exponential")
  expect_equal(length(data), 2)
})
