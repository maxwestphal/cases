test_that("draw_data: general tests", {
  data <- draw_data()
  expect_equal(length(data), 2)
})
