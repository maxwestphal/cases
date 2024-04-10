test_that("draw_data_lfc: general tests", {
  data <- draw_data_lfc()
  expect_equal(length(data), 2)
  
  data <- draw_data_lfc(random=TRUE)
  expect_equal(length(data), 2)
})
