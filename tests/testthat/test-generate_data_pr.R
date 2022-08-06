test_that("draw_data_prb: general tests", {
  data <- draw_data_prb()
  expect_type(data, "double")
})
