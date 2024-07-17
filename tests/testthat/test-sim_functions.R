test_that("simulation functions", {
  set.seed(123)
  results_lfc <-
    generate_instance_lfc() %>%
    process_instance()

  expect_s3_class(results_lfc, "data.frame")
  expect_equal(dim(results_lfc), c(10, 39))

  set.seed(123)
  results_roc <-
    generate_instance_roc() %>%
    process_instance()

  expect_s3_class(results_roc, "data.frame")
  expect_equal(dim(results_roc), c(10, 39))
})
