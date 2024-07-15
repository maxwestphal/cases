test_that("define_contrast tests", {
  ## sample data
  set.seed(123)
  data <- draw_data_roc(
    n = 400,
    prev = c(0.25, 0.75),
    m = 4,
    auc = c(0.9, 0.95),
    e = 10,
    modnames = LETTERS[1:4]
  )

  print(define_contrast("one", 1))

  results_raw <- evaluate(data, contrast = define_contrast("raw"))
  expect_s3_class(results_raw, "cases_results")
  expect_equal(nrow(results_raw[[1]]), 4)

  results_one <- evaluate(data, contrast = define_contrast("one", 1), benchmark = 0)
  expect_s3_class(results_one, "cases_results")
  expect_equal(nrow(results_one[[1]]), 3)

  results_all <- evaluate(data, contrast = define_contrast("all"), benchmark = 0)
  expect_s3_class(results_all, "cases_results")
  expect_equal(nrow(results_all[[1]]), 6)
})
