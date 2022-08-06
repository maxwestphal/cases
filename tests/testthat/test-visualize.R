test_that("visualize: general tests", {
  set.seed(123)
  data <- draw_data_roc(n=400,
                        prev=c(0.25, 0.75),
                        m =4,
                        auc=c(0.9, 0.95),
                        e = 10,
                        modnames = LETTERS[1:4])

  expect_s3_class(visualize(evaluate(data)), "ggplot")
})


