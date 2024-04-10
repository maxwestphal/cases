test_that("categorize() works", {
  set.seed(123)
  M <- as.data.frame(mvtnorm::rmvnorm(20, mean=rep(0, 3), sigma=2*diag(3)))
  C <- categorize(M, labels=letters[1:3])
  expect_s3_class(C, "data.frame")
  expect_equal(dim(C), c(20, 3))
})
