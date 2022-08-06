test_that("cormat works", {
  m <- 5
  rho <- 0.5
  
  R1 <- cormat_equi(m, rho)
  R2 <- cormat_ar1(m, rho)
  
  expect_type(R1, "double")
  expect_type(R2, "double")
  
  expect_equal(dim(R1), c(m,m))
  expect_equal(dim(R2), c(m,m))
  
  expect_equal(diag(R1), rep(1, m))
  expect_equal(diag(R2), rep(1, m))
  
  expect_true(isSymmetric(R1))
  expect_true(isSymmetric(R2))
})
