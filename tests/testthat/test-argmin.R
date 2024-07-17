test_that("argmin/argmax", {
  expect_equal(argmin(c(8, 6, 9, 7)), 2)
  expect_equal(argmax(c(8, 6, 9, 7)), 3)
})

test_that("pargmin/pargmax", {
  expect_equal(pargmin(c(7, 8, 9), c(9, 8, 7)), c(1, 1, 2))
  expect_equal(pargmax(c(7, 8, 9), c(9, 8, 7)), c(2, 1, 1))
})
