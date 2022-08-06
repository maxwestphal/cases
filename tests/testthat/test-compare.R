test_that("compare", {
  pred <- matrix(c(1,1,0), 5, 3)
  colnames(pred) <- paste0("rule", 1:3)
  labels <- c(1, 1, 0, 0, 1)

  ## expected results
  e1 <- data.frame(rule1 = c(1, 1, 1, 0, 1),
                   rule2 = c(0, 1, 0, 1, 1),
                   rule3 = c(1, 0, 0, 0, 0))
  e2 <- split(e1, labels)
  names(e2) <- c("specificity", "sensitivity")
  
  ## partition = FALSE 
  c1 <- compare(pred, labels, FALSE)
  expect_s3_class(c1, "data.frame")
  expect_equal(dim(c1), c(5, 3))
  expect_equal(c1, e1)
  
  ## partition = TRUE 
  c2 <- compare(pred, labels, TRUE)
  expect_type(c2, "list")  
  expect_s3_class(c2[[1]], "data.frame")
  expect_equal(length(c2), 2)
  expect_equal(unname(sapply(c2, nrow)), c(2,3))
  expect_equal(unname(sapply(c2, ncol)), c(3,3))
  expect_equal(c2, e2)
})
