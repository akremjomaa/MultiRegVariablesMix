test_that("GradientDescentOptimizer initialization works", {
  optimizer <- GradientDescentOptimizer$new(learning_rate = 0.01)

  expect_equal(optimizer$learning_rate, 0.01)
})

test_that("GradientDescentOptimizer update works", {
  optimizer <- GradientDescentOptimizer$new(learning_rate = 0.01)

  W <- matrix(0.5, nrow = 3, ncol = 3)
  gradient <- matrix(0.1, nrow = 3, ncol = 3)

  updated_W <- optimizer$update(W, gradient)

  expect_false(identical(W, updated_W))
})
