test_that("AdamOptimizer initialization works", {
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(3, 3))

  expect_equal(optimizer$learning_rate, 0.01)
  expect_equal(optimizer$beta1, 0.9)
  expect_equal(optimizer$beta2, 0.999)
  expect_equal(optimizer$epsilon, 1e-8)
  expect_equal(dim(optimizer$m), c(3, 3))
  expect_equal(dim(optimizer$v), c(3, 3))
})

test_that("AdamOptimizer clone works", {
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(3, 3))
  cloned_optimizer <- optimizer$clone(deep = TRUE)

  expect_false(identical(optimizer, cloned_optimizer))

  expect_equal(optimizer$learning_rate, cloned_optimizer$learning_rate)
  expect_equal(optimizer$beta1, cloned_optimizer$beta1)
})
test_that("AdamOptimizer updates weights correctly", {
  W <- matrix(0, 2, 2)
  gradient <- matrix(0.1, 2, 2)
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(2, 2))

  W_updated <- optimizer$update(W, gradient)

  expect_true(all(W_updated != W)) # Weights should change
  expect_true(all(optimizer$m != 0)) # Momentum term updated
  expect_true(all(optimizer$v != 0)) # Second moment updated
})


