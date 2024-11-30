library(testthat)
source(here("R", "RMSpropOptimizer.R"))

test_that("RMSpropOptimizer initialization works", {
  optimizer <- RMSpropOptimizer$new(learning_rate = 0.01, beta1 = 0.9, epsilon = 1e-8, weight_dim = c(3, 3))

  expect_equal(optimizer$learning_rate, 0.01)
  expect_equal(optimizer$beta1, 0.9)
  expect_equal(optimizer$epsilon, 1e-8)
  expect_equal(dim(optimizer$m), c(3, 3))
})

test_that("RMSpropOptimizer update works", {
  optimizer <- RMSpropOptimizer$new(learning_rate = 0.01, beta1 = 0.9, epsilon = 1e-8, weight_dim = c(3, 3))

  W <- matrix(0.5, nrow = 3, ncol = 3)
  gradient <- matrix(0.1, nrow = 3, ncol = 3)

  updated_W <- optimizer$update(W, gradient)

  expect_false(identical(W, updated_W))
})
