library(testthat)
source(here("R", "MomentumOptimizer.R"))


test_that("MomentumOptimizer initialization works", {
  optimizer <- MomentumOptimizer$new(learning_rate = 0.01, beta1 = 0.9, weight_dim = c(3, 3))

  expect_equal(optimizer$learning_rate, 0.01)
  expect_equal(optimizer$beta1, 0.9)
  expect_equal(dim(optimizer$m), c(3, 3))
})

test_that("MomentumOptimizer update works", {
  optimizer <- MomentumOptimizer$new(learning_rate = 0.01, beta1 = 0.9, weight_dim = c(3, 3))

  W <- matrix(0.5, nrow = 3, ncol = 3)
  gradient <- matrix(0.1, nrow = 3, ncol = 3)

  updated_W <- optimizer$update(W, gradient)

  expect_false(identical(W, updated_W))
})

test_that("MomentumOptimizer clone works", {
  optimizer <- MomentumOptimizer$new(learning_rate = 0.01, beta1 = 0.9, weight_dim = c(3, 3))
  cloned_optimizer <- optimizer$clone(deep = TRUE)

  expect_false(identical(optimizer, cloned_optimizer))
  expect_equal(optimizer$learning_rate, cloned_optimizer$learning_rate)
})
