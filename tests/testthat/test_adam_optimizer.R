library(testthat)
source(here("R", "AdamOptimizer.R"))

test_that("AdamOptimizer initialization works", {
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(3, 3))

  expect_equal(optimizer$learning_rate, 0.01)
  expect_equal(optimizer$beta1, 0.9)
  expect_equal(optimizer$beta2, 0.999)
  expect_equal(optimizer$epsilon, 1e-8)
  expect_equal(dim(optimizer$m), c(3, 3))  # Vérifie la dimension de 'm'
  expect_equal(dim(optimizer$v), c(3, 3))  # Vérifie la dimension de 'v'
})

test_that("AdamOptimizer update works", {
  # Test de la mise à jour des poids
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(3, 3))

  W <- matrix(0.5, nrow = 3, ncol = 3)  # Poids initiaux
  gradient <- matrix(0.1, nrow = 3, ncol = 3)  # Gradient de test

  # Applique l'update
  updated_W <- optimizer$update(W, gradient)

  # Vérifie que les poids ont changé (ce qui signifie que la mise à jour a fonctionné)
  expect_false(identical(W, updated_W))
})

test_that("AdamOptimizer clone works", {
  # Test de la méthode clone()
  optimizer <- AdamOptimizer$new(learning_rate = 0.01, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim = c(3, 3))
  cloned_optimizer <- optimizer$clone(deep = TRUE)

  # Vérifie que l'objet cloné n'est pas identique à l'original (pas la même adresse mémoire)
  expect_false(identical(optimizer, cloned_optimizer))

  # Vérifie que les paramètres sont identiques
  expect_equal(optimizer$learning_rate, cloned_optimizer$learning_rate)
  expect_equal(optimizer$beta1, cloned_optimizer$beta1)
})

