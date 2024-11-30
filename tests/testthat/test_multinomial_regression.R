test_that("MultinomialRegression preprocesses data correctly", {
  data <- data.frame(
    num1 = rnorm(10),
    cat1 = factor(c("A", "B", "A", "C", "B", "A", "C", "B", "A", "B"))
  )
  target <- c(1, 2, 1, 3, 2, 1, 3, 2, 1, 2)
  model <- MultinomialRegression$new(encoding_type = "label")

  model$preprocess(data, target)

  expect_true(model$data_preprocessed) # Check if data is marked as preprocessed
  expect_true(!is.null(model$X)) # Feature matrix created
  expect_equal(ncol(model$X), 2 + 1) # 2 features + bias term
})
test_that("MultinomialRegression fits model without errors", {
  data <- data.frame(
    num1 = rnorm(20),
    num2 = rnorm(20),
    cat1 = factor(sample(c("A", "B", "C"), 20, replace = TRUE))
  )
  target <- sample(1:3, 20, replace = TRUE)
  model <- MultinomialRegression$new(encoding_type = "label", optimizer_type = "adam", epochs = 10)

  expect_no_error(model$fit(data, target)) # Model should train without errors
  expect_true(!is.null(model$W)) # Weights should be initialized
})

test_that("MultinomialRegression predicts correctly", {
  data <- data.frame(
    num1 = rnorm(10),
    num2 = rnorm(10),
    cat1 = factor(sample(c("A", "B", "C"), 10, replace = TRUE))
  )
  target <- sample(1:3, 10, replace = TRUE)
  model <- MultinomialRegression$new(encoding_type = "label", optimizer_type = "adam", epochs = 10)

  model$fit(data, target)
  predictions <- model$predict(data)

  expect_equal(length(predictions), 10) # Predictions should match the number of samples
  expect_true(all(predictions %in% 1:3)) # Predictions should be valid class labels
})

