test_that("Preprocessor identifies columns correctly", {
  data <- data.frame(
    num1 = rnorm(10),
    num2 = rnorm(10),
    cat1 = factor(c("A", "B", "A", "C", "B", "A", "C", "B", "A", "B")),
    cat2 = c("X", "Y", "X", "Z", "Y", "X", "Z", "Y", "X", "Y")
  )
  preprocessor <- Preprocessor$new("label")
  preprocessor$identify_columns(data)

  expect_equal(preprocessor$numerical_cols, c("num1", "num2"))
  expect_equal(preprocessor$categorical_cols, c("cat1", "cat2"))
})
