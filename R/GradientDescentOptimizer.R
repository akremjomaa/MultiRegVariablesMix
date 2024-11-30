#' @title Gradient Descent Optimizer
#'
#' @description
#' Implements the standard Gradient Descent optimization algorithm.
#' The optimizer updates the weights by moving in the direction of the negative gradient of the loss function.
#'
#' @import R6
#' @importFrom stats runif
#' @export
GradientDescentOptimizer <- R6Class("GradientDescentOptimizer",
                                    public = list(
                                      #' @field learning_rate Numeric. The learning rate for the optimizer.
                                      learning_rate = NULL,

                                      #' @description
                                      #' Initialize the GradientDescentOptimizer with the given learning rate.
                                      #'
                                      #' @param learning_rate Numeric. The learning rate for the optimizer.
                                      initialize = function(learning_rate) {
                                        self$learning_rate <- learning_rate
                                      },

                                      #' @description
                                      #' Updates the weights using the Gradient Descent algorithm.
                                      #'
                                      #' @param W Matrix. Current weight matrix.
                                      #' @param gradient Matrix. Gradient of the loss function with respect to the weights.
                                      #' @return Updated weight matrix after applying the Gradient Descent update rule.
                                      update = function(W, gradient) {
                                        W <- W - self$learning_rate * gradient
                                        return(W)
                                      }
                                    )
)
