#' @title RMSprop Optimizer
#'
#' @description
#' Implements the RMSprop optimization algorithm for training models.
#' RMSprop (Root Mean Square Propagation) adjusts the learning rate
#' based on a moving average of squared gradients, which helps stabilize
#' the learning process.
#'
#' @import R6
#' @importFrom stats runif
#' @export
RMSpropOptimizer <- R6Class("RMSpropOptimizer",
                            public = list(
                              #' @field learning_rate Numeric. The learning rate for the optimizer.
                              learning_rate = NULL,

                              #' @field beta1 Numeric. The decay rate for squared gradients.
                              beta1 = NULL,

                              #' @field epsilon Numeric. A small constant to avoid division by zero.
                              epsilon = NULL,

                              #' @field m Matrix. The running average of squared gradients.
                              m = NULL,

                              #' @description
                              #' Initialize the RMSpropOptimizer with the given parameters.
                              #'
                              #' @param learning_rate Numeric. The learning rate for weight updates.
                              #' @param beta1 Numeric. The decay rate for squared gradients (between 0 and 1).
                              #' @param epsilon Numeric. A small value to avoid division by zero.
                              #' @param weight_dim Vector. Dimensions of the weight matrix (e.g., `c(rows, cols)`).
                              initialize = function(learning_rate, beta1, epsilon, weight_dim) {
                                if (!is.numeric(learning_rate) || learning_rate <= 0) {
                                  stop("learning_rate must be a positive number.")
                                }
                                if (!is.numeric(beta1) || beta1 < 0 || beta1 > 1) {
                                  stop("beta1 must be in the range [0, 1].")
                                }
                                if (!is.numeric(epsilon) || epsilon <= 0) {
                                  stop("epsilon must be a positive number.")
                                }
                                if (length(weight_dim) != 2 || any(weight_dim <= 0)) {
                                  stop("weight_dim must be a vector of two positive integers.")
                                }
                                self$learning_rate <- learning_rate
                                self$beta1 <- beta1
                                self$epsilon <- epsilon
                                self$m <- matrix(0, nrow = weight_dim[1], ncol = weight_dim[2])
                              },

                              #' @description
                              #' Updates the weights using the RMSprop optimization algorithm.
                              #'
                              #' @param W Matrix. Current weight matrix.
                              #' @param gradient Matrix. Gradient of the loss function with respect to the weights.
                              #' @return Updated weight matrix after applying the RMSprop update rule.
                              update = function(W, gradient) {
                                # Update the running average of squared gradients
                                self$m <- self$beta1 * self$m + (1 - self$beta1) * (gradient ^ 2)
                                # Update weights using RMSprop
                                W <- W - (self$learning_rate / (sqrt(self$m) + self$epsilon)) * gradient
                                return(W)
                              }
                            )
)
