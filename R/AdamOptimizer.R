#' @title Adam Optimizer
#'
#' @description
#' Implements the Adam (Adaptive Moment Estimation) optimization algorithm.
#' This optimizer combines the benefits of momentum and RMSProp for efficient
#' and adaptive gradient-based optimization. It is widely used for training deep
#' neural networks and other machine learning models.
#'
#' @import R6
#' @importFrom stats runif
#' @export
AdamOptimizer <- R6Class("AdamOptimizer",
                         public = list(

                           # Fields
                           #' @field learning_rate Learning rate for the optimizer.
                           #' @field beta1 Exponential decay rate for the first moment estimates.
                           #' @field beta2 Exponential decay rate for the second moment estimates.
                           #' @field epsilon Small constant to avoid division by zero in the update rule.
                           #' @field m First moment estimate (initialized to zero).
                           #' @field v Second moment estimate (initialized to zero).
                           #' @field t Time step (initialized to zero).

                           learning_rate = NULL,
                           beta1 = NULL,
                           beta2 = NULL,
                           epsilon = NULL,
                           m = NULL,
                           v = NULL,
                           t = 0,

                           #' @description
                           #' Initializes the Adam optimizer with the given parameters.
                           #'
                           #' @param learning_rate The learning rate for weight updates. Default: 0.001.
                           #' @param beta1 The exponential decay rate for the first moment estimates. Default: 0.9.
                           #' @param beta2 The exponential decay rate for the second moment estimates. Default: 0.999.
                           #' @param epsilon A small constant for numerical stability. Default: 1e-8.
                           #' @param weight_dim Dimensions of the weight matrix (e.g., `c(rows, cols)`).
                           initialize = function(learning_rate = 0.001, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, weight_dim) {
                             self$learning_rate <- learning_rate
                             self$beta1 <- beta1
                             self$beta2 <- beta2
                             self$epsilon <- epsilon

                             # Initialize the first and second moment vectors to zeros
                             self$m <- matrix(0, nrow = weight_dim[1], ncol = weight_dim[2])
                             self$v <- matrix(0, nrow = weight_dim[1], ncol = weight_dim[2])
                           },

                           #' @description
                           #' Updates the weights using the Adam optimization algorithm.
                           #'
                           #' @param W Current weight matrix.
                           #' @param gradient Gradient of the loss function with respect to the weights.
                           #' @return Updated weight matrix after applying the Adam update.
                           update = function(W, gradient) {
                             # Increment the time step
                             self$t <- self$t + 1

                             # Update biased first moment estimate
                             self$m <- self$beta1 * self$m + (1 - self$beta1) * gradient

                             # Update biased second moment estimate
                             self$v <- self$beta2 * self$v + (1 - self$beta2) * (gradient^2)

                             # Compute bias-corrected estimates
                             m_hat <- self$m / (1 - self$beta1^self$t)
                             v_hat <- self$v / (1 - self$beta2^self$t)

                             # Perform weight update
                             W <- W - self$learning_rate * m_hat / (sqrt(v_hat) + self$epsilon)
                             return(W)
                           }
                         )
)
