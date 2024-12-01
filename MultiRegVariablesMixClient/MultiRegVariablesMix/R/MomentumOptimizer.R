#' @title Momentum Optimizer
#'
#' @description
#' Implements the Momentum optimization algorithm for training models.
#' The optimizer updates the weights using the momentum term, which accelerates
#' gradient descent in the relevant direction and dampens oscillations.
#'
#' @import R6
#' @importFrom stats runif
#' @export
MomentumOptimizer <- R6Class("MomentumOptimizer",
                             public = list(
                               #' @field learning_rate Numeric. The learning rate for the optimizer.
                               learning_rate = NULL,

                               #' @field beta1 Numeric. The momentum coefficient (between 0 and 1).
                               beta1 = NULL,

                               #' @field m Matrix. The momentum term, initialized as a zero matrix.
                               m = NULL,

                               #' @description
                               #' Initialize the MomentumOptimizer with the given parameters.
                               #'
                               #' @param learning_rate Numeric. The learning rate for weight updates.
                               #' @param beta1 Numeric. The momentum coefficient (between 0 and 1).
                               #' @param weight_dim Vector. Dimensions of the weight matrix (e.g., `c(rows, cols)`).
                               initialize = function(learning_rate, beta1, weight_dim) {
                                 if (!is.numeric(learning_rate) || learning_rate <= 0) {
                                   stop("learning_rate must be a positive number.")
                                 }
                                 if (!is.numeric(beta1) || beta1 < 0 || beta1 > 1) {
                                   stop("beta1 must be in the range [0, 1].")
                                 }
                                 if (length(weight_dim) != 2 || any(weight_dim <= 0)) {
                                   stop("weight_dim must be a vector of two positive integers.")
                                 }
                                 self$learning_rate <- learning_rate
                                 self$beta1 <- beta1
                                 self$m <- matrix(0, nrow = weight_dim[1], ncol = weight_dim[2])
                               },

                               #' @description
                               #' Updates the weights using the Momentum optimization algorithm.
                               #'
                               #' @param W Matrix. Current weight matrix.
                               #' @param gradient Matrix. Gradient of the loss function with respect to the weights.
                               #' @return Updated weight matrix after applying the Momentum update.
                               update = function(W, gradient) {
                                 # Update the momentum term
                                 self$m <- self$beta1 * self$m + (1 - self$beta1) * gradient
                                 # Update weights
                                 W <- W - self$learning_rate * self$m
                                 return(W)
                               }
                             )
)
