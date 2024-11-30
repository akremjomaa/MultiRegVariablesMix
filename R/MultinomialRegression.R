#'@title Multinomial Regression Model
#'
#'@description This class implements a multinomial regression model using R6, with support for
#' preprocessing, softmax activation, and gradient-based optimization.
#'
#' @field X The input matrix (features), with an added bias term.
#' @field y The target variable (labels).
#' @field W The weight matrix of the model.
#' @field optimizer The optimizer used for updating weights.
#' @field epochs The number of epochs for training.
#' @field batch_size The batch size for mini-batch gradient descent.
#' @field preprocessor An instance of the `Preprocessor` class for data preprocessing.
#' @field optimizer_type The type of optimizer used (e.g., "momentum", "rmsprop", "adam").
#' @field learning_rate The learning rate for gradient descent.
#' @field beta1 The beta1 parameter for optimizers like Adam and RMSprop.
#' @field beta2 The beta2 parameter for the Adam optimizer.
#' @field epsilon A small value to prevent division by zero in optimizers.
#' @field l1_ratio The L1 regularization ratio.
#' @field a The regularization strength parameter.
#' @field label_mappings A mapping of class labels.
#' @field data_preprocessed A flag indicating if the data has been preprocessed.
#' @field cost_history A vector storing the cost at each epoch.
#' @import R6
#' @import ggplot2
#' @import stats
#' @importFrom utils head
#' @export
MultinomialRegression <- R6Class("MultinomialRegression",
                                 public = list(
                                   X = NULL,
                                   y = NULL,
                                   W = NULL,
                                   optimizer = NULL,
                                   epochs = NULL,
                                   batch_size = NULL,
                                   preprocessor = NULL,
                                   optimizer_type = NULL,
                                   learning_rate = NULL,
                                   beta1 = NULL,
                                   beta2 = NULL,
                                   epsilon = NULL,
                                   l1_ratio = NULL,
                                   a = NULL,
                                   label_mappings = NULL,
                                   data_preprocessed = FALSE,
                                   cost_history = c(),

                                   #' @description
                                   #' Initializes the multinomial regression model.
                                   #'
                                   #' @param encoding_type The type of encoding for categorical variables ("label", "one-hot", "afdm").
                                   #' @param optimizer_type The optimizer type ("momentum", "rmsprop", "gd", "adam").
                                   #' @param learning_rate The learning rate for the optimizer (default: 0.01).
                                   #' @param epochs The number of epochs for training (default: 10000).
                                   #' @param batch_size The batch size for mini-batch gradient descent.
                                   #' @param beta1 The beta1 parameter for momentum-based optimizers (default: 0.9).
                                   #' @param beta2 The beta2 parameter for Adam optimizer (default: 0.999).
                                   #' @param epsilon A small value to prevent division by zero in optimizers (default: 1e-8).
                                   #' @param a The regularization strength parameter (default: 0).
                                   #' @param l1_ratio The L1 regularization ratio (default: 0).
                                   initialize = function(encoding_type = c("label", "one-hot", "afdm"),
                                                         optimizer_type = c("momentum", "rmsprop", "gd", "adam"),
                                                         learning_rate = 0.01,
                                                         epochs = 10000,
                                                         batch_size = NULL,
                                                         beta1 = 0.9,
                                                         beta2 = 0.999,
                                                         epsilon = 1e-8,
                                                         a = 0,
                                                         l1_ratio = 0) {
                                     self$preprocessor <- Preprocessor$new(encoding_type = encoding_type)
                                     self$epochs <- epochs
                                     self$batch_size <- batch_size
                                     self$optimizer_type <- match.arg(optimizer_type)
                                     self$learning_rate <- learning_rate
                                     self$beta1 <- beta1
                                     self$epsilon <- epsilon
                                     self$a <- a
                                     self$l1_ratio <- l1_ratio
                                     self$data_preprocessed <- FALSE
                                     self$cost_history <- c()
                                   },

                                   #' @description
                                   #' Computes the softmax function for a given input matrix.
                                   #'
                                   #' @param z A numeric matrix of logits.
                                   #' @return A matrix of probabilities.
                                   softmax = function(z) {
                                     exp(z) / rowSums(exp(z))
                                   },

                                   #' @description
                                   #' Converts a vector of class labels into a one-hot encoded matrix.
                                   #'
                                   #' @param y A vector of class labels.
                                   #' @param K The number of unique classes.
                                   #' @return A one-hot encoded matrix.
                                   one_hot_encode = function(y, K) {
                                     n <- length(y)
                                     y_onehot <- matrix(0, n, K)
                                     for (i in 1:n) {
                                       y_onehot[i, y[i]] <- 1
                                     }
                                     return(y_onehot)
                                   },

                                   #' @description
                                   #' Performs a Factorial Analysis of Mixed Data (FAMD) for exploratory analysis.
                                   #'
                                   #' @param data The dataset to analyze.
                                   explore_famd = function(data) {
                                     self$preprocessor$explore_famd(data)
                                   },

                                   #' @description
                                   #' Preprocesses the data using the preprocessor instance.
                                   #'
                                   #' @param X The feature matrix.
                                   #' @param y The target vector (optional).
                                   #' @param is_training A boolean indicating if the data is for training.
                                   #' @param ncp Number of components to retain during dimensionality reduction (optional).
                                   preprocess = function(X, y = NULL, is_training = TRUE, ncp = NULL) {
                                     X <- self$preprocessor$preprocess(X, is_training = is_training, ncp = ncp)
                                     self$X <- cbind(1, as.matrix(X))  # Add bias term
                                     self$y <- y
                                     self$data_preprocessed <- TRUE
                                   },

                                   #' @description
                                   #' Fits the model to the given data.
                                   #'
                                   #' @param X The feature matrix.
                                   #' @param y The target vector.
                                   #' @param preprocess A boolean indicating if the data should be preprocessed.
                                   #' @param ncp Number of components to retain during dimensionality reduction (optional).
                                   fit = function(X = NULL, y = NULL, preprocess = TRUE, ncp = NULL) {
                                     if (!self$data_preprocessed && preprocess) {
                                       if (is.null(X) || is.null(y)) {
                                         stop("Please provide X and y if preprocess = TRUE.")
                                       }
                                       self$preprocess(X, y, is_training = TRUE, ncp = ncp)
                                     } else if (!self$data_preprocessed && !preprocess) {
                                       if (is.null(X) || is.null(y)) {
                                         stop("Data must be preprocessed or provided directly.")
                                       }
                                       self$X <- cbind(1, as.matrix(X))  # Manually add bias term
                                       self$y <- y
                                     }

                                     if (is.null(self$X) || is.null(self$y)) {
                                       stop("Data is not available. Ensure that preprocess() was called or provide X and y directly.")
                                     }
                                     X <- self$X
                                     y <- self$y
                                     n <- nrow(X)
                                     p <- ncol(X)
                                     classes <- unique(y)
                                     K <- length(classes)
                                     self$W <- matrix(0, p, K)

                                     # Initialize optimizer
                                     if (self$optimizer_type == "momentum") {
                                       self$optimizer <- MomentumOptimizer$new(self$learning_rate, self$beta1, c(p, K))
                                     } else if (self$optimizer_type == "rmsprop") {
                                       self$optimizer <- RMSpropOptimizer$new(self$learning_rate, self$beta1, self$epsilon, c(p, K))
                                     } else if (self$optimizer_type == "gd") {
                                       self$optimizer <- GradientDescentOptimizer$new(self$learning_rate)
                                     } else if (self$optimizer_type == "adam") {
                                       self$optimizer <- AdamOptimizer$new(self$learning_rate, beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, c(p, K))
                                     }

                                     y_onehot <- self$one_hot_encode(y, K)
                                     cost = Inf
                                     self$cost_history <- c()

                                     for (epoch in 1:self$epochs) {
                                       if (is.null(self$batch_size) || self$batch_size >= nrow(X)) {
                                         X_batch <- X
                                         y_batch <- y_onehot
                                       } else {
                                         indices <- sample(1:n, self$batch_size, replace = TRUE)
                                         X_batch <- X[indices, ]
                                         y_batch <- y_onehot[indices, ]
                                       }
                                       logits <- X_batch %*% self$W
                                       probs <- self$softmax(logits)
                                       gradient <- t(X_batch) %*% (probs - y_batch) / nrow(X_batch)
                                       self$W <- self$optimizer$update(self$W, gradient)

                                       if (epoch %% 100 == 0) {
                                         if ((cost - sum(y_batch * log(probs)) / nrow(X_batch)) < 1e-4) {
                                           print("Convergence reached. Stopping early.")
                                           break
                                         }
                                         cost <- -sum(y_batch * log(probs)) / nrow(X_batch)
                                         self$cost_history <- c(self$cost_history, cost)
                                         if (epoch %% 100 == 0) {
                                           cat("Epoch:", epoch, "- Cost:", cost, "\n")
                                         }
                                       }
                                     }
                                   },
                                 #' @description
                                 #' Plots the cost evolution during training.
                                 #'
                                 #' @return A ggplot object visualizing the cost evolution.
                                 plot_cost_evolution = function() {
                                   if (is.null(self$cost_history) || length(self$cost_history) == 0) {
                                     stop("The cost history is empty. Train the model first.")
                                   }

                                   cost_data <- data.frame(
                                     epoch = 1:length(self$cost_history),
                                     cost = self$cost_history
                                   )

                                   ggplot(cost_data, aes(x = epoch, y = cost)) +
                                     geom_line(color = "skyblue", linewidth = 1) +
                                     theme_minimal() +
                                     labs(title = "Cost Evolution During Training",
                                          x = "Epoch",
                                          y = "Cost") +
                                     theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
                                 },

                                 #' @description
                                 #' Plots the distribution of predictions by class.
                                 #'
                                 #' @param X The input data for generating predictions.
                                 #' @param y_true Optional true class labels for comparison.
                                 #' @return A ggplot object visualizing the distribution of predictions.
                                 plot_prediction_distribution = function(X, y_true = NULL) {
                                   if (is.null(self$W)) {
                                     stop("The model is not yet trained. Unable to generate predictions.")
                                   }

                                   predictions <- self$predict(X)
                                   prediction_data <- data.frame(predicted_class = as.factor(predictions))

                                   if (!is.null(y_true)) {
                                     prediction_data$true_class <- as.factor(y_true)
                                   }

                                   plot <- ggplot(prediction_data, aes(x = predicted_class, fill = predicted_class)) +
                                     geom_bar(alpha = 0.7) +
                                     scale_fill_brewer(palette = "Set3") +
                                     theme_minimal() +
                                     labs(
                                       title = "Distribution of Predictions by Class",
                                       x = "Predicted Class",
                                       y = "Number of Predictions"
                                     )

                                   if (!is.null(y_true)) {
                                     plot <- plot + facet_wrap(~ true_class, ncol = 1, scales = "free_y") +
                                       labs(subtitle = "Faceted by True Class")
                                   }

                                   return(plot)
                                 },

                                 #' @description
                                 #' Predicts the class labels for the given input data.
                                 #'
                                 #' @param X The input data matrix.
                                 #' @return A vector of predicted class labels.
                                 predict = function(X) {
                                   probs <- self$predict_proba(X)
                                   predictions <- apply(probs, 1, which.max)
                                   return(predictions)
                                 },

                                 #' @description
                                 #' Predicts the class probabilities for the given input data.
                                 #'
                                 #' @param X The input data matrix.
                                 #' @return A matrix of predicted probabilities for each class.
                                 predict_proba = function(X) {
                                   X <- self$preprocessor$preprocess(X, is_training = FALSE)
                                   X <- cbind(1, as.matrix(X))
                                   logits <- X %*% self$W
                                   probs <- self$softmax(logits)
                                   return(probs)
                                 },

                                 #' @description
                                 #' Computes the importance of each variable based on model weights.
                                 #'
                                 #' @return A named vector of variable importance scores, sorted in decreasing order.
                                 var_importance = function() {
                                   if (is.null(self$W)) {
                                     stop("The model is not yet trained. Unable to calculate variable importance.")
                                   }
                                   W_no_bias <- self$W[-1, , drop = FALSE]  # Exclude the first row (bias)
                                   importance <- rowSums(abs(W_no_bias))  # Sum of weights for each variable
                                   importance_sorted <- sort(importance, decreasing = TRUE)

                                   importance_data <- data.frame(
                                     variable = names(importance),
                                     importance = importance
                                   )

                                   plot <- ggplot(importance_data, aes(x = reorder(variable, -importance), y = importance)) +
                                     geom_bar(stat = "identity", fill = "skyblue") +
                                     theme_minimal() +
                                     labs(title = "Variable Importance",
                                          x = "Variable",
                                          y = "Importance") +
                                     theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                   print(plot)
                                   return(importance_sorted)
                                 },

                                 #' @description
                                 #' Selects variables based on a given importance threshold.
                                 #'
                                 #' @param threshold The quantile threshold for variable selection (default: 0.75).
                                 #' @return A subset of the input matrix `X` containing selected variables.
                                 var_select = function(threshold = 0.75) {
                                   importance <- self$var_importance()
                                   threshold_value <- quantile(importance, threshold)
                                   selected_vars <- which(importance >= threshold_value)
                                   cat("Selected variables:", names(self$X)[selected_vars], "\n")
                                   return(self$X[, selected_vars, drop = FALSE])
                                 },
                                 #' @description
                                 #' Plots the confusion matrix for the given true and predicted labels.
                                 #'
                                 #' @param y_true The true class labels.
                                 #' @param y_pred The predicted class labels.
                                 #' @param classes Optional vector of class labels. If NULL, derived from `y_true` and `y_pred`.
                                 #' @return A ggplot object visualizing the confusion matrix.
                                 plot_confusion_matrix = function(y_true, y_pred, classes = NULL) {
                                   if (is.null(classes)) {
                                     classes <- sort(unique(c(y_true, y_pred)))
                                   }
                                   cm <- table(y_true, y_pred)
                                   cm_data <- as.data.frame(as.table(cm))
                                   colnames(cm_data) <- c("Actual", "Predicted", "Count")

                                   plot <- ggplot(cm_data, aes(x = Predicted, y = Actual, fill = Count)) +
                                     geom_tile(color = "white") +
                                     scale_fill_gradient(low = "skyblue", high = "blue") +
                                     geom_text(aes(label = Count), color = "white", size = 5) +
                                     labs(title = "Confusion Matrix",
                                          x = "Predicted",
                                          y = "Actual") +
                                     theme_minimal() +
                                     theme(axis.text.x = element_text(angle = 45, hjust = 1))
                                   print(plot)
                                 },

                                 #' @description
                                 #' Plots the ROC curves for a multi-class classification model.
                                 #'
                                 #' @param X The input data.
                                 #' @param y_true The true class labels.
                                 #' @return A ggplot object visualizing the ROC curves.
                                 plot_roc = function(X, y_true) {
                                   if (is.null(self$W)) {
                                     stop("The model is not yet trained. Unable to plot the ROC curve.")
                                   }
                                   y_probs <- self$predict_proba(X)
                                   classes <- sort(unique(y_true))
                                   roc_data_list <- list()
                                   auc_values <- c()

                                   for (class in classes) {
                                     y_true_binary <- as.integer(y_true == class)  # Current class as positive
                                     y_scores <- y_probs[, which(classes == class)]

                                     # Compute ROC for this class
                                     roc_curve <- roc(y_true_binary, y_scores)
                                     auc_values <- c(auc_values, auc(roc_curve))

                                     roc_data_list[[as.character(class)]] <- data.frame(
                                       FPR = rev(roc_curve$specificities),
                                       TPR = rev(roc_curve$sensitivities),
                                       Class = as.character(class)
                                     )
                                   }

                                   roc_data <- do.call(rbind, roc_data_list)

                                   # Plot the ROC curves
                                   plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Class)) +
                                     geom_line() +
                                     geom_abline(linetype = "dashed", color = "red") +
                                     theme_minimal() +
                                     labs(title = sprintf("Multi-Class ROC Curves (Mean AUC = %.3f)", mean(auc_values)),
                                          x = "False Positive Rate (FPR)",
                                          y = "True Positive Rate (TPR)") +
                                     theme(text = element_text(size = 12))
                                   print(plot)
                                 },

                                 #' @description
                                 #' Prints a brief summary of the model and its current state.
                                 #'
                                 #' @return Prints information about the model.
                                 print = function() {
                                   cat("Multinomial Regression Model\n")
                                   cat("----------------------------\n")
                                   cat(sprintf("Encoding type: %s\n", self$preprocessor$encoding_type))
                                   cat(sprintf("Preprocessed: %s\n", ifelse(self$data_preprocessed, "Yes", "No")))
                                   if (!is.null(self$X)) {
                                     cat(sprintf("Training data: %d observations, %d features\n", nrow(self$X), ncol(self$X) - 1))
                                   } else {
                                     cat("Training data: Not yet provided.\n")
                                   }
                                   cat(sprintf("Model trained: %s\n", ifelse(!is.null(self$W), "Yes", "No")))
                                   cat("----------------------------\n")
                                 },

                                 #' @description
                                 #' Provides a detailed summary of the model, its data, and its training status.
                                 #'
                                 #' @return Prints a detailed summary of the model.
                                 summary = function() {
                                   cat("Summary of Multinomial Regression Model\n")
                                   cat("---------------------------------------\n")

                                   # Part 1: General Information
                                   cat("Model Information:\n")
                                   cat("  - Model type: Multinomial Regression\n")
                                   cat("  - Objective: Multinomial classification with mixed-type features.\n")
                                   cat(sprintf("  - Encoding type: %s\n", self$preprocessor$encoding_type))
                                   cat(sprintf("  - Preprocessed: %s\n", ifelse(self$data_preprocessed, "Yes", "No")))
                                   cat("\n")

                                   # Part 2: Data Information
                                   cat("Data Information:\n")
                                   if (!is.null(self$X)) {
                                     cat(sprintf("  - Training data: %d observations, %d features\n", nrow(self$X), ncol(self$X) - 1))
                                   } else {
                                     cat("  - Training data: Not provided.\n")
                                   }
                                   if (!is.null(self$y)) {
                                     classes <- unique(self$y)
                                     cat(sprintf("  - Classes: %s\n", paste(classes, collapse = ", ")))
                                     class_counts <- table(self$y)
                                     cat("  - Observations per class:\n")
                                     for (cls in names(class_counts)) {
                                       cat(sprintf("      Class %s: %d observations\n", cls, class_counts[[cls]]))
                                     }
                                   } else {
                                     cat("  - Target variable: Not provided.\n")
                                   }
                                   cat("\n")

                                   # Part 3: Optimization Parameters
                                   cat("Optimization Parameters:\n")
                                   cat(sprintf("  - Optimizer: %s\n", self$optimizer_type))
                                   cat(sprintf("  - Learning rate: %.5f\n", self$learning_rate))
                                   cat(sprintf("  - Epochs: %d\n", self$epochs))
                                   cat(sprintf("  - Batch size: %d\n", self$batch_size))
                                   if (self$optimizer_type %in% c("momentum", "rmsprop")) {
                                     cat(sprintf("  - Beta1: %f\n", self$beta1))
                                   }
                                   if (self$optimizer_type == "rmsprop") {
                                     cat(sprintf("  - Epsilon: %f\n", self$epsilon))
                                   }
                                   cat("\n")

                                   # Part 4: Training Status
                                   cat("Training Status:\n")
                                   if (!is.null(self$W)) {
                                     cat("  - Model trained: Yes\n")
                                     if (!is.null(self$y)) {
                                       cat(sprintf("  - Coefficients learned for %d features and %d classes.\n", ncol(self$X) - 1, ncol(self$W)))
                                     }
                                   } else {
                                     cat("  - Model trained: No\n")
                                   }

                                   if (!is.null(self$W)) {
                                     cat("Model Performance:\n")
                                     logits <- self$X %*% self$W
                                     probs <- self$softmax(logits)
                                     y <- self$one_hot_encode(self$y, length(classes))

                                     cost <- -sum(y * log(probs)) / nrow(self$X)
                                     cat(sprintf("  - Final cost (cross-entropy loss): %f\n", cost))

                                     # Calculate accuracy
                                     predicted_classes <- apply(probs, 1, which.max)
                                     accuracy <- mean(predicted_classes == self$y) * 100
                                     cat(sprintf("  - Accuracy: %.2f%%\n", accuracy))
                                     cat("  - Predicted probabilities (summary):\n")
                                     print(summary(probs))
                                   } else {
                                     cat("Model Performance: Not trained yet.\n")
                                   }

                                   cat("---------------------------------------\n")
                                 }
   )
  )
