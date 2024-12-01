#' @title Preprocessor Class
#' @description A class for preprocessing data, including handling mixed types (categorical and numerical),
#' scaling, and encoding (label encoding, one-hot encoding, or AFDM for dimensionality reduction).
#' @import R6
#' @importFrom FactoMineR FAMD
#' @importFrom graphics barplot lines
#' @importFrom stats runif
#' @export
Preprocessor <- R6Class("Preprocessor",
                        public = list(
                          #' @field categorical_cols Names of categorical columns in the dataset.
                          categorical_cols = NULL,

                          #' @field numerical_cols Names of numerical columns in the dataset.
                          numerical_cols = NULL,

                          #' @field label_mappings A list of mappings for label encoding of categorical variables.
                          label_mappings = NULL,

                          #' @field one_hot_columns A list of column names generated during one-hot encoding.
                          one_hot_columns = NULL,

                          #' @field scaling_params Parameters (mean, sd) for scaling numerical variables.
                          scaling_params = NULL,

                          #' @field afdm_model The AFDM model object for dimensionality reduction.
                          afdm_model = NULL,

                          #' @field encoding_type Encoding type: "label", "one-hot", or "afdm".
                          encoding_type = NULL,

                          #' @description Initialize the preprocessor with the chosen encoding type.
                          #' @param encoding_type The type of encoding to apply: "label", "one-hot", or "afdm".
                          initialize = function(encoding_type = c("label", "one-hot", "afdm")) {
                            self$encoding_type <- match.arg(encoding_type)
                            self$scaling_params <- list()
                          },

                          #' @description Identify categorical and numerical columns in a dataset.
                          #' @param data A data frame.
                          identify_columns = function(data) {
                            self$categorical_cols <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
                            self$numerical_cols <- names(data)[sapply(data, is.numeric)]
                          },

                          #' @description Scale numerical data to have zero mean and unit variance.
                          #' @param data A data frame.
                          #' @param is_training A boolean indicating if this is the training dataset.
                          #' @return The scaled dataset.
                          scale_data = function(data, is_training = TRUE) {
                            if (is_training) {
                              self$scaling_params <- lapply(data[self$numerical_cols], function(col) {
                                list(mean = mean(col, na.rm = TRUE), sd = sd(col, na.rm = TRUE))
                              })
                            }
                            data[self$numerical_cols] <- lapply(names(self$scaling_params), function(col) {
                              (data[[col]] - self$scaling_params[[col]]$mean) / self$scaling_params[[col]]$sd
                            })
                            return(data)
                          },

                          #' @description Apply label encoding to categorical variables.
                          #' @param data A data frame.
                          #' @param is_training A boolean indicating if this is the training dataset.
                          #' @return The label-encoded dataset.
                          encode_label = function(data, is_training = TRUE) {
                            if (is_training) {
                              self$label_mappings <- list()
                              for (col in self$categorical_cols) {
                                levels <- unique(data[[col]])
                                self$label_mappings[[col]] <- setNames(seq_along(levels), levels)
                                data[[col]] <- self$label_mappings[[col]][data[[col]]]
                              }
                            } else {
                              for (col in self$categorical_cols) {
                                data[[col]] <- self$label_mappings[[col]][data[[col]]]
                              }
                            }
                            return(data)
                          },

                          #' @description Apply one-hot encoding to categorical variables.
                          #' @param data A data frame.
                          #' @param is_training A boolean indicating if this is the training dataset.
                          #' @return The one-hot encoded dataset as a matrix.
                          encode_one_hot = function(data, is_training = TRUE) {
                            if (is_training) {
                              self$one_hot_columns <- list()
                              for (col in self$categorical_cols) {
                                one_hot <- model.matrix(~ . - 1, data = data[, col, drop = FALSE])
                                self$one_hot_columns[[col]] <- colnames(one_hot)
                                data <- cbind(data, one_hot)
                                data[[col]] <- NULL
                              }
                              data <- as.matrix(data)
                            } else {
                              for (col in self$categorical_cols) {
                                one_hot <- model.matrix(~ . - 1, data = data[, col, drop = FALSE])
                                columns_to_add <- setdiff(self$one_hot_columns[[col]], colnames(one_hot))
                                for (missing_col in columns_to_add) {
                                  one_hot[, missing_col] <- 0
                                }
                                data <- cbind(data, one_hot)
                                data[[col]] <- NULL
                              }
                              data <- as.matrix(data)
                            }
                            return(data)
                          },

                          #' @description Explore variances explained by dimensions in AFDM.
                          #' @param X A dataset.
                          #' @return Variances explained by each dimension.
                          explore_famd = function(X) {
                            famd_result <- FactoMineR::FAMD(X)
                            eig.val <- famd_result$eig
                            barplot(eig.val[, 2], names.arg = 1:nrow(eig.val),
                                    main = "Variances Explained by Dimensions (%)",
                                    xlab = "Principal Dimensions", ylab = "Percentage of variances", col = "steelblue")
                            lines(x = 1:nrow(eig.val), eig.val[, 2], type = "b", pch = 19, col = "red")
                            return(eig.val)
                          },

                          #' @description Apply AFDM encoding for dimensionality reduction.
                          #' @param data A data frame.
                          #' @param is_training A boolean indicating if this is the training dataset.
                          #' @param ncp The number of principal components to retain.
                          #' @return The dataset projected into the AFDM space.
                          encode_afdm = function(data, is_training = TRUE, ncp = NULL) {
                            if (is_training) {
                              if (is.null(ncp)) {
                                temp_famd <- FAMD(data, ncp = Inf, graph = FALSE)
                                var_explained <- temp_famd$eig[, 2]
                                cumulative_variance <- cumsum(var_explained)
                                ncp <- which(cumulative_variance >= 80)[1]
                              }
                              message(paste("Using", ncp, "components for AFDM encoding."))
                              self$afdm_model <- FAMD(data, ncp = ncp, graph = FALSE)
                              return(as.data.frame(self$afdm_model$ind$coord))
                            } else {
                              transformed_data <- predict(self$afdm_model, newdata = data)
                              return(as.data.frame(transformed_data$coord))
                            }
                          },

                          #' @description Preprocess the dataset by scaling and encoding.
                          #' @param data A data frame.
                          #' @param is_training A boolean indicating if this is the training dataset.
                          #' @param ncp The number of principal components to retain for AFDM.
                          #' @return The preprocessed dataset.
                          preprocess = function(data, is_training = TRUE, ncp = NULL) {
                            self$identify_columns(data)
                            data <- self$scale_data(data, is_training)

                            if (self$encoding_type == "label") {
                              data <- self$encode_label(data, is_training)
                            } else if (self$encoding_type == "one-hot") {
                              data <- self$encode_one_hot(data, is_training)
                            } else if (self$encoding_type == "afdm") {
                              data <- self$encode_afdm(data, is_training, ncp = ncp)
                            }

                            return(data)
                          }
                        )
)
