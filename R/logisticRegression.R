# Classe de prétraitement avec exploration interactive de l'AFDM
Preprocessor <- R6Class("Preprocessor",
                        public = list(
                          encoding  = NULL,
                          categorical_columns = NULL,
                          quantitative_columns = NULL,
                          n_famd_dim = NULL,

                          initialize = function(encoding = "onehot", n_famd_dim = NULL) {
                            self$encoding  <- encoding
                            self$n_famd_dim <- n_famd_dim  # L'utilisateur peut définir n_famd_dim ici
                          },

                          identify_columns = function(X) {
                            self$quantitative_columns <- names(X)[sapply(X, is.numeric)]
                            self$categorical_columns <- names(X)[sapply(X, function(col) is.factor(col) || is.character(col))]
                            if (length(self$quantitative_columns) == 0) {
                              stop("Aucune colonne quantitative détectée.")
                            }
                            if (length(self$categorical_columns) == 0) {
                              cat("Aucune colonne catégorielle détectée.\n")
                            }
                          },

                          # Fonction d'exploration de l'AFDM
                          explore_famd = function(X) {
                            famd_result <- FactoMineR::FAMD(X, graph = FALSE)
                            eig.val <- famd_result$eig

                            # Affichage du graphique des variances expliquées
                            barplot(eig.val[, 2], names.arg = 1:nrow(eig.val),
                                    main = "Variances Explained by Dimensions (%)",
                                    xlab = "Principal Dimensions", ylab = "Percentage of variances", col = "steelblue")
                            lines(x = 1:nrow(eig.val), eig.val[, 2], type = "b", pch = 19, col = "red")
                            cat("Examen terminé. Vous pouvez choisir un nombre de composantes selon le graphique.\n")
                            cat("Si vous ne choisissez pas, le modèle utilisera les composantes pour 80% de variance.\n")
                          },

                          # Choix du nombre de composantes automatiquement si non défini
                          set_famd_components = function(X) {
                            if (is.null(self$n_famd_dim)) {
                              self$n_famd_dim <- self$determine_famd_components(X)
                            }
                            cat("Nombre de composantes FAMD choisi :", self$n_famd_dim, "\n")
                          },

                          # Prétraitement en fonction de l'encodage et n_famd_dim
                          fit_transform = function(X) {
                            self$identify_columns(X)
                            if (self$encoding == "afdm") {
                              if (is.null(self$n_famd_dim)) {
                                self$set_famd_components(X)  # Définir automatiquement si non défini
                              }
                              famd_result <- FactoMineR::FAMD(X, ncp = self$n_famd_dim, graph = FALSE)
                              return(famd_result$ind$coord[, 1:self$n_famd_dim])  # Retourner les composantes
                            } else if (self$encoding == "onehot") {
                              return(as.matrix(self$apply_onehot(X)))
                            } else if (self$encoding == "label") {
                              return(as.matrix(self$apply_label_encoding(X)))
                            } else {
                              stop("Type d'encodage non supporté")
                            }
                          },

                          # Fonction de transformation des données
                          transform = function(X) {
                            self$identify_columns(X)
                            return(self$fit_transform(X))
                          },

                          # Encodage One-Hot pour les variables catégorielles
                          apply_onehot = function(X) {
                            X_quantitative <- X[self$quantitative_columns]
                            X_categorical <- X[self$categorical_columns]
                            if (length(self$categorical_columns) > 0) {
                              dummy <- caret::dummyVars(~ ., data = X_categorical)
                              X_encoded <- predict(dummy, newdata = X_categorical)
                            } else {
                              X_encoded <- NULL
                            }
                            if (is.null(X_encoded)) {
                              return(as.matrix(X_quantitative))
                            } else {
                              return(cbind(as.matrix(X_quantitative), X_encoded))
                            }
                          },

                          # Encodage Label pour les variables catégorielles
                          apply_label_encoding = function(X) {
                            X_encoded <- X
                            for (col in self$categorical_columns) {
                              X_encoded[[col]] <- as.numeric(factor(X[[col]])) - 1
                            }
                            return(X_encoded)
                          },

                          # Calcul automatique des composantes FAMD pour 80% de variance
                          determine_famd_components = function(X) {
                            famd_result <- FactoMineR::FAMD(X, graph = FALSE)
                            explained_variance <- cumsum(famd_result$eig[, 2]) / sum(famd_result$eig[, 2])
                            n_components <- which(explained_variance >= 0.80)[1]
                            return(n_components)
                          }
                        )
)




# Classe de régression logistique multinomiale
LogisticRegression <- R6Class("LogisticRegression",
                              public = list(
                                coefficients = NULL,
                                max_iter = 100,
                                learning_rate = 0.01,
                                preprocessor = NULL,
                                n_features = NULL,
                                n_classes = NULL,
                                fitted = FALSE,
                                alpha = 1,       # Contrôle de la force de la régularisation (L1 + L2)
                                l1_ratio = 0.5,  # Ratio entre L1 et L2 (0: L2, 1: L1)

                                initialize = function(max_iter = 100, learning_rate = 0.01, encoding = "onehot", alpha = 1, l1_ratio = 0.5) {
                                  self$max_iter <- max_iter
                                  self$learning_rate <- learning_rate
                                  self$alpha <- alpha
                                  self$l1_ratio <- l1_ratio
                                  self$preprocessor <- Preprocessor$new(encoding)
                                },

                                softmax = function(z) {
                                  exp_z <- exp(z - apply(z, 1, max))
                                  return(exp_z / rowSums(exp_z))
                                },

                                # Fonction de coût modifiée pour inclure ElasticNet
                                cost_function = function(X, y) {
                                  m <- nrow(X)
                                  y_one_hot <- matrix(0, m, self$n_classes)
                                  y_one_hot[cbind(1:m, y + 1)] <- 1
                                  linear_model <- X %*% self$coefficients
                                  probabilities <- self$softmax(linear_model)

                                  # Log loss
                                  log_loss <- -sum(y_one_hot * log(probabilities + 1e-15)) / m

                                  # Termes de régularisation L1 et L2 (ElasticNet)
                                  l1_term <- self$l1_ratio * sum(abs(self$coefficients))
                                  l2_term <- (1 - self$l1_ratio) * sum(self$coefficients^2) / 2

                                  return(log_loss + self$alpha * (l1_term + l2_term))
                                },

                                # Gradient de la fonction de coût avec ElasticNet
                                compute_gradient = function(X, y) {
                                  m <- nrow(X)
                                  y_one_hot <- matrix(0, m, self$n_classes)
                                  y_one_hot[cbind(1:m, y + 1)] <- 1
                                  linear_model <- X %*% self$coefficients
                                  probabilities <- self$softmax(linear_model)
                                  gradient <- t(X) %*% (probabilities - y_one_hot) / m

                                  # Gradient de la régularisation L1 et L2
                                  # L1 (lasso) utilise la dérivée de la valeur absolue (sign)
                                  # L2 (ridge) utilise la dérivée du carré
                                  l1_grad <- self$l1_ratio * sign(self$coefficients)
                                  l2_grad <- (1 - self$l1_ratio) * self$coefficients

                                  return(gradient + self$alpha * (l1_grad + l2_grad))
                                },

                                fit = function(X, y) {
                                  X_processed <- self$preprocessor$fit_transform(X)
                                  X_processed <- as.matrix(X_processed)
                                  self$n_features <- ncol(X_processed)
                                  self$n_classes <- length(unique(y))
                                  self$coefficients <- matrix(0, self$n_features, self$n_classes)

                                  for (iter in 1:self$max_iter) {
                                    # Calcul du gradient
                                    gradient <- self$compute_gradient(X_processed, y)
                                    # Mise à jour des coefficients
                                    self$coefficients <- self$coefficients - self$learning_rate * gradient
                                  }
                                  self$fitted <- TRUE
                                },

                                predict = function(X) {
                                  if (!self$fitted) stop("Le modèle n'est pas encore entraîné.")
                                  probabilities <- self$predict_proba(X)
                                  return(apply(probabilities, 1, which.max) - 1)
                                },

                                predict_proba = function(X) {
                                  if (!self$fitted) stop("Le modèle n'est pas encore entraîné.")
                                  X_processed <- self$preprocessor$transform(X)
                                  linear_model <- X_processed %*% self$coefficients
                                  return(self$softmax(linear_model))
                                },

                                summary = function() {
                                  if (!self$fitted) {
                                    cat("Modèle non entraîné\n")
                                  } else {
                                    cat("Modèle Régression Logistique Multinomiale avec ElasticNet\n")
                                    cat("Caractéristiques:", self$n_features, "| Classes:", self$n_classes, "\n")
                                    cat("Alpha:", self$alpha, "| L1_ratio:", self$l1_ratio, "\n")
                                    cat("Iterations:", self$max_iter, "\n")
                                    cat("Coefficients:\n")
                                    print(self$coefficients)
                                  }
                                }
                              )
)

