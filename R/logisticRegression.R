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
                            self$identify_columns(X)
                            X_quantitative <- X[self$quantitative_columns]
                            X[self$quantitative_columns] <- scale(X_quantitative)
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
                            # Standardiser les variables quantitatives avant le FAMD
                            X_quantitative <- X[self$quantitative_columns]
                            X[self$quantitative_columns] <- scale(X_quantitative)

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
                            #X_quantitative <- scale(X_quantitative)
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
                            #X[self$quantitative_columns] <- scale(X[self$quantitative_columns])
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

LogisticRegression <- R6Class("LogisticRegression",
        public = list(
          coefficients = NULL,
          max_iter = 100,
          learning_rate = 0.01,
          preprocessor = NULL,
          n_features = NULL,
          n_classes = NULL,
          fitted = FALSE,
          alpha = 1,       # Force de régularisation
          l1_ratio = 0.5,  # Ratio entre L1 et L2
          solver = 'lbfgs',# Paramètre pour choisir le solveur
          tolerance = 1e-4,# Critère d'arrêt (seuil de convergence)
          batch_size = 100, # Taille des mini-batchs pour les méthodes stochastiques
          n_famd_dim = NULL,

          initialize = function(max_iter = 100, learning_rate = 0.01, encoding = "onehot", alpha = 1, l1_ratio = 0.5 ,preprocessor = NULL, n_famd_dim = NULL, solver = 'lbfgs') {
            self$max_iter <- max_iter
            self$learning_rate <- learning_rate
            self$alpha <- alpha
            self$l1_ratio <- l1_ratio
            self$solver <- solver
            self$preprocessor <- preprocessor

            # Vérification de la validité des combinaisons solver et régularisation
            self$validate_parameters()
          },

          # Fonction de validation des paramètres
          # Fonction de validation des paramètres
          validate_parameters = function() {
            # Si l1_ratio > 0 (L1 ou ElasticNet), seul 'saga' est permis pour L1 pur
            if (self$l1_ratio > 0 && self$l1_ratio < 1 && self$solver != 'saga') {
              stop("Erreur : ElasticNet (L1+L2) ne peut être utilisé qu'avec le solveur 'saga'.")
            }

            # Si l1_ratio == 1 (L1 pur), seul 'saga' est permis
            if (self$l1_ratio == 1 && self$solver != 'saga') {
              stop("Erreur : La régularisation L1 (Lasso) ne peut être utilisée qu'avec le solveur 'saga'.")
            }

            # Si l1_ratio == 0 (L2 pur), tous les solveurs sont permis
            if (self$l1_ratio == 0) {
              if (self$solver == 'saga') {
                cat("Avertissement : Le solveur 'saga' est optimisé pour L1, mais il est également utilisé pour L2.\n")
              }
            }

            # Cas pour ElasticNet avec l1_ratio entre 0 et 1, mais utilisé avec un mauvais solveur
            if (self$l1_ratio > 0 && self$l1_ratio < 1 && !self$solver %in% c('saga', 'lbfgs', 'newton-cg')) {
              stop("Erreur : ElasticNet (L1+L2) peut être utilisé avec les solveurs 'saga', 'lbfgs' ou 'newton-cg'.")
            }
          },


          softmax = function(z) {
            exp_z <- exp(z - apply(z, 1, max))
            return(exp_z / rowSums(exp_z))
          },

          cost_function = function(X, y) {
            m <- nrow(X)
            y_one_hot <- matrix(0, m, self$n_classes)
            y_one_hot[cbind(1:m, y + 1)] <- 1
            linear_model <- X %*% self$coefficients
            probabilities <- self$softmax(linear_model)

            log_loss <- -sum(y_one_hot * log(probabilities + 1e-15)) / m

            l1_term <- self$l1_ratio * sum(abs(self$coefficients))
            l2_term <- (1 - self$l1_ratio) * sum(self$coefficients^2)

            return(log_loss + self$alpha * (l1_term + l2_term))
          },

          compute_gradient = function(X, y) {
            m <- nrow(X)
            y_one_hot <- matrix(0, m, self$n_classes)
            y_one_hot[cbind(1:m, y + 1)] <- 1
            linear_model <- X %*% self$coefficients
            probabilities <- self$softmax(linear_model)

            # Calcul du gradient
            gradient <- t(X) %*% (probabilities - y_one_hot) / m

            # Terme de régularisation
            l1_grad <- self$l1_ratio * sign(self$coefficients)
            l2_grad <- (1 - self$l1_ratio) * self$coefficients

            return(gradient + self$alpha * (l1_grad + l2_grad))
          },

          lbfgs_optimizer = function(X, y) {
            for (iter in 1:self$max_iter) {
              gradient <- self$compute_gradient(X, y)
              self$coefficients <- self$coefficients - self$learning_rate * gradient
              cost <- self$cost_function(X, y)

              # Arrêter si la convergence est atteinte
              if (iter > 1 && abs(prev_cost - cost) < self$tolerance) {
                cat("Convergence atteinte à l'itération", iter, "\n")
                break
              }
              prev_cost <- cost
            }
          },
          newton_cg_optimizer = function(X, y) {
            for (iter in 1:self$max_iter) {
              # Calculer les gradients et la Hessienne
              gradient <- self$compute_gradient(X, y)
              linear_model <- X %*% self$coefficients
              probabilities <- self$softmax(linear_model)

              # Calculer la matrice diagonale des poids pour la Hessienne
              m <- nrow(X)
              W <- diag(as.vector(probabilities * (1 - probabilities)), m)

              # Calculer la Hessienne (H = X^T W X)
              hessian <- t(X) %*% W %*% X

              # Calculer le pas de Newton : H^(-1) * gradient
              step <- solve(hessian, gradient)

              # Mettre à jour les coefficients
              self$coefficients <- self$coefficients - step

              # Calculer le coût
              cost <- self$cost_function(X, y)

              # Arrêter si la convergence est atteinte
              if (iter > 1 && abs(prev_cost - cost) < self$tolerance) {
                cat("Convergence atteinte à l'itération", iter, "\n")
                break
              }

              prev_cost <- cost
            }
          },

          sag_optimizer = function(X, y) {
            for (iter in 1:self$max_iter) {
              for (i in seq(1, nrow(X), by = self$batch_size)) {
                batch_X <- X[i:min(i + self$batch_size - 1, nrow(X)), ]
                batch_y <- y[i:min(i + self$batch_size - 1, length(y))]

                # Calcul du gradient avec régularisation
                gradient <- self$compute_gradient(batch_X, batch_y)
                self$coefficients <- self$coefficients - self$learning_rate * gradient
              }

              cost <- self$cost_function(X, y)

              # Critère de convergence
              if (iter > 1 && abs(prev_cost - cost) < self$tolerance) {
                cat("Convergence atteinte à l'itération", iter, "\n")
                break
              }
              prev_cost <- cost
            }
          },

          saga_optimizer = function(X, y) {
            # Implémentation de SAGA pour la régression logistique
            for (iter in 1:self$max_iter) {
              # Utiliser des mini-batchs
              for (i in seq(1, nrow(X), by = self$batch_size)) {
                batch_X <- X[i:min(i + self$batch_size - 1, nrow(X)), ]
                batch_y <- y[i:min(i + self$batch_size - 1, length(y))]

                gradient <- self$compute_gradient(batch_X, batch_y)
                self$coefficients <- self$coefficients - self$learning_rate * gradient
              }
              cost <- self$cost_function(X, y)

              if (iter > 1 && abs(prev_cost - cost) < self$tolerance) {
                cat("Convergence atteinte à l'itération", iter, "\n")
                break
              }
              prev_cost <- cost
            }
          },

          fit = function(X, y) {
            self$n_famd_dim <- self$preprocessor$n_famd_dim
            X_processed <- self$preprocessor$fit_transform(X)
            X_processed <- as.matrix(X_processed)
            self$n_features <- ncol(X_processed)
            self$n_classes <- length(unique(y))
            self$coefficients <- matrix(0, self$n_features, self$n_classes)

            if (self$solver == 'lbfgs') {
              cat("Utilisation de LBFGS pour l'optimisation...\n")
              self$lbfgs_optimizer(X_processed, y)
            } else if (self$solver == 'sag') {
              cat("Utilisation de SAG pour l'optimisation...\n")
              self$sag_optimizer(X_processed, y)
            } else if (self$solver == 'saga') {
              cat("Utilisation de SAGA pour l'optimisation...\n")
              self$saga_optimizer(X_processed, y)
            } else if (self$solver == 'newton-cg') {
              cat("Utilisation de Newton-CG pour l'optimisation...\n")
              self$newton_cg_optimizer(X_processed, y)
            } else {
              stop("Solveur non reconnu.")
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
              cat("Modèle Régression Logistique Multinomiale\n")
              cat("Caractéristiques:", self$n_features, "| Classes:", self$n_classes, "\n")
              cat("Alpha:", self$alpha, "| L1_ratio:", self$l1_ratio, "\n")
              cat("Solver:", self$solver, "\n")
              cat("Iterations:", self$max_iter, "\n")
              cat("Coefficients:\n")
              print(self$coefficients)
            }
          }
        )
)
