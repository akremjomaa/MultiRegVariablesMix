preprocess <- function(X) {
  # Imputation des valeurs manquantes
  for (var in colnames(X)) {
    if (is.numeric(X[[var]])) {
      X[[var]][is.na(X[[var]])] <- mean(X[[var]], na.rm = TRUE)
    } else {
      X[[var]][is.na(X[[var]])] <- as.character(stats::mode(X[[var]]))
    }
  }

  # Application de l'encodage ou AFDM en fonction de self$encoding_method
  if (self$encoding_method == "one_hot") {
    # Encodage One-Hot
    for (var in names(X)) {
      if (is.factor(X[[var]])) {
        X <- cbind(X, model.matrix(~ X[[var]] - 1))
        X[[var]] <- NULL
      }
    }
  } else if (self$encoding_method == "famd") {
    famd_result <- FAMD(X, ncp = self$n_famd_dim, graph = FALSE)
    X <- as.data.frame(famd_result$ind$coord[, 1:self$n_famd_dim])
    cat("AFDM applied, retaining", self$n_famd_dim, "components.\n")
  } else {
    stop("Méthode d'encodage non reconnue. Choisissez 'one_hot' ou 'famd'.")
  }

  return(scale(X))  # Normalisation
}


# Méthode d'exploration de l'AFDM pour aider à choisir le nombre de composantes
explore_famd <- function(X) {
  # Exécuter l'AFDM sans réduction de dimensions
  famd_result <- FAMD(X, graph = FALSE)

  # Afficher le graphique de la variance expliquée
  eig.val <- famd_result$eig
  barplot(eig.val[, 2],
          names.arg = 1:nrow(eig.val),
          main = "Variances Explained by Dimensions (%)",
          xlab = "Principal Dimensions",
          ylab = "Percentage of variances",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(eig.val), eig.val[, 2],
        type = "b", pch = 19, col = "red")

  # Message pour guider l'utilisateur
  cat("Analyse exploratoire de l'AFDM terminée. \n")
  cat("Veuillez examiner le graphique et fixer 'n_famd_dim' pour conserver le nombre de composantes souhaité.\n")
}
