
# MultiRegVariablesMix

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/akremjomaa/MultiRegVariablesMix)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/built%20with-R-blue.svg)](https://www.r-project.org/)

**MultiRegVariablesMix** est un package R conçu pour effectuer des régressions logistiques multinomiales avec des variables mixtes. Ce package offre une intégration fluide entre le prétraitement des données, l'entraînement d'un modèle de régression logistique, et des outils d'évaluation visuelle.

---

## 🚀 Fonctionnalités principales

- **Régression logistique multinomiale** adaptée aux variables numériques et catégoriques.
- **Prétraitement intégré** :
  - Label encoding.
  - One-hot encoding.
  - Analyse Factorielle des Données Mixtes (AFDM) pour les variables mixtes.
- **Optimisation** avec prise en charge de :
  - Gradient Descent.
  - Adam.
  - RMSprop.
  - Momentum.
- **Visualisations** :
  - Courbes d'évolution du coût.
  - Matrice de confusion.
  - Importance des variables.
  - Courbes ROC multiclasses.
  - Distribution des prédictions.

---

## 📦 Installation

Pour installer la dernière version de **MultiRegVariablesMix** depuis GitHub :

```
# Installez remotes si nécessaire
install.packages("remotes")

# Installez le package depuis GitHub
remotes::install_github("akremjomaa/MultiRegVariablesMix")
```

---

## 🔧 Guide d'utilisation

Voici un guide étape par étape pour utiliser **MultiRegVariablesMix**.

### 1. Chargement du package

```
library(MultiRegVariablesMix)
```

---

### 2. Préparation des données

Créez un jeu de données d'entraînement et de test :

```
set.seed(42)

# Jeu de données synthétique
X <- data.frame(
  num1 = rnorm(100),  # Variable numérique
  num2 = runif(100, 1, 10),  # Variable numérique
  cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),  # Variable catégorique
  cat2 = sample(c("X", "Y"), 100, replace = TRUE)  # Variable catégorique
)
y <- sample(1:3, 100, replace = TRUE)  # Variable cible (classes)

# Division en jeu d'entraînement et de test
train_idx <- sample(seq_len(nrow(X)), size = 70)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]
```

---

### 3. Initialisation du modèle

Instanciez la classe principale `MultinomialRegression` avec les hyperparamètres souhaités.

```
model <- MultinomialRegression$new(
  optimizer_type = "adam",    # Optimiseur
  encoding_type = "afdm",     # Type de prétraitement
  learning_rate = 0.001,      # Taux d'apprentissage
  epochs = 10000,             # Nombre d'époques
  beta1 = 0.9,                # Paramètre pour Adam
  epsilon = 1e-4              # Stabilité numérique
)
```

---

### 4. Exploration des données avec AFDM

Explorez la variance expliquée par les dimensions d'AFDM pour déterminer le nombre de composantes à conserver :

```
eig_values <- model$explore_famd(X_train)
ncp <- 3 # Conserver 3 composantes principales
# Si aucun ncp n'est précisé, le package conserve automatiquement les composantes représentant 80 % de la variance cumulative.
```

---

### 5. Prétraitement des données

Appliquez le prétraitement avec le modèle :

```
X_train_processed <- model$preprocess(X_train, y_train, is_training = TRUE, ncp = ncp)
# Si le paramètre ncp est omis, le package conservera par défaut les composantes expliquant 80 % de la variance.
```

---

### 6. Entraînement du modèle

Entraînez le modèle sur les données prétraitées.

```
model$fit()
# Par défaut, utilise les données prétraitées. Si vous voulez passer vos propres données, spécifiez `X` et `y` en mettant le paramètre 'preprocess' à FALSE.
```

---

### 7. Évaluation des performances

#### a. Courbe d'évolution du coût

```
model$plot_cost_evolution()
```
 
 
#### b. Importance des variables


```
importance <- model$var_importance()
print(importance)
```


#### c. Prédictions et probabilités


```
predictions <- model$predict(X_test)
probas <- model$predict_proba(X_test)
```


#### d. Matrice de confusion


```
model$plot_confusion_matrix(y_test, predictions)
```


#### e. Courbes ROC multiclasses


```
model$plot_roc(X_test, y_test)
```


#### f. Distribution des prédictions


```
model$plot_prediction_distribution(X_test, y_true = y_test)
```

---

### 8. Résumé du modèle

Affichez les informations globales et détaillées sur le modèle.

```
# Vue d'ensemble
model$print()
```

```
# Résumé complet
model$summary()
```

---

## 🛠️ Dépendances

Ce package dépend des bibliothèques suivantes (installées automatiquement lors de l'installation) :

- `R6`
- `ggplot2`
- `stats`
- `FactoMineR`
- `pROC`

---

## 🐞 Rapport de bugs

Pour signaler un problème ou proposer une amélioration, rendez-vous sur :  
[BugReports](https://github.com/akremjomaa/MultiRegVariablesMix/issues)

---

## 📄 Licence

Ce package est distribué sous la licence MIT. Consultez le fichier [LICENSE](https://github.com/akremjomaa/MultiRegVariablesMix/blob/master/LICENSE) pour plus d'informations.

---

## 👥 Auteurs

- **Akrem Jomaa** – [akrem.jomaa@univ-lyon2.fr](mailto:akrem.jomaa@univ-lyon2.fr)
- **Edina Adjaro Patoussi** – [e.adjaro-patoussi@univ-lyon2.fr](mailto:e.adjaro-patoussi@univ-lyon2.fr)
- **Joel Sollari** – [joel.sollari@univ-lyon2.fr](mailto:joel.sollari@univ-lyon2.fr)

---

