
# MultiRegVariablesMix

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/akremjomaa/MultiRegVariablesMix)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/built%20with-R-blue.svg)](https://www.r-project.org/)

**MultiRegVariablesMix** is an R package designed for performing multinomial logistic regression with mixed variables. It provides a seamless integration of data preprocessing, logistic regression modeling, and visual performance evaluation tools.

---

## 🚀 Key Features

- **Multinomial Logistic Regression** for numerical and categorical variables.
- **Integrated Preprocessing**:
  - Label encoding.
  - One-hot encoding.
  - Factorial Analysis for Mixed Data (FAMD) for mixed variables.
- **Optimization supporting**:
  - Gradient Descent.
  - Adam.
  - RMSprop.
  - Momentum.
- **Visualizations**:
  - Cost evolution curves.
  - Confusion matrices.
  - Variable importance.
  - Multi-class ROC curves.
  - Prediction distributions.

---

## 📦 Installation

To install the latest version of **MultiRegVariablesMix** from GitHub:

```
# Install remotes if needed
install.packages("remotes")

# Install the package from GitHub
remotes::install_github("https://github.com/akremjomaa/MultiRegVariablesMix.git")
```

---

## 🔧 Usage Guide

Here is a step-by-step guide to using **MultiRegVariablesMix**.

### 1. Load the package

```
library(MultiRegVariablesMix)
```

---

### 2. Prepare your data

Create a training and testing dataset:

```
set.seed(42)

# Synthetic dataset
X <- data.frame(
  num1 = rnorm(100),  # Numerical variable
  num2 = runif(100, 1, 10),  # Numerical variable
  cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),  # Categorical variable
  cat2 = sample(c("X", "Y"), 100, replace = TRUE)  # Categorical variable
)
y <- sample(1:3, 100, replace = TRUE)  # Target variable (classes)

# Split into training and testing sets
train_idx <- sample(seq_len(nrow(X)), size = 70)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]
```

---

### 3. Initialize the model

Instantiate the main `MultinomialRegression` class with the desired hyperparameters:

```
model <- MultinomialRegression$new(
  optimizer_type = "adam",    # Optimizer
  encoding_type = "afdm",     # Preprocessing method
  learning_rate = 0.001,      # Learning rate
  epochs = 10000,             # Number of epochs
  beta1 = 0.9,                # Parameter for Adam
  epsilon = 1e-4              # Numerical stability
)
```

---

### 4. Data exploration with FAMD

Explore the variance explained by FAMD dimensions to determine the number of principal components to retain:

```
eig_values <- model$explore_famd(X_train)
ncp <- 3 # Retain 3 principal components
# If ncp is not specified, the package automatically retains components explaining 80% of the cumulative variance.
```

---

### 5. Data Preprocessing

Preprocess the data with the model:

```
X_train_processed <- model$preprocess(X_train, y_train, is_training = TRUE, ncp = ncp)
# If ncp is omitted, the package defaults to retaining components explaining 80% variance.
```

---

### 6. Train the model

Train the model on the preprocessed data:

```
model$fit()
# By default, it uses preprocessed data. If you want to use your own data, specify `X` and `y` while setting `preprocess` to FALSE.
```

---

### 7. Performance Evaluation

#### a. Cost Evolution Curve

```
model$plot_cost_evolution()
```

#### b. Variable Importance

```
importance <- model$var_importance()
print(importance)
```

#### c. Predictions and Probabilities

```
predictions <- model$predict(X_test)
probas <- model$predict_proba(X_test)
```

#### d. Confusion Matrix

```
model$plot_confusion_matrix(y_test, predictions)
```

#### e. Multi-class ROC Curves

```
model$plot_roc(X_test, y_test)
```

#### f. Prediction Distribution

```
model$plot_prediction_distribution(X_test, y_true = y_test)
```

---

### 8. Model Summary

Display global and detailed information about the model:

```
# Overview
model$print()
```

```
# Full summary
model$summary()
```

---

## 🛠️ Dependencies

This package uses R version 4.4.1 and depends on the following libraries (automatically installed during installation):

- `R6_2.5.1`
- `ggplot2_3.5.1`
- `stats`
- `FactoMineR_2.11`
- `pROC_1.18.5`

---

## 🐞 Bug Reports

To report an issue or suggest an improvement, visit:  
[BugReports](https://github.com/akremjomaa/MultiRegVariablesMix/issues)

---

## 📄 License

This package is distributed under the MIT License. See the [LICENSE](https://github.com/akremjomaa/MultiRegVariablesMix/blob/master/LICENSE.md) file for details.

---

## 👥 Authors

- **Akrem Jomaa** – [akrem.jomaa@univ-lyon2.fr](mailto:akrem.jomaa@univ-lyon2.fr)
- **Edina Adjaro Patoussi** – [e.adjaro-patoussi@univ-lyon2.fr](mailto:e.adjaro-patoussi@univ-lyon2.fr)
- **Joel Sollari** – [joel.sollari@univ-lyon2.fr](mailto:joel.sollari@univ-lyon2.fr)

---
