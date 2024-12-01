# MultiRegVariablesMixClient

**Link** : [MultiRegVariablesMixClient](https://edadjaro.shinyapps.io/MultiRegVariablesMixClient/)

## Description  
**MultiRegVariablesMixClient** est une application **Shiny** conçue pour effectuer des régressions logistiques multinomiales sur des données mixtes. Elle fournit un environnement interactif permettant de charger, explorer et prétraiter des données, d'entraîner des modèles, et de réaliser des prédictions.

## Fonctionnalités  
- **Chargement de fichiers** : Importation de fichiers de données au format CSV ou Excel.  
- **Exploration des données** : Visualisation des structures et des statistiques descriptives des données.  
- **Prétraitement** : Gestion des valeurs manquantes, normalisation et mise à l'échelle des données.  
- **Modélisation** : Entraînement d'un modèle de régression logistique multinomiale.  
- **Évaluation du modèle** : Analyse des performances à l'aide de matrices de confusion et de mesures comme la précision.  
- **Exportation** : Sauvegarde des modèles pour une utilisation ultérieure.

## Installation  
Pour installer les dépendances nécessaires, exécutez les commandes suivantes dans R :

```bash
git clone -b interfaceEdina https://github.com/akremjomaa/MultiRegVariablesMix.git
```

```r
install.packages(c("shiny", "shinydashboard", "DT", "FactoMineR", "caret", "nnet", "ggplot2", "readxl", 'plotly','devtools' ,'writexl'))
```

## Architecture du projet  
Le projet est organisé comme suit :

```
MultiRegVariablesMixClient/
├── modules/
│   ├── accueil_module.R             # Interface utilisateur pour la page d'accueil
│   ├── fileLoader_module.R          # Chargement des fichiers de données
│   ├── exploration_module.R         # Exploration des données
│   ├── preprocess_module.R          # Prétraitement des données
│   ├── fit_module.R                 # Entraînement et évaluation du modèle
├── server.R                         # Code serveur pour la logique de l'application
├── ui.R                             # Interface utilisateur pour Shiny
├── app.R                            # Application Shiny
├── README.md                        # Documentation du projet
```

## Utilisation  
Pour lancer l'application Shiny, exécutez le code suivant dans R :

```r
runApp("path/to/MultiRegVariablesMixClient")
```

### Détails des modules  
- **`accueil_module.R`** : Contient l'interface utilisateur pour la page d'accueil.  
- **`fileLoader_module.R`** : Gère le chargement des fichiers de données dans l'application.  
- **`exploration_module.R`** : Fournit des outils pour visualiser et explorer les données.  
- **`fit_module.R`** : Entraînement et évaluation du modèle de régression logistique multinomiale.  
- **`predict_module.R`** : Prédiction de nouvelles observations.

### Fichiers principaux  
- **`server.R`** : Code serveur gérant la logique de l'application.  
- **`ui.R`** : Code de l'interface utilisateur.  
- **`app.R`** : Application Shiny principale.  
- **`README.md`** : Description du projet et guide d'utilisation (ce fichier).

## Contributeurs  
Pour toute question ou suggestion, veuillez contacter :  
**Email** :  
- Edina Adjaro Patoussi – e.adjaro-patoussi@univ-lyon2.fr  
- Akrem Jomaa – akrem.jomaa@univ-lyon2.fr  
- Joel Sollari – joel.sollari@univ-lyon2.fr  
