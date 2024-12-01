library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)

plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)




# # Modules
# source("modules/data_table_module.R")
# source("modules/summary_module.R")
# source("modules/histogram_module.R")
# source("modules/scatterplot_module.R")
source("modules/accueil_module.R")
source("modules/fileLoader_module.R")
source("modules/exploration_module.R")
source("modules/preprocess_module.R")
source("modules/fit_module.R")
source("modules/predict_module.R")


# Interface utilisateur
ui <- dashboardPage(
  title = "Regression Logistique Multinomiale",

  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,

    # Header ----
     dashboardHeader(
      status = "lime",
      title = dashboardBrand(
        title = "Regression Logistique Multinomiale",
        color = "olive",
      ),
      controlbarIcon = icon("circle-info"),
      fixed = TRUE

    ),

  # Barre latérale avec menu de navigation

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarMenuid",

      menuItem(
        "Accueil",
        tabName = "home",
        icon = icon("home"),
        selected = TRUE
      ),
      menuItem(
        "Import des données",
        tabName = "Lecture_des_donnees",
        icon = icon("file-import")
      ),
      menuItem(
        "Exploration des données",
        tabName = "exploration",
        icon = icon("chart-bar")
      ),
      # menuItem(
      #   "Prétraitement",
      #   tabName = "preprocess",
      #   icon = icon("broom")
      # ),
      menuItem(
        "Configuration du modèle",
        tabName = "fit",
        icon = icon("cogs")
      ),
      menuItem(
        "Prédiction",
        tabName = "predict",
        icon = icon("chart-line")
      )
      # menuItem(
      #   "Graphiques",
      #   tabName = "graph",
      #   icon = icon("chart-pie")
      # ),
      # menuItem(
      #   "Aide",
      #   tabName = "help",
      #   icon = icon("question-circle")
      # )
    )
  ),

  dashboardBody(
    # Onglet pour Accueil
    tabItems(
      tabItem(
        tabName = "home",
        AccueilModuleUI("Accueil")
      #fileLoaderModuleUI("fileLoader")

      ),
        # Onglet Logistique Fit
    tabItem(
      tabName = "Lecture_des_donnees",
      fileLoaderModuleUI("fileLoader")
      # explorationModuleUI("exploration")
      # h2("Logistique Fit"),  # Contenu placeholder
    ),
        # Onglet Logistique Fit
    tabItem(
      tabName = "exploration",
      # fileLoaderModuleUI("fileLoader")
      explorationModuleUI("exploration")
      # h2("Logistique Fit"),  # Contenu placeholder
    ),

    tabItem(
      tabName = "preprocess",
      # UI Module
      preprocessModuleUI("preprocess")
      # h2("Logistique Fit"),  # Contenu placeholder
    ),

    # # Onglet pour Lecture des données
    #   tabItem(
    #     tabName = "Lecture_des_données",

    #     fluidRow(
    #       # Autres fonctionnalités comme un résumé ou des graphiques
    #       tabBox(
    #         title = "Exploration des données",
    #         id = "exploration_tabs",
    #         width = 12,
    #         tabPanel(
    #           "Résumé statistique",
    #           # moduleUI("summary_module")
    #         ),
    #         tabPanel(
    #           "Histogramme",
    #           # moduleUI("histogram_module")
    #         ),
    #         tabPanel(
    #           "Nuage de points",
    #           # moduleUI("scatterplot_module")
    #         )
    #     )

    #   )
    # ),

    # Onglet Logistique Fit
    tabItem(
      tabName = "fit",
      # h2("Logistique Fit"),  # Contenu placeholder
      # UI Module
      fitModuleUI ("fit")
      # fileLoaderModuleUI("fileLoader")
    ),

    # Onglet Logistique Predict
    tabItem(
      tabName = "predict",
      predictModuleUI("predict"),

      h2("Logistique Predict")  # Contenu placeholder
    ),

    # Onglet Graphique
    tabItem(
      tabName = "graph",
      h2("Graphique")  # Contenu placeholder
    )

    )
  )
)
