library(shiny)
library(shinydashboard)
library(DT)
library(FactoMineR)  # For FAMD
library(caret)       # For dummyVars
library(nnet) 

# Charger les modules
source("ui.R")
source("server.R")


# Créer l'application
shinyApp(ui = ui, server = server)
