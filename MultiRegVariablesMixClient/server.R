library(shiny)
library(ggplot2)
library(readxl)
library(DT)

# Modules
source("modules/accueil_module.R")
source("modules/fileLoader_module.R") 
source("modules/exploration_module.R")
source("modules/preprocess_module.R")
source("modules/fit_module.R")



# server.R
server <- function(input, output, session) {
  # Initialize reactive values
  shared_data <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  
  # File Loader Module
  moduleServer(
    id = "fileLoader",
    module = function(input, output, session) {
      data <- fileLoaderModuleServer(input, output, session, shared_data)
      
      # Watch for data changes
      observeEvent(data(), {
        req(data())
        shared_data(data())
        showNotification("Données chargées avec succès", type = "message")
      })
    }
  )
  
  # Exploration Module
  moduleServer(
    id = "exploration",
    module = function(input, output, session) {
      explorationModuleServer(input, output, session, reactive({ shared_data() }))
    }
  )
  
  # Preprocessing Module
  moduleServer(
    id = "preprocess",
    module = function(input, output, session) {
      result <- preprocessModuleServer(input, output, session, reactive({ shared_data() }))
      
      # Update shared data after preprocessing
      observeEvent(result(), {
        req(result())
        shared_data(result())
        showNotification("Prétraitement appliqué avec succès", type = "message")
      })
    }
  )
  
  # Model Fitting Module
  moduleServer(
    id = "fit",
    module = function(input, output, session) {
      result <- fitModuleServer(input, output, session, reactive({ shared_data() }))
      
      # Update model after fitting
      observeEvent(result(), {
        req(result())
        model(result())
        showNotification("Modèle entraîné avec succès", type = "message")
      })
    }
  )
  
  # Prediction Module
  moduleServer(
    id = "predict",
    module = function(input, output, session) {
      predictModuleServer(
        input, 
        output, 
        session,
        reactive({ shared_data() }),
        reactive({ model() })
      )
    }
  )
  
  # Data flow monitoring
  observe({
    # Monitor data state
    if (is.null(shared_data())) {
      showNotification(
        "En attente des données...",
        type = "warning",
        duration = NULL,
        id = "data_status"
      )
    } else {
      removeNotification(id = "data_status")
    }
    
    # Monitor model state
    if (!is.null(shared_data()) && is.null(model())) {
      showNotification(
        "Données prêtes - Vous pouvez entraîner le modèle",
        type = "message",
        duration = NULL,
        id = "model_status"
      )
    } else if (!is.null(model())) {
      removeNotification(id = "model_status")
    }
  })
  
  # Return reactive values for debugging
  list(
    data = shared_data,
    model = model
  )
}