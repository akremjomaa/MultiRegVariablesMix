library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

# UI Module
preprocessModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = 12,
      # Tab 1: Outlier Detection and Removal
      tabPanel(
        "Gestion des outliers",
        fluidRow(
          box(
            width = 4,
            title = "Paramètres IQR",
            sliderInput(
              ns("iqr_range"),
              "Intervalle Interquartile (IQR)",
              min = 1.0,
              max = 3.0,
              value = 1.5,
              step = 0.1
            ),
            selectInput(
              ns("num_vars"),
              "Variables numériques à traiter",
              choices = NULL,
              multiple = TRUE
            ),
            actionButton(
              ns("remove_outliers"),
              "Supprimer les outliers",
              class = "btn-primary"
            )
          ),
          box(
            width = 8,
            title = "Aperçu des outliers",
            plotOutput(ns("outliers_plot")),
            DTOutput(ns("outliers_table"))
          )
        )
      ),
      
      # Tab 2: Missing Values Handling
      tabPanel(
        "Gestion des valeurs manquantes",
        fluidRow(
          box(
            width = 4,
            title = "Méthodes d'imputation",
            selectInput(
              ns("num_method"),
              "Méthode pour variables numériques",
              choices = c(
                "Moyenne" = "mean",
                "Médiane" = "median",
                "Plus proche voisin" = "knn"
              )
            ),
            selectInput(
              ns("cat_method"),
              "Méthode pour variables catégorielles",
              choices = c(
                "Mode" = "mode",
                "Nouvelle catégorie" = "new_category"
              )
            ),
            actionButton(
              ns("impute_missing"),
              "Appliquer l'imputation",
              class = "btn-primary"
            )
          ),
          box(
            width = 8,
            title = "Aperçu des valeurs manquantes",
            plotOutput(ns("missing_plot")),
            DTOutput(ns("missing_table"))
          )
        )
      )
    )
  )
}

 

preprocessModuleServer <- function(input, output, session, shared_data) {
  ns <- session$ns
  
  # Reactive data access
  data <- reactive({
    req(shared_data())
    if(is.list(shared_data()) && !is.data.frame(shared_data())) {
      shared_data()[[1]]
    } else {
      shared_data()
    }
  })
  
  # Update numeric variables choices
  observe({
    req(data())
    num_vars <- names(data())[sapply(data(), is.numeric)]
    updateSelectInput(session, "num_vars", 
                     choices = num_vars,
                     selected = num_vars[1])
  })
  
  # Outlier detection function
  detect_outliers <- function(x, iqr_range) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - iqr_range * iqr
    upper_bound <- q3 + iqr_range * iqr
    x < lower_bound | x > upper_bound
  }
  
  # Handle outlier removal with progress
  observeEvent(input$remove_outliers, {
    req(data(), input$num_vars, input$iqr_range)
    
    withProgress(message = 'Suppression des outliers...', {
      df <- data()
      rows_before <- nrow(df)
      
      # Process each selected variable
      for (var in input$num_vars) {
        incProgress(1/length(input$num_vars), 
                   detail = paste("Traitement de", var))
        
        outliers <- detect_outliers(df[[var]], input$iqr_range)
        df <- df[!outliers, ]
      }
      
      # Update shared data
      shared_data(df)
      
      # Show results
      rows_removed <- rows_before - nrow(df)
      showNotification(
        paste(rows_removed, "observations supprimées"),
        type = "message"
      )
    })
  })
  
  # Missing values imputation
  observeEvent(input$impute_missing, {
    req(data(), input$num_method, input$cat_method)
    
    withProgress(message = 'Imputation des valeurs manquantes...', {
      df <- data()
      
      # Numeric variables
      num_vars <- names(df)[sapply(df, is.numeric)]
      for (var in num_vars) {
        if (any(is.na(df[[var]]))) {
          if (input$num_method == "mean") {
            df[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm = TRUE)
          } else if (input$num_method == "median") {
            df[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm = TRUE)
          }
        }
      }
      
      # Categorical variables
      cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      for (var in cat_vars) {
        if (any(is.na(df[[var]]))) {
          if (input$cat_method == "mode") {
            mode_val <- names(sort(table(df[[var]]), decreasing = TRUE))[1]
            df[[var]][is.na(df[[var]])] <- mode_val
          } else if (input$cat_method == "new_category") {
            df[[var]] <- factor(df[[var]], levels = c(levels(df[[var]]), "Missing"))
            df[[var]][is.na(df[[var]])] <- "Missing"
          }
        }
      }
      
      # Update shared data
      shared_data(df)
      showNotification("Valeurs manquantes imputées avec succès", type = "message")
    })
  })
  
  # Return reactive data for chain updates
  return(reactive({ data() }))
}