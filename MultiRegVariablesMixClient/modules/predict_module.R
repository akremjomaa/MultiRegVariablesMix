library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(writexl)
library(ggplot2)

# UI Module
predictModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = 12,
      # Tab 1: Individual prediction
      tabPanel(
        "Prédiction individuelle",
        fluidRow(
          box(
            width = 6,
            title = "Entrée des données",
            uiOutput(ns("input_fields")),
            actionButton(
              ns("predict_single"),
              "Prédire",
              class = "btn-primary"
            )
          ),
          box(
            width = 6,
            title = "Résultat de la prédiction",
            verbatimTextOutput(ns("single_prediction_result")),
            plotOutput(ns("prediction_proba_plot"))
          )
        )
      ),
      
      # Tab 2: Batch prediction
      tabPanel(
        "Prédictions multiples",
        fluidRow(
          box(
            width = 6,
            title = "Import des données",
            fileInput(
              ns("pred_file"),
              "Fichier CSV ou Excel",
              accept = c(".csv", ".xlsx", ".xls")
            ),
            selectInput(
              ns("file_type"),
              "Type de fichier",
              choices = c(
                "CSV" = "csv",
                "Excel" = "excel"
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'csv'", ns("file_type")),
              selectInput(
                ns("separator"),
                "Séparateur",
                choices = c(
                  "Virgule" = ",",
                  "Point-virgule" = ";",
                  "Tabulation" = "\t"
                )
              ),
              checkboxInput(ns("header"), "Première ligne contient les en-têtes", value = TRUE)
            ),
            actionButton(
              ns("predict_batch"),
              "Lancer les prédictions",
              class = "btn-primary"
            )
          ),
          box(
            width = 6,
            title = "Export",
            selectInput(
              ns("export_format"),
              "Format d'export",
              choices = c("CSV" = "csv", "Excel" = "xlsx")
            ),
            downloadButton(
              ns("download_predictions"),
              "Télécharger les prédictions"
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Résultats",
            DTOutput(ns("predictions_table"))
          )
        )
      )
    )
  )
}

# Server Module
predictModuleServer <- function(input, output, session, shared_data, model) {
  ns <- session$ns
  
  # Get feature names excluding target variable
  feature_names <- reactive({
    req(shared_data(), model())
    setdiff(names(shared_data()), model()$target_var)
  })
  
  # Create dynamic input fields
  output$input_fields <- renderUI({
    req(feature_names())
    
    lapply(feature_names(), function(var) {
      if (is.numeric(shared_data()[[var]])) {
        numericInput(
          ns(paste0("input_", var)),
          label = var,
          value = mean(shared_data()[[var]], na.rm = TRUE)
        )
      } else if (is.factor(shared_data()[[var]]) || is.character(shared_data()[[var]])) {
        selectInput(
          ns(paste0("input_", var)),
          label = var,
          choices = unique(shared_data()[[var]]),
          selected = unique(shared_data()[[var]])[1]
        )
      }
    })
  })
  
  # Handle individual prediction
  observeEvent(input$predict_single, {
    req(model(), feature_names())
    
    # Collect input values
    input_data <- data.frame(
      lapply(feature_names(), function(var) {
        input_val <- input[[paste0("input_", var)]]
        if (is.factor(shared_data()[[var]])) {
          factor(input_val, levels = levels(shared_data()[[var]]))
        } else if (is.numeric(shared_data()[[var]])) {
          as.numeric(input_val)
        } else {
          input_val
        }
      })
    )
    names(input_data) <- feature_names()
    
    # Make prediction
    tryCatch({
      pred = model()$model_instance$predict(input_data)
      probs = model()$model_instance$predict_proba(input_data)
      # pred <- predict(model()$model_instance, newdata = input_data)
      # probs <- predict(model()$model_instance, newdata = input_data)
      
      output$single_prediction_result <- renderText({
        paste("Prédiction:", pred)
      })
      
      output$prediction_proba_plot <- renderPlot({
        barplot(as.numeric(probs),
                names.arg = levels(model()$y),
                main = "Probabilités par classe",
                col = "steelblue",
                las = 2)
      })
      
    }, error = function(e) {
      showNotification(
        paste("Erreur de prédiction"),
        type = "error",
        duration = 2
      )
    })
  })
  
  # Handle batch predictions
  predictions <- reactiveVal(NULL)
  
  observeEvent(input$predict_batch, {
    req(input$pred_file, model())
    
    withProgress(message = 'Prédictions en cours...', {
      # Read file
      df <- if (input$file_type == "csv") {
        read.csv(
          input$pred_file$datapath,
          sep = input$separator,
          header = input$header
        )
      } else {
        read_excel(input$pred_file$datapath)
      }
      
      # Validate columns
      missing_cols <- setdiff(feature_names(), names(df))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Colonnes manquantes"),
          type = "error",
          duration = 2
        )
        return(NULL)
      }
      
      # Make predictions
      tryCatch({
      pred = model()$model_instance$predict(input_data)
      probs = model()$model_instance$predict_proba(input_data)


        # preds <- predict(model()$model_instance$predict, newdata = df, type = "class")
        # probs <- predict(model()$model_instance$predict, newdata = df, type = "probs")
        
        # Combine predictions with original data
        result <- cbind(
          df,
          Prediction = preds,
          as.data.frame(probs)
        )
        
        predictions(result)
        
      }, error = function(e) {
        showNotification(
          paste("Erreur lors des prédictions"),
          type = "error",
          duration = 2
        )
      })
    })
  })
  
  # Display predictions table
  output$predictions_table <- renderDT({
    req(predictions())
    datatable(
      predictions(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )
  })
  
  # Download handler
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste0(
        "predictions_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".",
        input$export_format
      )
    },
    content = function(file) {
      if (input$export_format == "csv") {
        write.csv(predictions(), file, row.names = FALSE)
      } else {
        write_xlsx(predictions(), file)
      }
    }
  )
  
  return(predictions)
}