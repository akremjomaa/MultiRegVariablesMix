library(shiny)
library(shinydashboard)
library(DT)
library(FactoMineR)
library(devtools)
library(caret)


# library(MultiRegVariablesMix)
load_all("MultiRegVariablesMix")

# UI Module
fitModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = 12,
      # Tab 1: Configuration
      tabPanel(
        "Configuration du modèle",
        fluidRow(
          # Target variable selection
          box(
            width = 6,
            title = "Variable cible",
            selectInput(
              ns("target_var"),
              "Variable cible (catégorielle)",
              choices = NULL
            ),
            sliderInput(
              ns("train_ratio"),
              "Proportion Train/Test",
              min = 0.5,
              max = 0.9,
              value = 0.7,
              step = 0.1
            )
          ),
          # Preprocessing options
          box(
            width = 6,
            title = "Prétraitement",
            selectInput(
              ns("na_numeric"),
              "Valeurs manquantes (numériques)",
              choices = c(
                "Moyenne" = "mean",
                "Médiane" = "median"
              )
            ),
            selectInput(
              ns("na_categorical"),
              "Valeurs manquantes (catégorielles)",
              choices = c(    
                "Mode" = "mode"
              )
            ),
            selectInput(
              ns("scaling"),
              "Mise à l'échelle (numériques)",
              choices = c(
                "Aucun" = "none",
                "Standardisation (moyenne=0, écart-type=1)" = "standard",
                "Normalisation (min=0, max=1)" = "normalize"
              )
            ),
            selectInput(
              ns("encoding"),
              "Encodage (catégorielles)",
              choices = c(
                "Label Encoding" = "label",
                "One-Hot Encoding" = "one-hot",
                "AFDM" = "afdm"
              )
            )
          )
        ),
        fluidRow(
          # Model training options
          box(
            width = 6,
            title = "Options d'entraînement",
            selectInput(
              ns("optimizer_type"),
              "Type d'optimiseur",
              choices = c("adam", "momentum", "rmsprop", "gd"),
              selected = "adam"
            ),
            numericInput(
              ns("learning_rate"),
              "Taux d'apprentissage",
              value = 0.01,
              min = 0.0001,
              max = 1,
              step = 0.0001
            ),
            numericInput(
              ns("epochs"),
              "Nombre d'époques",
              value = 100,
              min = 1,
              max = 1000,
              step = 1
            ),
            numericInput(
              ns("batch_size"),
              "Taille du lot",
              value = 32,
              min = 1,
              max = 512,
              step = 1
            )
          ),
          box(
            width = 6,
            title = "Encodage et entraînement",
            actionButton(
              ns("fit_model"), 
              "Entraîner le modèle",
              class = "btn-primary"
            )
          )
        )
      ),
      
      # Tab 2: Results
      tabPanel(
        "Résumé du modèle",
        box(
          width = 12,
          title = "Coefficients et statistiques",
          verbatimTextOutput(ns("model_summary")),
          DTOutput(ns("coef_table"))
        )
      ),
      
      # Tab 3: Variable Importance
      tabPanel(
        "Importance des variables",
        fluidRow(
          box(
            width = 12,
            plotOutput(ns("var_importance_plot")),
            DTOutput(ns("var_importance_table")),
            plotOutput(ns("famd_scree_plot")),
            DTOutput(ns("famd_contributions")),
            plotOutput(ns("famd_variables_plot"))
          )
        )
      ),
      
      # Tab 4: Export
      tabPanel(
        "Export du modèle",
        box(
          width = 12,
          title = "Sauvegarder le modèle",
          textInput(
            ns("model_name"),
            "Nom du modèle",
            value = paste0("model_", format(Sys.time(), "%Y%m%d_%H%M%S"))
          ),
          downloadButton(
            ns("download_model"),
            "Télécharger le modèle"
          ),
          verbatimTextOutput(ns("model_info"))
        )
      )
    )
  )
}


# Server Module
fitModuleServer <- function(input, output, session, shared_data) {
  ns <- session$ns
  
  # Initialize model reactive value
  model <- reactiveVal(NULL)
  
  # Data access with validation
  data <- reactive({
    req(shared_data())
    validate(need(!is.null(shared_data()), "Aucune donnée n'a été chargée"))
    
    df <- if(is.list(shared_data()) && !is.data.frame(shared_data())) {
      shared_data()[[1]]
    } else {
      shared_data()
    }
    
    validate(need(nrow(df) > 0, "Le jeu de données est vide"))
    return(df)
  })
  
  # Update target variable choices
  observe({
    req(data())
    df <- data()
    
    # Get categorical variables
    cat_vars <- names(df)[sapply(df, function(x) {
      if(is.factor(x) || is.character(x)) {
        n_levels <- length(unique(na.omit(x)))
        return(n_levels > 2 && n_levels < nrow(df)/2)   
      }
      return(FALSE)
    })]
    
    validate(need(length(cat_vars) > 0, "Aucune variable catégorielle valide trouvée"))
    
    updateSelectInput(session, "target_var", 
                     choices = cat_vars,
                     selected = if(length(cat_vars) > 0) cat_vars[1] else NULL)
  })
  

        # Enhanced preprocessing function
  preprocess_data <- function(df, input) {
          # Missing value handling
          numeric_vars <- names(df)[sapply(df, is.numeric)]
          categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
          
          # Numeric missing value imputation
          df[numeric_vars] <- lapply(df[numeric_vars], function(x) {
            switch(input$na_numeric,
                  "mean" = {
                    x[is.na(x)] <- mean(x, na.rm = TRUE)
                    x
                  },
                  "median" = {
                    x[is.na(x)] <- median(x, na.rm = TRUE)
                    x
                  },
                  "knn" = {
                    # Requires imputation package
                    # x <- impute(x, method = "knn")
                    x
                  })
          })
          
          # Categorical missing value handling
          df[categorical_vars] <- lapply(df[categorical_vars], function(x) {
            switch(input$na_categorical,
                  "mode" = {
                    mode_val <- names(sort(table(x), decreasing = TRUE))[1]
                    x[is.na(x)] <- mode_val
                    x
                  },
                  "new_category" = {
                    x[is.na(x)] <- "Unknown"
                    x
                  })
          })
          
          # Scaling
          df[numeric_vars] <- switch(input$scaling,
            "standard" = scale(df[numeric_vars]),
            "normalize" = apply(df[numeric_vars], 2, function(x) (x - min(x)) / (max(x) - min(x))),
            "robust" = {
              # Robust scaling using median and IQR
              apply(df[numeric_vars], 2, function(x) {
                (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
              })
            },
            df[numeric_vars]
          )
          
          return(df)
    }

  # Model fitting handler
  observeEvent(input$fit_model, {
    req(data(), input$target_var)
 
    
    withProgress(message = 'Entraînement du modèle...', {
      
      tryCatch({
        # Preprocess data
        # df <- data()
        df <- preprocess_data(data(), input)
        

    # # Traitement des valeurs manquantes
    # numeric_vars <- names(df)[sapply(df, is.numeric)]
    # df[numeric_vars] <- lapply(df[numeric_vars], function(x) {
    #   x[is.na(x)] <- ifelse(input$na_numeric == "mean", mean(x, na.rm = TRUE), median(x, na.rm = TRUE))
    #   x
    # })

    # # Traitement des variables catégorielles manquantes
    # if (input$na_categorical == "mode") {
    #   cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    #   df[cat_vars] <- lapply(df[cat_vars], function(x) {
    #     mode_value <- as.character(stats::Mode(x[!is.na(x)]))
    #     x[is.na(x)] <- mode_value
    #     x
    #   })
    # }

    # # Scaling
    # if (input$scaling == "standardization") {
    #   df[numeric_vars] <- lapply(df[numeric_vars], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    # } else if (input$scaling == "normalization") {
    #   df[numeric_vars] <- lapply(df[numeric_vars], function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    # }




        # Initialize preprocessor
        # preprocessor <- Preprocessor$new(encoding_type = input$encoding)
        
        # Preprocess the data
        # df <- preprocessor$preprocess(df, is_training = TRUE)
        
        # Prepare target variable
        target <- df[[input$target_var]]
        if (!is.factor(target)) {
          target <- as.factor(target)
        }
        validate(need(length(unique(target)) > 2, "La variable cible doit avoir plus de 2 catégories"))
        
        # Train/test split
        set.seed(123)
        train_idx <- createDataPartition(target, p = input$train_ratio, list = FALSE)

        # train_idx <- sample(nrow(df), size = floor(input$train_ratio * nrow(df)))
        
        # Feature engineering
        pred_vars <- setdiff(names(df), input$target_var)
        validate(need(length(pred_vars) > 0, "Aucune variable prédictive disponible"))
        
        # Prepare data for model
        X_train <- df[train_idx, pred_vars, drop = FALSE]
        y_train <- target[train_idx]
        X_test <- df[-train_idx, pred_vars, drop = FALSE]
        y_test <- target[-train_idx]
        
        # Initialize and train the multinomial regression model
        model_instance <- MultinomialRegression$new(
          optimizer_type = input$optimizer_type,
          encoding_type = input$encoding,
          learning_rate = input$learning_rate,
          epochs = input$epochs,
          batch_size = input$batch_size
          # preprocessor = preprocessor
        )
        
        # if(input$encoding == "onehot"){
        #   X_train <- model_instance$preprocess(X_train,y_train ,is_training = TRUE)
        #   }
        # if(input$encoding == "label"){
        #   X_train <- model_instance$preprocess(X_train, y_train,is_training = TRUE)
        #   }
        # if(input$encoding == "afdm"){
        #   X_train <- model_instance$preprocess(X_train, y_train,is_training = TRUE, ncp = 5)
           
        #   }

        model_instance$fit(X_train, y_train, preprocess = TRUE, ncp = 1)
        
        # Make predictions
        pred_train <- model_instance$predict(X_train)

        # if(input$encoding == "onehot"){
        #   X_test <- model_instance$preprocess(X_train,y_train ,is_training = FALSE)
        #   }
        # if(input$encoding == "label"){
        #   X_test <- model_instance$preprocess(X_train, y_train,is_training = FALSE)
        #   }
        # if(input$encoding == "afdm"){
        #   X_test <- model_instance$preprocess(X_train, y_train,is_training = FALSE, ncp = 5)   
        #   }


        pred_test <- model_instance$predict(X_test)
        
        # Calculate metrics
        conf_mat <- table(Actual = y_test, Predicted = pred_test)
        accuracy <- mean(pred_test == y_test)
        
        # Store all model data
        model_data <- list(
          model_instance = model_instance,
          target_var = input$target_var,
          # train_idx = train_idx,
          # X_train = X_train,
          # y_train = y_train,
          # X_test = X_test,
          # y_test = y_test,
          # pred_train = pred_train,
          # pred_test = pred_test,
          conf_mat = conf_mat,
          accuracy = accuracy
        )
        
        # Update model
        model(model_data)
        
        showNotification(
          sprintf("Modèle entraîné avec succès (%.1f%% accuracy)", 
                  accuracy * 100),
          type = "message",
          duration = 2
        )
        
      }, error = function(e) {
        showNotification(
          sprintf("Erreur lors de l'entraînement: %s", e$message),
          type = "error",
          duration = 2
        )
      })
    })
  })
  

  # Model summary output
  output$model_summary <- renderPrint({
    req(model())
    summary(model()$model_instance$summary())
  })
  
  # Coefficient table output
  output$coef_table <- renderDT({
    req(model())
    coef_df <- as.data.frame(model()$model_instance$coef())
    datatable(coef_df)
  })
  
  # Variable importance plot
  output$var_importance_plot <- renderPlot({
    req(model())
    var_imp <- model()$model_instance$var_importance()
    barplot(var_imp, main = "Importance des variables", col = "blue")
  })
  
  # Variable importance table
  output$var_importance_table <- renderDT({
    req(model())
    var_imp <- model()$model_instance$var_importance()
    var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)
    datatable(var_imp_df)
  })
  
  # FAMD scree plot
  output$famd_scree_plot <- renderPlot({
    req(model())
    eig_val <- model()$model_instance$preprocessor$explore_famd(model()$X_train)
    barplot(eig_val[, 2], names.arg = 1:nrow(eig_val),
            main = "Variances Explained by Dimensions (%)",
            xlab = "Principal Dimensions", ylab = "Percentage of variances", col = "steelblue")
    lines(x = 1:nrow(eig_val), eig_val[, 2], type = "b", pch = 19, col = "red")
  })
  
  # FAMD contributions table
  output$famd_contributions <- renderDT({
    req(model())
    eig_val <- model()$model_instance$preprocessor$explore_famd(model()$X_train)
    datatable(eig_val)
  })
  
  # FAMD variables plot
  output$famd_variables_plot <- renderPlot({
    req(model())
    model()$model_instance$plot_confusion_matrix()
    # famd_result <- FactoMineR::FAMD(model()$X_train)
    # plot(famd_result, choix = "var")
  })
  
  # Model info output
  output$model_info <- renderPrint({
    req(model())
    model()
  })
  
  # Download model handler
  output$download_model <- downloadHandler(
    filename = function() {
      paste0(input$model_name, ".rds")
    },
    content = function(file) {
      saveRDS(model(), file)
    }
  )
  
  return(reactive({ model() }))
}