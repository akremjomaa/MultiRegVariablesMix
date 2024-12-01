library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(data.table)

fileLoaderModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Importer des fichiers",
        width = 6,
        fileInput(
          ns("dataFiles"),
          "Sélectionnez des fichiers (CSV ou Excel)",
          multiple = TRUE,
          accept = c(".csv", ".xlsx", ".xls")
        )
      ),
      box(
        title = "Options d'importation",
        width = 6,
        uiOutput(ns("options"))
      )
    ),
    box(
      title = "Aperçu des fichiers",
      width = 12,
      DTOutput(ns("preview"))
      # downloadButton(ns("download"), "Télécharger les données")
    )
  )
}

fileLoaderModuleServer <- function(input, output, session, shared_data) {
  ns <- session$ns
  
  # Initialize shared_data if NULL
  if (is.null(shared_data)) {
    shared_data <- reactiveVal(NULL)
  }

  # Store current file type
  file_type <- reactiveVal(NULL)
  
  # Update file type when file is selected
  observe({
    req(input$dataFiles)
    file_type(tools::file_ext(input$dataFiles$name[1]))
  })
  
  # Render import options based on file type
  output$options <- renderUI({
    req(file_type())
    
    if (file_type() == "csv") {
      tagList(
        selectInput(
          ns("sep"), 
          "Séparateur",
          choices = c(
            "Point-virgule" = ";",
            "Virgule" = ",", 
            "Tabulation" = "\t"
          ),
          selected = ";"
        ),
        selectInput(
          ns("header"), 
          "En-tête",
          choices = c(
            "Oui" = TRUE, 
            "Non" = FALSE
          ),
          selected = TRUE
        ),
        selectInput(
          ns("encoding"),
          "Encodage",
          choices = c(
            "UTF-8" = "UTF-8",
            "Latin-1" = "Latin-1"
          ),
          selected = "UTF-8"
        )
      )
    } else if (file_type() %in% c("xlsx", "xls")) {
      numericInput(
        ns("sheet1"),
        "Numéro de la feuille",
        value = 1,
        min = 1
      )
    }
  })
  
  # Load and process data
  loaded_data <- reactive({
    req(input$dataFiles, file_type())
    
    withProgress(message = 'Chargement des données...', {
      tryCatch({
        file_path <- input$dataFiles$datapath[1]
        
        # Validate file exists
        validate(need(file.exists(file_path), "Le fichier n'existe pas"))
        
        if (file_type() == "csv") {
          req(input$sep, input$header, input$encoding)
          df <- fread(
            file = file_path,
            sep = input$sep,
            header = as.logical(input$header),
            data.table = FALSE,
            encoding = input$encoding,
            stringsAsFactors = TRUE,
            na.strings = c("NA", "", "NULL")
          )
        } else {
          req(input$sheet1)
          df <- read_excel(
            file_path,
            sheet = as.numeric(input$sheet1),
            col_names = TRUE
          )
          # Convert characters to factors
          df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
        }
        
        # Validate data frame
        validate(
          need(ncol(df) > 0, "Le fichier ne contient aucune colonne"),
          need(nrow(df) > 0, "Le fichier ne contient aucune ligne")
        )
        
        # Update shared data
        shared_data(df)
        
        showNotification(
          paste("Données chargées avec succès:", nrow(df), "lignes,", ncol(df), "colonnes"),
          type = "message",
          duration = 2
        )
        
        return(df)
        
      }, error = function(e) {
        showNotification(
          paste("Erreur lors de l'importation"),
          type = "error",
          duration = 2
        )
        return(NULL)
      })
    })
  })
  
  # Preview table
  output$preview <- renderDT({
    req(loaded_data())
    datatable(
      loaded_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      selection = 'none'
    )
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(loaded_data(), file, row.names = FALSE)
    }
  )
  
  # Return reactive for access to shared data
  return(reactive({ shared_data() }))
}