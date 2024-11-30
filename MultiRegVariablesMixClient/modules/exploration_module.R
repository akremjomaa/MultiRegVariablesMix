library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

# UI Module
explorationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      width = 12,
      # Onglet 1: Statistiques générales
      tabPanel(
        "Statistiques générales",
        fluidRow(
          # box(
          #   width = 12,
          #   title = "Structure des données",
          #   fluidRow(
          #     column(12,
          #       DTOutput(ns("structure_table")),
          #       actionButton(ns("apply_changes"), "Appliquer les modifications", 
          #                  class = "btn-primary", style = "margin-top: 10px")
          #     )
          #   )
          # ),
          box(
            width = 6,
            title = "Structure des données",
            verbatimTextOutput(ns("str_output"))
          ),
          box(
            width = 6,
            title = "Résumé statistique",
            verbatimTextOutput(ns("summary_output"))
          )
        )
      ),
      
      # Onglet 2: Variables catégorielles
      tabPanel(
        "Variables catégorielles",
        fluidRow(
          box(
            width = 3,
            selectInput(ns("cat_var"), "Choisir une variable catégorielle", choices = NULL),
            actionButton(ns("plot_cat"), "Générer les graphiques")
          ),
          box(
            width = 9,
            plotlyOutput(ns("bar_plot")),
            plotlyOutput(ns("pie_chart"))
          )
        )
      ),
      
      # Onglet 3: Variables continues
      tabPanel(
        "Variables continues",
        fluidRow(
          box(
            width = 3,
            selectInput(ns("num_var"), "Choisir une variable continue", choices = NULL),
            actionButton(ns("plot_num"), "Générer les graphiques")
          ),
          box(
            width = 9,
            plotlyOutput(ns("histogram")),
            plotlyOutput(ns("boxplot")),
            plotlyOutput(ns("density_plot"))
          )
        )
      )
    )
  )
}

# Server Module
explorationModuleServer <- function(input, output, session, shared_data) {
  ns <- session$ns
  
  # Reactive wrapper for shared data
  data <- reactive({
    req(shared_data())
    if (is.list(shared_data()) && !is.data.frame(shared_data())) {
      shared_data()[[1]]
    } else {
      shared_data()
    }
  })
  
  # Structure table
  
  # Structure table with fixed column handling
# output$structure_table <- renderDT({
#   req(data())
#   df <- data()
  
  # # Create structure summary

  # struct_df <- data.frame(
  #   Variable = names(df),
  #   Type = vapply(df, function(x) class(x)[1], character(1)),
  #   Action = vapply(names(df), function(v) {
  #     sprintf(
  #       '<select class="form-control action-select" data-var="%s">
  #         <option value="keep">Garder</option>
  #         <option value="remove">Supprimer</option>
  #       </select>',
  #       v
  #     )
  #   }, character(1)),
  #   Conversion = vapply(names(df), function(v) {
  #     if (is.integer(df[[v]])) {
  #       sprintf(
  #         '<select class="form-control type-select" data-var="%s">
  #           <option value="keep">Garder type actuel</option>
  #           <option value="factor">Convertir en facteur</option>
  #         </select>',
  #         v
  #       )
  #     } else {
  #       "Pas de conversion disponible"
  #     }
  #   }, character(1)),
  #   stringsAsFactors = FALSE
  # )
  
#   datatable(
#     struct_df,
#     escape = FALSE,
#     selection = 'none',
#     options = list(
#       pageLength = nrow(struct_df),
#       dom = 't',
#       columnDefs = list(
#         list(targets = c(2,3), className = "dt-center")
#       )
#     ),
#     callback = JS(sprintf("
#       table.on('change', 'select', function() {
#         var $select = $(this);
#         Shiny.setInputValue(
#           '%s', 
#           {
#             variable: $select.data('var'),
#             action: $select.val(),
#             type: $select.hasClass('action-select') ? 'action' : 'conversion'
#           },
#           {priority: 'event'}
#         );
#       });
#     ", ns("structure_changes")))
#   )
# })
  
  # # Handle structure changes
  # observeEvent(input$apply_changes, {
  #   req(data())
  #   df <- data()
  #   changes <- isolate(input[[ns("structure_changes")]])
    
  #   if (!is.null(changes)) {
  #     withProgress(message = 'Application des modifications...', {
  #       tryCatch({
  #         if (changes$type == "action" && changes$action == "remove") {
  #           df[[changes$variable]] <- NULL
  #         }
  #         else if (changes$type == "conversion" && changes$action == "factor" && 
  #                  is.integer(df[[changes$variable]])) {
  #           df[[changes$variable]] <- as.factor(df[[changes$variable]])
  #         }
          
  #         shared_data(df)
  #         showNotification("Modifications appliquées avec succès", type = "success")
  #       }, error = function(e) {
  #         showNotification(paste("Erreur:", e$message), type = "error")
  #       })
  #     })
  #   }
  # })
  
  # Update variable choices
  observe({
    req(data())
    df <- data()
    
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "cat_var", choices = cat_vars)
    updateSelectInput(session, "num_var", choices = num_vars)
  })
  
  # Display data structure
  output$str_output <- renderPrint({
    req(data())
    str(data())
  })
  
  output$summary_output <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Categorical plots
  observeEvent(input$plot_cat, {
    req(data(), input$cat_var)
    df <- data()
    
    output$bar_plot <- renderPlotly({
      p <- ggplot(df, aes_string(x = input$cat_var)) +
        geom_bar(fill = "steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribution de", input$cat_var))
      ggplotly(p)
    })
    
    output$pie_chart <- renderPlotly({
      freq_table <- table(df[[input$cat_var]])
      plot_ly(labels = names(freq_table), 
              values = as.numeric(freq_table),
              type = "pie",
              hole = 0.4) %>%
        layout(title = paste("Répartition de", input$cat_var))
    })
  })
  
  # Continuous plots
  observeEvent(input$plot_num, {
    req(data(), input$num_var)
    df <- data()
    
    output$histogram <- renderPlotly({
      p <- ggplot(df, aes_string(x = input$num_var)) +
        geom_histogram(fill = "steelblue", bins = 30) +
        theme_minimal() +
        labs(title = paste("Distribution de", input$num_var))
      ggplotly(p)
    })
    
    output$boxplot <- renderPlotly({
      p <- ggplot(df, aes_string(y = input$num_var)) +
        geom_boxplot(fill = "steelblue") +
        theme_minimal() +
        labs(title = paste("Boîte à moustaches de", input$num_var))
      ggplotly(p)
    })
    
    output$density_plot <- renderPlotly({
      p <- ggplot(df, aes_string(x = input$num_var)) +
        geom_density(fill = "steelblue", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Densité de", input$num_var))
      ggplotly(p)
    })
  })
  
  return(reactive({ data() }))
}