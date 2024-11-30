dataExplorationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("selected_file"), "Choisir un fichier", choices = NULL),
    tabsetPanel(
      tabPanel("Structure", verbatimTextOutput(ns("structure"))),
      tabPanel("Résumé statistique", verbatimTextOutput(ns("summary"))),
      tabPanel("Visualisation",
               selectInput(ns("variable"), "Choisir une variable numérique", choices = NULL),
               plotOutput(ns("histogram"))
      )
    )
  )
}

# Define server logic for the exploration module
data_table_server <- function(input, output, session, data_list) {
  ns <- session$ns
  
  observe({
    updateSelectInput(session, "selected_file", choices = names(data_list()))
  })
  
  selected_data <- reactive({
    req(input$selected_file)
    data_list()[[input$selected_file]]
  })
  
  observe({
    updateSelectInput(session, "variable", choices = names(selected_data()))
  })
  
  output$structure <- renderPrint({
    req(selected_data())
    str(selected_data())
  })
  
  output$summary <- renderPrint({
    req(selected_data())
    summary(selected_data())
  })
  
  output$histogram <- renderPlot({
    req(selected_data(), input$variable)
    data <- selected_data()
    var <- input$variable
    if (is.numeric(data[[var]])) {
      hist(data[[var]], main = paste("Histogramme de", var), xlab = var, col = "skyblue", border = "white")
    } else {
      showNotification("Veuillez sélectionner une variable numérique.", type = "error")
    }
  })
}

 