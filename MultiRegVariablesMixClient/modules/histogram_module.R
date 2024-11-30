# Module pour afficher un histogramme
moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("column_select"), "Choisissez une colonne:", choices = names(mtcars)),
    plotOutput(ns("histogram_output"))
  )
}

histogram_server <- function(input, output, session) {
  ns <- session$ns

  # Données exemple (remplacez par les vôtres)
  data <- mtcars

  output$histogram_output <- renderPlot({
    req(input$column_select)
    ggplot(data, aes_string(x = input$column_select)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(
        title = paste("Distribution de", input$column_select),
        x = input$column_select,
        y = "Fréquence"
      )
  })
}
