# Module pour afficher un nuage de points
moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x_var"), "Variable X:", choices = names(mtcars)),
    selectInput(ns("y_var"), "Variable Y:", choices = names(mtcars)),
    plotOutput(ns("scatterplot_output"))
  )
}

scatterplot_server <- function(input, output, session) {
  ns <- session$ns

  # Données exemple (remplacez par les vôtres)
  data <- mtcars

  output$scatterplot_output <- renderPlot({
    req(input$x_var, input$y_var)
    ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(
        title = paste("Relation entre", input$x_var, "et", input$y_var),
        x = input$x_var,
        y = input$y_var
      )
  })
}
