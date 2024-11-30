# Module pour afficher un résumé des données
moduleUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("summary_output"))
}

summary_server <- function(input, output, session) {
  ns <- session$ns

  # Données exemple (remplacez par les vôtres)
  data <- mtcars

  output$summary_output <- renderPrint({
    summary(data)
  })
}
