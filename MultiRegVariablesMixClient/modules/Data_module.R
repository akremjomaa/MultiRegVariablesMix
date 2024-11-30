# # UI du module
# fileLoaderModuleUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     box(
#       title = "Charger un fichier",
#       status = "primary",
#       solidHeader = TRUE,
#       width = 12,
#       fileInput(ns("file_input"), "Choisissez un fichier CSV ou Excel",
#                 accept = c(".csv", ".xlsx", ".xls")),
#       conditionalPanel(
#         condition = sprintf("input['%s'] && input['%s'].indexOf('.csv') !== -1", ns("file_input"), ns("file_input")),
#         ns = ns,
#         textInput(ns("delimiter"), "Délimiteur (par défaut : ',')", value = ",")
#       )
#     ),
#     box(
#       title = "Aperçu des données",
#       status = "primary",
#       solidHeader = TRUE,
#       width = 12,
#       DTOutput(ns("data_table"))
#     )
#   )
# }

# # Serveur du module
# fileLoaderModuleServer <- function(input, output, session) {
#   ns <- session$ns

#   # Réactif pour lire le fichier
#   data <- reactive({
#     req(input$file_input)

#     file <- input$file_input$datapath
#     ext <- tools::file_ext(file)

#     if (ext == "csv") {
#       read.csv(file, sep = input$delimiter, stringsAsFactors = FALSE)
#     } else if (ext %in% c("xlsx", "xls")) {
#       readxl::read_excel(file)
#     } else {
#       stop("Format de fichier non pris en charge.")
#     }
#   })

#   # Affichage des données
#   output$data_table <- renderDT({
#     req(data())
#     datatable(data(), options = list(scrollX = TRUE))
#   })
# }
