# mod_output_ui.R
mod_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("output_data"))
  )
}

# mod_output_server.R
mod_output_server <- function(id, result) {
  moduleServer(id, function(input, output, session) {
    output$output_data <- renderPrint({
      result()
    })
  })
}