# SHINY UI #####################################################################

manual_configuration_ui <- function(id, algorithms_list) {
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # TODO
  select_algorithms <- selectizeInput(
    inputId = ns("algorithmsList"),
    label = "Select a list of ML algorithms",
    choices = algorithms_list,
    multiple = TRUE
  )
  
  # TODO
  tagList(
    select_algorithms
  )
}

# SERVER LOGIC #################################################################

manual_configuration <- function(input, output, session) {
  # TODO
  algorithms_selected <- reactive({
    input$algorithmsList
  })
  
  # TODO
  algorithms_selected()
}
