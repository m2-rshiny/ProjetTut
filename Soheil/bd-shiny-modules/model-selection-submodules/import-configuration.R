# SHINY UI #####################################################################

import_configuration_ui <- function(id) {
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # TODO
  upload_config_file <- fileInput(
    inputId = ns("uploadConfigFile"),
    label = "Load a configuration file",
    accept = c("application/json", ".json")
  )
  
  # TODO
  tagList(
    upload_config_file
  )
}

# SERVER LOGIC #################################################################

import_configuration <- function(input, output, session) {
  # TODO
  fromJSON(input$uploadConfigFile$datapath)
}
