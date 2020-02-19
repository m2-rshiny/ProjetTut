# SHINY UI #####################################################################

built_in_dataset_importation_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # Input to select a built-in dataset
  select_dataset <- selectInput(
    inputId = ns("datasetName"),
    label = "Select a dataset",
    choices = list.files(path = "built-in-datasets/")
  )

  # Return the UI
  tagList(
    select_dataset
  )
}

# SERVER LOGIC #################################################################

built_in_dataset_importation <- function(input, output, session) {
  # Load the built-in dataset selected (reactive object)
  dataset <- reactive({
    e <- new.env()
    load(file = paste0("built-in-datasets/", input$datasetName), envir = e)
    e[[input$datasetName]]
  })

  # Return the reactive dataset
  dataset
}
