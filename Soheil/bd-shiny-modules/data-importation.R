# By default, the file size limit is 5 MB. It can be changed by setting this
# option. Here we'll raise limit to 50 MB.
options(shiny.maxRequestSize = 50 * 1024^2)

# SHINY SUBMODULES #############################################################

source("bd-shiny-modules/data-importation-submodules/built-in-dataset-importation.R")
source("bd-shiny-modules/data-importation-submodules/external-dataset-importation.R")

# SHINY UI #####################################################################

data_importation_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # Input to select whether we want to import an external or built-in dataset
  data_source <- radioButtons(
    inputId = ns("selectSource"),
    label = "Would you like to import a built-in dataset or one from an external file?",
    choices = list(
      "Built-in dataset" = "builtInDataset",
      "External dataset" = "externalDataset"
    )
  )

  # Button to load data
  load_data_button <- actionButton(ns("loadDataButton"), "Load data")

  # DataTable output
  datatable_output <- dataTableOutput(ns("datatable"))
  
  # Tibble output
  tibble_output <- verbatimTextOutput(ns("tibble"))

  # UI LAYOUTS ####################################################################

  # Sidebar panel
  sidebar_panel <- sidebarPanel(
    data_source,
    tags$hr(),
    uiOutput(ns("sourceSelected")),
    load_data_button
  )

  # Main panel
  main_panel <- mainPanel(
    h4("Dataset"),
    datatable_output,
    h4("More details"),
    tibble_output
  )

  # Sidebar layout
  sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)

  # Return the UI
  tagList(
    h3("Data Importation"),
    hr(),
    sidebar_layout
  )
}

# SERVER LOGIC #################################################################

data_importation <- function(input, output, session) {
  # Namespace
  ns <- session$ns
  
  # UI to be displayed according to the selected data source
  import_built_in_dataset_ui <- built_in_dataset_importation_ui(ns("builtIn"))
  import_external_dataset_ui <- external_dataset_importation_ui(ns("external"))
  
  # Render the UI according to the selected data source
  output$sourceSelected <- renderUI({
    switch(input$selectSource,
      "builtInDataset" = import_built_in_dataset_ui,
      "externalDataset" = import_external_dataset_ui
    )
  })

  # Store the dataset imported
  data_imported <- eventReactive(input$loadDataButton, {
    dataset <- switch(input$selectSource,
      "builtInDataset" = callModule(built_in_dataset_importation, "builtIn"),
      "externalDataset" = callModule(external_dataset_importation, "external")
    )
    as_tibble(dataset())
  })

  # DataTable to display
  output$datatable <- renderDataTable({
    datatable(data_imported(), options = list(scrollX = TRUE))
  })
  
  # Tibble to display
  output$tibble <- renderPrint({
    data_imported()
  })

  # Return the tibble
  data_imported
}
