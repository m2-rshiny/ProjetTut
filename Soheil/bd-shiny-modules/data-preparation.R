source("bd-shiny-modules/data-preparation-submodules/select-filter-feature.R")
source("bd-shiny-modules/data-preparation-submodules/change-type-feature.R")
source("bd-shiny-modules/data-preparation-submodules/rename-feature.R")
source("bd-shiny-modules/data-preparation-submodules/mutate-feature.R")

# SHINY UI #####################################################################

data_preparation_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # DataTable output
  datatable_output <- dataTableOutput(ns("datatable"))

  # Tibble output
  tibble_output <- verbatimTextOutput(ns("tibble"))

  # Select input to choose the preprocessing function
  select_feature <- selectInput(
    inputId = ns("selectFeature"),
    label = "What would you like to do?",
    choices = list(
      "Select, arrange or filter data" = "selectFilter",
      "Add new variable" = "mutate",
      "Change the type of one or more columns" = "changeType",
      "Rename a column" = "rename"
    )
  )

  # Button to apply changes to the dataset
  apply_button <- actionButton(ns("apply"), "Apply")

  # Button to return to the original dataset
  reset_button <- actionButton(ns("reset"), "Reset")

  # UI LAYOUTS #################################################################

  # Sidebar panel
  sidebar_panel <- sidebarPanel(
    select_feature,
    hr(),
    uiOutput(ns("featureSelected")),
    reset_button,
    apply_button
  )

  # Sidebar panel
  main_panel <- mainPanel(
    h4("Dataset"),
    datatable_output,
    h4("More details"),
    tibble_output
  )
  # Sidebar layout
  sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)

  # Return UI
  tagList(
    h3("Data Preparation"),
    hr(),
    sidebar_layout
  )
}

# SERVER LOGIC #################################################################

data_preparation <- function(input, output, session, orig_dataset) {
  # TODO
  rv <- reactiveValues(current_dataset = NULL)
  current_dataset <- reactive({
    if (is.null(rv$current_dataset)) return(orig_dataset())
    rv$current_dataset
  })

  # Namespace
  ns <- session$ns

  # TODO
  output$featureSelected <- renderUI({
    switch(input$selectFeature,
      "selectFilter" = select_filter_feature_ui(ns("selectFilter")),
      "changeType" = change_type_feature_ui(ns("changeType"), colnames(current_dataset())),
      "rename" = rename_feature_ui(ns("rename"), colnames(current_dataset())),
      "mutate" = mutate_feature_ui(ns("mutate"))
    )
  })

  # Store the dataset transformed
  observeEvent(input$apply, {
    rv$current_dataset <- switch(input$selectFeature,
      "selectFilter" = callModule(select_filter_feature, "selectFilter", current_dataset),
      "changeType" = callModule(change_type_feature, "changeType", current_dataset),
      "rename" = callModule(rename_feature, "rename", current_dataset),
      "mutate" = callModule(mutate_feature, "mutate", current_dataset)
    )
  })

  # Reset dataset
  observeEvent(input$reset, {
    rv$current_dataset <- orig_dataset()
  })

  # DataTable to display
  output$datatable <- renderDataTable({
    datatable(current_dataset(), options = list(scrollX = TRUE))
  })

  # Tibble output
  output$tibble <- renderPrint({
    current_dataset()
  })

  # Return the DataTable
  return(current_dataset)
}
