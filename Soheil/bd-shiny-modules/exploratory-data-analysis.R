source("bd-shiny-modules/exploratory-data-analysis-submodules/visualising-distributions.R")

# SHINY UI #####################################################################

exploratory_data_analysis_ui <- function(id) {
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # TODO
  plot_output <- plotOutput(ns("plot"))

  # TODO
  select_action <- selectInput(
    inputId = ns("selectAction"),
    label = "What would you like to do?",
    choices = list(
      "Visualize distributions" = "visualizeDist"
    )
  )

  # TODO
  show_button <- actionButton(ns("show"), "Show")

  # Sidebar panel
  sidebar_panel <- sidebarPanel(
    select_action,
    hr(),
    uiOutput(ns("actionSelected")),
    show_button
  )

  # Sidebar panel
  main_panel <- mainPanel(
    h4("Data visualization"),
    plot_output
  )

  # Sidebar layout
  sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)

  tagList(
    h3("Exploratory Data Analysis"),
    hr(),
    sidebar_layout
  )
}

# SERVER LOGIC #################################################################

exploratory_data_analysis <- function(input, output, session, dataset) {
  # TODO
  rv <- reactiveValues(current_plot = NULL)
  current_plot <- reactive({
    rv$current_plot
  })

  # Namespace
  ns <- session$ns

  # TODO
  output$actionSelected <- renderUI({
    switch(input$selectAction,
      "visualizeDist" = visualising_distributions_ui(ns("visualizeDist"), colnames(dataset()))
    )
  })

  # TODO
  observeEvent(input$show, {
    rv$current_plot <- switch(input$selectAction,
      "visualizeDist" = callModule(visualising_distributions, "visualizeDist", dataset)
    )
  })
  
  # TODO
  output$plot <- renderPlot({
    current_plot()
  }, height = 650)
}
