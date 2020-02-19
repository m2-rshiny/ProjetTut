source("bd-shiny-modules/model-selection-submodules/manual-configuration.R")
source("bd-shiny-modules/model-selection-submodules/import-configuration.R")

# SHINY UI #####################################################################

model_selection_ui <- function(id) {
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # TODO
  classif_or_reg <- radioButtons(ns("classifOrReg"),
    label = "Choose a type of prediction problem",
    choices = c("Classification", "Regression")
  )

  # TODO
  select_config_method <- radioButtons(ns("configMethod"),
    label = "Select a configuration method",
    choices = list(
      "Configure manually" = "manual",
      "Import a configuration file" = "file"
    )
  )

  # TODO
  load_button <- actionButton(ns("load"), "Load configuration")

  # UI LAYOUTS #################################################################

  # Sidebar panel
  sidebar_panel <- sidebarPanel(
    h3("General Configuration"),
    hr(),
    classif_or_reg,
    select_config_method,
    hr(),
    uiOutput(ns("configMethodSelected")),
    hr(),
    load_button,
    hr(),
    uiOutput(ns("params"))
  )

  # Sidebar panel
  main_panel <- mainPanel(
  )
  # Sidebar layout
  sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)

  # Return UI
  tagList(
    h3("Model Selection"),
    hr(),
    sidebar_layout
  )
}

# SERVER LOGIC #################################################################

model_selection <- function(input, output, session, dataset, algorithms_list, algorithms_details) {
  # Namespace
  ns <- session$ns

  # TODO
  rv <- reactiveValues(algorithms_selected = NULL)
  algorithms_selected <- reactive({
    rv$algorithms_selected
  })

  # TODO
  output$configMethodSelected <- renderUI({
    switch(input$configMethod,
      "manual" = manual_configuration_ui(ns("manual"), algorithms_list),
      "file" = import_configuration_ui(ns("file"))
    )
  })

  # TODO
  observeEvent(input$load, {
    rv$algorithms_selected <- switch(input$configMethod,
      "manual" = callModule(manual_configuration, "manual")
    )
  })

  # TODO
  output$params <- renderUI({
    # Fonction qui créé le bon nombre d'inputs en fonction de ses paramètres
    ui <- lapply(algorithms_selected(), function(i) {
      # Information à propos du modèle
      doc <- getModelInfo(model = i, regex = FALSE)[[1]]

      # TODO
      algo_name <- algorithms_details[[i]]$name
      algo_id <- i

      # Partie ui
      tagList(
        hr(),
        h4(HTML(str_interp("${algo_name} | <code>${i}</code>"))),
        hr(),
        # Ajout des différents paramètres pris par l'algorithme
        p(HTML(algorithms_details[[i]]$desc)),
        hr(),
        apply(algorithms_details[[i]]$parameters, 1, function(j) {
          parameter_id <- j["id"]
          parameter_name <- j["name"]
          parameter_type <- j["type"]
          textInput(paste0(i, parameter_id), HTML(str_interp("${parameter_name} (<code>${parameter_id}</code>, ${parameter_type})")))
        })
      )
    })
    
    tagList(
      h3("Hyperparameter Space"),
      ui
    )
  })
}
