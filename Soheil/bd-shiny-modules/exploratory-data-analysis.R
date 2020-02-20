source("bd-shiny-modules/exploratory-data-analysis-submodules/visualising-distributions.R")

# SHINY UI #####################################################################

exploratory_data_analysis_ui <- function(id) {
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  plt_intro <- plotOutput(ns("data_desc"))
  
  # Graphique des valeurs manquantes
  plt_missing <- plotOutput(ns("plt_missing"))
  
  # Permet de selectionner une variable
  var_desc_choice <- selectInput(ns("variable_desc"), "Choice one variable:", choices = "")
  
  # Affiche un boxplote si la variable est quanti sinon un barplot
  plt_var_desc <- plotlyOutput(ns("plt_variable_desc"))
  
  # Imputation
  ui_imput <- uiOutput(ns("imputation"))
  plot_density_imput <- plotOutput(ns("show_imput"))
  
  method_imputation <- selectInput(ns("met_imput"), "Imputation methode", choices = c("rf", "pmm", "norm", "mean"))

  tab2 <- tabPanel(
    "Stat Desc",
    class = "pages",
    fluidRow(
      column(
        4,
        plt_intro
      ), # Affiche l'intro du dataframe
      column(
        4,
        plt_missing # Affiche les données manquantes
      ),
      #  column(4, diagonalNetworkOutput("var_type")),
      column(
        4,
        var_desc_choice,
        # selectInput("xvar_desc", "Variable X:(Integer)", choices = ""),
        plt_var_desc # Affiche un boxplot ou un barplot
      )
    ),
    fluidRow(
      column(
        2,
        ui_imput,
        conditionalPanel(
          condition = "input.imput == true",
          ns = ns,
          method_imputation,
          actionButton(ns("exec_imput"), "Execute")
        )
      ),
      column(
        5,
        conditionalPanel(
          condition = "input.imput == true",
          ns = ns,
          plot_density_imput
        )
      ),
      column(5, dataTableOutput(ns("tbl")))
    )
  )
  
  # # TODO
  # plot_output <- plotOutput(ns("plot"))
  # 
  # # TODO
  # select_action <- selectInput(
  #   inputId = ns("selectAction"),
  #   label = "What would you like to do?",
  #   choices = list(
  #     "Visualize distributions" = "visualizeDist"
  #   )
  # )
  # 
  # # TODO
  # show_button <- actionButton(ns("show"), "Show")
  # 
  # # Sidebar panel
  # sidebar_panel <- sidebarPanel(
  #   select_action,
  #   hr(),
  #   uiOutput(ns("actionSelected")),
  #   show_button
  # )
  # 
  # # Sidebar panel
  # main_panel <- mainPanel(
  #   h4("Data visualization"),
  #   plot_output
  # )
  # 
  # # Sidebar layout
  # sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)
  # 
  # tagList(
  #   h3("Exploratory Data Analysis"),
  #   hr(),
  #   sidebar_layout
  # )
}

# SERVER LOGIC #################################################################

exploratory_data_analysis <- function(input, output, session, data_inp) {
  ns <- session$ns
  
  output$data_desc <- renderPlot({
    plot_intro(data_inp()) +
      theme_minimal() +
      theme(legend.position = "top") +
      scale_fill_manual(values = mycolors)
  })
  
  output$plt_missing <- renderPlot({
    data_inp() %>%
      plot_missing() +
      ggtitle("Missing Values") +
      theme_minimal() +
      theme(legend.position = "top") +
      scale_fill_manual(values = mycolors)
  })
  
  observe({
    updateSelectizeInput(session, "variable_desc", choices = names(data_inp()))
    # updateSelectizeInput(session, "xvar_desc",
    # choices = names(data_inp())[-which(names(data_inp())==input$yvar_desc)])
  })
  
  output$plt_variable_desc <- renderPlotly({
    y <- as.data.frame(data_inp())[, input$variable_desc]
    calss_yvar_desc <- class(y)
    if (calss_yvar_desc == "factor" || calss_yvar_desc == "character") {
      p <- data_inp() %>%
        count(Variable = y) %>%
        mutate(Class = fct_reorder(Variable, n)) %>%
        ggplot(aes(Class, n)) +
        geom_col(fill = "#0072B2") +
        geom_text(aes(label = n), size = 3) +
        coord_flip() +
        labs(x = "", y = "")
      ggplotly(p)
    }
    else {
      p <- ggplot(data_inp()) +
        geom_boxplot(aes(x = "", y = y), fill = "#0072B2") +
        labs(x = "", y = "")
      ggplotly(p)
    }
  })
  
  # Imputation
  output$imputation <- renderUI({
    if (sum(is.na(as.data.frame(data_inp()))) != 0) {
      checkboxInput(ns("imput"), "Data Imputation", FALSE)
    }
  })
  
  
  data_imput <- eventReactive(input$exec_imput, {
    imp <- mice(data_inp(), seed = 1010, print = FALSE, method = input$met_imput)
    data_im <- complete(imp)
    output$show_imput <- renderPlot({
      ncol_na <- data.frame(vari = colSums(is.na(data_inp()))) %>%
        filter(vari > 0) %>%
        nrow()
      
      if (ncol_na < 7) {
        densityplot(imp)
      }
      else {
        return(NULL)
      }
    })
    print(data_im)
    return(data_im)
  })
  
  output$tbl <- renderDataTable({
    print(data_imput())
    ifelse(!is.null(data_imput()), df <- data_imput(), df <- data_inp())
    df
  })
  # # TODO
  # rv <- reactiveValues(current_plot = NULL)
  # current_plot <- reactive({
  #   rv$current_plot
  # })
  # 
  # # Namespace
  # ns <- session$ns
  # 
  # # TODO
  # output$actionSelected <- renderUI({
  #   switch(input$selectAction,
  #     "visualizeDist" = visualising_distributions_ui(ns("visualizeDist"), colnames(dataset()))
  #   )
  # })
  # 
  # # TODO
  # observeEvent(input$show, {
  #   rv$current_plot <- switch(input$selectAction,
  #     "visualizeDist" = callModule(visualising_distributions, "visualizeDist", dataset)
  #   )
  # })
  # 
  # # TODO
  # output$plot <- renderPlot({
  #   current_plot()
  # }, height = 650)
  
  data_imput
}
