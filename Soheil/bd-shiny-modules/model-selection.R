# SUBMODULES ###################################################################

source("bd-shiny-modules/model-selection-submodules/manual-configuration.R")
source("bd-shiny-modules/model-selection-submodules/import-configuration.R")

# SHINY UI #####################################################################

model_selection_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # To choose whether it's a classification or regression task
  classif_or_reg <- radioButtons(ns("classifOrReg"),
    label = "Choose a type of prediction problem",
    choices = c("Classification", "Regression")
  )

  # To choose whether we want to configure the process manually or with a file
  select_config_method <- radioButtons(ns("configMethod"),
    label = "Select a configuration method",
    choices = list(
      "Configure manually" = "manual",
      "Import a configuration file" = "file"
    )
  )

  # TODO
  confirm_button <- actionButton(ns("confirmConfig"), "Confirm configuration")

  # To select the target variable
  select_target_variable <- uiOutput(ns("targetVar"))

  # To indicate if we want to apply a one-hot encoding to the target variable
  do_one_hot <- radioButtons(ns("oneHot"), "Apply a one-hot encoding?",
    choices = c("Yes", "No"),
    selected = "No"
  )

  # To indicate whether we want to do a cross-validation process
  do_cv <- radioButtons(
    inputId = ns("doCV"),
    label = "Do you want to launch a cross-validation process?",
    choices = c("Yes", "No"),
    selected = "Yes"
  )

  # TODO
  download_config_button <- downloadButton(ns("dlConfig"), "Download configuration")

  # TODO
  refresh_json_button <- actionButton(ns("refreshJSON"), "Refresh")
  
  # TODO
  exec_button <- actionButton(ns("exec"), "Exécuter")

  # UI LAYOUTS #################################################################

  # First column (general configuration, hyperparameter space)
  first_column <- column(
    4,
    wellPanel(
      h4("General Configuration"),
      hr(),
      classif_or_reg,
      select_config_method,
      hr(),
      uiOutput(ns("configMethodSelected")),
      confirm_button
    ),
    wellPanel(
      h4("Resampling Strategy"),
      hr(),
      select_target_variable,
      do_one_hot,
      hr(),
      do_cv,
      uiOutput(ns("resampleDesc")),
      exec_button
    )
  )

  # Second column (hyperparameter space)
  second_column <- column(
    4,
    wellPanel(
      h4("Hyperparameter Space"),
      uiOutput(ns("hyperParams"))
    )
  )

  # Third column (cross-validation settings)
  third_column <- column(
    4,
    wellPanel(
      h4("JSON Configuration"),
      hr(),
      verbatimTextOutput(ns("jsonConfig")),
      refresh_json_button,
      download_config_button
    )
  )

  # Fluid row
  fluid_row <- fluidRow(
    first_column,
    second_column,
    third_column
  )

  # Return UI
  tagList(
    h3("Model Selection"),
    hr(),
    fluid_row,
    plotOutput(ns("plot_compare_algo")),
    uiOutput(ns("graph_algo"))
  )
}

# SERVER LOGIC #################################################################

model_selection <- function(input, output, session, dataset, algorithms_list,
                            algorithms_details, preprocessing_list) {
  # Namespace
  ns <- session$ns

  # List of reactive values
  rv <- reactiveValues(algorithms_selected = NULL, configuration = NULL)
  algorithms_selected <- reactive({
    rv$algorithms_selected
  })

  # Update the column names when choosing the target variable
  output$targetVar <- renderUI({
    selectInput(ns("targetVar"),
      "Choose a target variable",
      choices = colnames(dataset())
    )
  })

  # Render the correct UI according to the configuration method selected
  output$configMethodSelected <- renderUI({
    switch(input$configMethod,
      "manual" = manual_configuration_ui(ns("manual"), algorithms_list),
      "file" = import_configuration_ui(ns("file"))
    )
  })



  # Render the correct UI according to whether we want to do a CV or not
  output$resampleDesc <- renderUI({
    # To indicate the number of folds
    k_folds <- numericInput(
      inputId = ns("kFolds"),
      label = "Select a number of folds",
      value = 5,
      min = 2
    )

    # To indicate the proportion of the dataset to include in the train split
    train_size <- sliderInput(
      inputId = ns("trainSize"),
      label = "Indicate the proportion of the train split",
      min = 5,
      max = 95,
      value = 70
    )

    # Return the correct element
    switch(input$doCV,
      "Yes" = tagList(train_size, k_folds),
      "No" = train_size
    )
  })

  # Render the configuration UI according to the configuration method choosed
  observeEvent(input$confirmConfig, {
    if (input$configMethod == "manual") {
      rv$algorithms_selected <- callModule(manual_configuration, "manual")
    } else {
      rv$configuration <- callModule(import_configuration, "file")
      rv$algorithms_selected <- names(rv$configuration)
    }
    print(dataset())
  })

  # Render the UI to define the hyperparameter space
  output$hyperParams <- renderUI({
    lapply(algorithms_selected(), function(i) {
      algo_name <- algorithms_details[[i]]$name
      algo_id <- i
      tagList(
        hr(),
        h4(HTML(str_interp("${algo_name} | <code>${i}</code>"))),
        hr(),
        p(HTML(algorithms_details[[i]]$desc)),
        hr(),
        apply(algorithms_details[[i]]$parameters, 1, function(j) {
          par_id <- j["id"]
          par_name <- j["name"]
          par_type <- j["type"]
          textInput(
            ns(paste0(i, par_id)),
            HTML(
              str_interp("${par_name} (<code>${par_id}</code>, ${par_type})")
            )
          )
        }),
        selectizeInput(
          inputId = ns(paste0(i, "preprocess")),
          label = "Select preprocessing methods to apply in order",
          choices = preprocessing_list,
          multiple = TRUE
        )
      )
    })
  })

  # TODO
  observe({
    configuration <- rv$configuration
    algorithms <- names(configuration)
    for (a in algorithms) {
      for (hp in names(configuration[[a]]$tuneGrid)) {
        dput(dput(configuration[[a]]$tuneGrid[[hp]]), file = ".temp")
        value <- readLines(".temp", 1)
        updateTextInput(session, paste0(a, hp), value = value)
      }
      updateSelectizeInput(session, paste0(a, "preprocess"), selected = configuration[[a]]$preprocessing)
    }
  })

  # TODO
  output$jsonConfig <- renderText({
    prettify(toJSON(rv$configuration), indent = 2)
  })

  # TODO
  output$dlConfig <- downloadHandler(
    filename = function() {
      paste0("config", Sys.Date(), ".json")
    },
    content = function(file) {
      write_json(rv$configuration, file, pretty = TRUE)
    }
  )

  observeEvent(input$refreshJSON, {
    configuration <- list()
    for (a in algorithms_selected()) {
      configuration[[a]] <- list()
      configuration[[a]]$preprocessing <- input[[paste0(a, "preprocess")]]
      configuration[[a]]$tuneGrid <- list()
      for (hp in algorithms_details[[a]]$parameters$id) {
        value <- try(eval(parse(text=input[[paste0(a, hp)]])))
        if (!(class(value) %in% c("integer", "numeric", "character"))) value <- input[[paste0(a, hp)]]
        configuration[[a]]$tuneGrid[[hp]] <- value
        if (configuration[[a]]$tuneGrid[[hp]] == "") configuration[[a]]$tuneGrid[[hp]] <- NULL
      }
      if (length(configuration[[a]]$tuneGrid) == 0) configuration[[a]]$tuneGrid <- NULL
    }
    rv$configuration <- configuration
  })
  
  # TODO
  observeEvent(input$exec, {
    # TODO
    target <- input$targetVar
    df <- dataset()
    
    # Train set
    y_train <- as_vector(select(df, target))
    x_train <- select(df, -target)
    
    # One-hot encoding
    ifelse(input$oneHot == "Yes", compMat <- model.matrix(~ . - 1, x_train), compMat <- x_train)
    compMat <- cbind.data.frame(y = y_train, compMat)
    
    # Train/test split
    train_index <- sample(1:nrow(compMat), round(input$trainSize/100 * nrow(compMat)), replace = FALSE)
    compDf_train <- compMat[train_index, ]
    compDf_test <- compMat[-train_index,]

    ifelse(nrow(compDf_train) < 1000, pSubSample <- 0.1, pSubSample <- 0.01)
    classResp <- class(compDf_train$y)

    if (classResp == "numeric" || classResp == "integer"){
      mydata <- compDf_train %>%
        na.omit(.) %>%
        dplyr::sample_frac(pSubSample)

    } else {
      mydata <- compDf_train %>%
        na.omit(.) %>%
        dplyr::group_by(y) %>%
        dplyr::sample_frac(pSubSample) %>%
        dplyr::ungroup()
    }
    
    # TODO
    algo_choice <- algorithms_selected()

    #------------ Création de la liste des algo
    if (input$doCV == "Yes") {
      control <- trainControl(method = "cv", number = input$kFolds)
    } else {
      control <- trainControl()
    }
    
    x_train <- as.data.frame(mydata %>% select(-y))
    y_train <- mydata$y
    train_algo <- lapply(algo_choice, function (m) {
      train(x = x_train, y = y_train, trControl = control, preProcess = rv$configuration[[m]]$preprocessing, method = m, tuneGrid = expand.grid(rv$configuration[[m]]$tuneGrid))
    })

    # La sortie de train_algo est une liste contenant les résultats de chaque algo
    names(train_algo) <- algorithms_selected() # On attribut un nom à chaque liste
    cv_train <- train_algo

    #---- réactive pour détecter s'il y'a au moins un algo choisi
    choices_algo <- reactive({ # A ne surtout pas enlever
      if (is.null(algorithms_selected())) {
        return()
      }
      algo_reactive <- cv_train[algorithms_selected()]
      algo_reactive
    })

    #-------------- Affichage des résultats
    # Graphique pour comparer tous les algos
    output$plot_compare_algo <- renderPlot({
      if (length(choices_algo()) < 2) {
        return(NULL)
      }
      results <- resamples(choices_algo())
      scales <- list(x = list(relation = "free"), y = list(relation = "free"))
      ggplot(results, metric = results$metrics, colour = results$metrics, conf.level = .95, scales = scales) +
        theme_linedraw()
    })

    output$graph_algo <- renderUI({
      lapply(algorithms_selected(), function(i) {
        column(
          4,
          output[[paste0("c", i)]] <- renderPlotly({
            algo_result <- choices_algo()[[i]]$results
            position_accuracy <- which(names(algo_result) == "Accuracy")

            if (length(position_accuracy) == 0) {
              position_accuracy <- which(names(algo_result) == "RMSE")
            }

            df_metrics <- algo_result[, (position_accuracy - 1):(position_accuracy + 1)]
            df_metrics <- melt(df_metrics,
                               id.vars = names(df_metrics)[1],
                               variable.name = "metrics", value.name = "value_metrics"
            )
            p <- ggplot(df_metrics, aes(x = df_metrics[, 1], y = value_metrics, color = metrics)) +
              geom_point(col = ifelse(df_metrics[, 1] == get_best_result(choices_algo()[[i]])[, (position_accuracy - 1)],
                                      "red", "white"
              )) +
              geom_line() +
              labs(title = names(choices_algo()[i]), x = names(df_metrics)[1], y = " ") +
              scale_color_manual(values = mycolors)
            ggplotly(p) %>%
              layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
          })
        )
      })
    })
  })
}

get_best_result <- function(caret_fit) {
  best <- which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result <- caret_fit$results[best, ]
  rownames(best_result) <- NULL
  round(best_result, 2)
}

mycolors <- c("orange", "#0072B2","#990000", "#D55E00")
