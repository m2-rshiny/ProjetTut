source("scripts/code.R")
shinyServer(function(input, output) {

  #-------------Affichage données dans la l'onglet "Préparation des données"
  #--------------- Data --------------#  
  # output$Donnees <- renderText({
  #    sqAnoDf[1:30,1:10] %>%
  #        kable("html", escape = FALSE) %>%
  #        kable_styling(font_size = 11, full_width = FALSE,
  #                      bootstrap_options = c("striped", "condensed", "bordered")) %>%
  #        scroll_box(width = "1350px", height = "600px")
  # })

  # Variable qui va stocker la sortie de la table (pour avoir un truc bien réactif qui ne se charge que quand on clique sur le bouton)
  data_inp <- eventReactive(input$load_data, {

    # Fichier d'entrée
    infile <- input$dataSet

    # Au cas où il n'y aurait pas de fichier d'entrée
    if (is.null(infile)) {
      return(NULL)
    }

    # Lecture du fichier uploadé par l'utilisateur
    df <- read.csv(file = infile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })

  # Ce qui sera mis dans la table de données affichée
  output$contents <- renderTable({
    data_inp()
  })



  #-------------Variable à expliquer



  observeEvent(input$exec, {
    responsesOfInterest <- input$yvar
    featuresOfInterest <- paste0("X", 1:144)

    set.seed(7323)
    compDf <- sqAnoDf %>% # Notre data frame
      # dplyr::filter(!is.na(delta) & delta >= 0) %>%
      dplyr::select(responsesOfInterest[indResp], everything()) %>% # Sélectionne la colonne LABEL et les colonnes commençant par X
      na.omit(.) %>% # Enlève les lignes ayant des NA
      dplyr::group_by(responsesOfInterest[indResp]) %>% # Regroupe par rapport à la variable LABEL
      dplyr::sample_frac(pSubSample) %>% # Sélectionne pour chaque LABEL une proportion de 0.01
      dplyr::ungroup()

    # Récupère la classe de la variable LABEL
    classResp <- class(compDf[, responsesOfInterest[indResp]][[1]])
    yTrain <- compDf[, responsesOfInterest[indResp]][[1]]

    test <- compDf[, -c(1, ncol(compDf))]
    compMat <- model.matrix(~ . - 1, test)

    #-----------------algo
    algo_choice <- isolate(input$algo)
    mydata <- data.frame(y = yTrain, compMat)


    control <- trainControl(method = "cv", number = 10)
    set.seed(7323)
    form_algo <- list(
      "RF" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "rf",
        trControl = control,
        tuneGrid = grid[["rf"]],
        ntree = 500
      ),
      "Knn" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "knn",
        trControl = control,
        tuneGrid = grid[["knn"]]
      ),
      "Lasso" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "glmnet",
        trControl = control,
        # family="multinomial",
        tuneGrid = grid[["glmnet"]]
      )
    )

    c1 <- makePSOCKcluster(nCores)
    registerDoParallel(c1)

    train_algo <- lapply(algo_choice, function(m) {
      do.call("train", form_algo[[m]])
    })
    stopCluster(c1)
    names(train_algo) <- algo_choice
    cv_train <- train_algo
    # Comparaison des algo

    choices_algo <- reactive({
      if (is.null(input$algo)) {
        return("Sélectionnez un algo")
      }
      algo_reactive <- cv_train[c(input$algo)]
      algo_reactive
    })

    output$best_param1 <- renderText({
      if (is.null(choices_algo())) {
        return(NULL)
      }
      get_best_result(choices_algo()[[1]]) %>%
        kable("html", escape = FALSE, align = "r") %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("striped", "condensed", "bordered")
        )
    })

    output$algo1 <- renderPlot({
      if (is.null(choices_algo())) {
        return(NULL)
      }
      ggplot(choices_algo()[[1]]) +
        ggtitle(names(choices_algo())[1]) +
        scale_fill_manual(values = "orange")
    })


    output$best_param2 <- renderText({
      if (length(choices_algo()) < 2) {
        return(NULL)
      }
      get_best_result(choices_algo()[[2]]) %>%
        kable("html", escape = FALSE, align = "r") %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("striped", "condensed", "bordered")
        )
    })
    output$algo2 <- renderPlot({
      if (length(choices_algo()) < 2) {
        return(NULL)
      }
      ggplot(choices_algo()[[2]]) +
        ggtitle(names(choices_algo())[2])
    })


    output$best_param3 <- renderText({
      if (length(choices_algo()) < 3) {
        return(NULL)
      }
      get_best_result(choices_algo()[[3]]) %>%
        kable("html", escape = FALSE, align = "r") %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("striped", "condensed", "bordered")
        )
    })
    output$algo3 <- renderPlot({
      if (length(choices_algo()) < 3) {
        return(NULL)
      }
      ggplot(choices_algo()[[3]]) +
        ggtitle(names(choices_algo())[3])
    })
  })









})
