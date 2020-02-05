source("scripts/code.R")
shinyServer(function(input, output, session) {

  #-------------Affichage données dans la l'onglet "Préparation des données"

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
    data_inp()[1:20, 1:5]
  })

  output$input_type_text <- renderText({
    input$input_type
  })

  output$dynamic_value <- renderPrint({
    input$dynamic
  })


  # 2 ML Parametres -----------------------------------------------------------

  #-------------Affichage automatique pour le choix des paramètres des algo
  # Aide https://stackoverflow.com/questions/19130455/create-dynamic-number-of-input-elements-with-r-shiny

  output$params <- renderUI({
    #Recherche du nombre d'algos séléctionnés
    nb_algos <- length(input$algorithme)
        
    #Fonction qui créé le bon nombre d'inputs en fonction de ses paramètres
    lapply(1:nb_algos, function(i) {
      textInput(paste0("ind", i), length(input$algorithme))
    })
    #textInput("label", length(input$algorithme))
    
    
  })
  
  # 3 ML Algorithme ---------------------------------------------------------

  #-------------- Pour réactualiser la variable y à expliquer
  observe({
    updateSelectizeInput(session, "yvar", choices = names(data_inp()))
  })

  #-------------- Comparaison des algo
  
  
  observeEvent(input$exec, {
    responsesOfInterest <- input$yvar
    set.seed(7323)
    compDf <- data_inp() %>% # Notre data frame
      # dplyr::filter(!is.na(delta) & delta >= 0) %>%
      dplyr::select(responsesOfInterest[indResp], everything()) %>% # Sélectionne la colonne LABEL et les colonnes commençant par X
      na.omit(.) %>% # Enlève les lignes ayant des NA
      dplyr::group_by(responsesOfInterest[indResp]) %>% # Regroupe par rapport à la variable LABEL
      dplyr::sample_frac(pSubSample) %>% # Sélectionne pour chaque LABEL une proportion de 0.01
      dplyr::ungroup()
    
    # Récupère la classe de la variable LABEL
    yTrain <- compDf[, responsesOfInterest[indResp]][[1]]
    classResp <- class(yTrain)
    classResp_level <- length(unique(yTrain))
    
    if(classResp == 'integer' && classResp_level < 10) {
      yTrain <- factor(yTrain)
    }
    test <- compDf[, -c(1, ncol(compDf))]
    compMat <- model.matrix(~ . - 1, test)
    
    #-----------------algo
    algo_choice <- isolate(input$algo)
    mydata <- data.frame(y = yTrain, compMat)
    
    #------------ Création de la liste des algo
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

    c1 <- makePSOCKcluster(nCores) # détecte le nombre de coeur de l'ordi pour paralliser les exécution du train()
    registerDoParallel(c1)

    #--------train() de tous les algo présents dans la liste
    train_algo <- lapply(algo_choice, function(m) {
      do.call("train", form_algo[[m]])
    })
    stopCluster(c1) # Fermeture du cluster pour arreter le parallélisme

    # La sortie de train_algo est une liste contenant les résultats de chaque algo
    names(train_algo) <- algo_choice # On attribut un nom à chaque liste
    cv_train <- train_algo

    #---- réactive pour détecter s'il y'a au moins un algo choisi
    choices_algo <- reactive({
      if (is.null(input$algo)) {
        return()
      }
      algo_reactive <- cv_train[c(input$algo)]
      algo_reactive
    })

    #-------------- Affichage des résultats
    output$best_param1 <- renderText({
      if(is.null(choices_algo())){return(NULL)}
      get_best_result(choices_algo()[[1]])[1:3] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
      
    })
    
    output$algo1 <- renderPlotly({
      if(is.null(choices_algo())){return(NULL)}
      metrics <- choices_algo()[[1]]$results[,1:3]
      metrics <- melt(metrics, id.vars="mtry",variable.name = "Accuracy",value.name="Kappa")
      p <-  ggplot(metrics, aes(x = mtry, y = value, color = variable)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Random Forest", x = "mtry", y = "Cross-Validation") +
        theme(legend.position = "top") +
        scale_color_manual(values = mycolors)
      ggplotly(p)%>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    })
    
    
    output$best_param2 <- renderText({
      if(length(choices_algo())<2){return(NULL)}
      get_best_result(choices_algo()[[2]])[1:3] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
      
    })
    output$algo2 <- renderPlotly({
      if(length(choices_algo())<2){return(NULL)}
      metrics <- choices_algo()[[2]]$results[,1:3]
      metrics <- melt(metrics, id.vars="k",variable.name = "Accuracy",value.name="Kappa")
      p <-  ggplot(metrics, aes(x = k, y = value, color = variable)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Knn", x = "k", y = " ") +
        theme(legend.position = "none") +
        scale_color_manual(values = mycolors)
      ggplotly(p)
    })
    
    
    output$best_param3 <- renderText({
      if(length(choices_algo())<3){return(NULL)}
      get_best_result(choices_algo()[[3]])[2:4] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
      
    })
    output$algo3 <- renderPlotly({
      if(length(choices_algo())<3){return(NULL)}
      metrics <- choices_algo()[[3]]$results[,2:4]
      metrics <- melt(metrics, id.vars="lambda",variable.name = "Accuracy",value.name="Kappa")
      p <-  ggplot(metrics, aes(x = lambda, y = value, color = variable)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Lasso", x = "lambda", y = " ") +
        theme(legend.position = "none") +
        scale_color_manual(values = mycolors)
      ggplotly(p)
    })
  })








  #--------------- Data --------------#  
  # output$Donnees <- renderText({
  #    sqAnoDf[1:30,1:10] %>%
  #        kable("html", escape = FALSE) %>%
  #        kable_styling(font_size = 11, full_width = FALSE,
  #                      bootstrap_options = c("striped", "condensed", "bordered")) %>%
  #        scroll_box(width = "1350px", height = "600px")
  # })
})
