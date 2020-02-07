source("scripts/code.R")
shinyServer(function(input, output, session) {
  
  #Changement de capacité maximum de fichier de chargement
  options(shiny.maxRequestSize=30*1024^2)

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
    data_inp()
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
    #Fonction qui créé le bon nombre d'inputs en fonction de ses paramètres
    lapply(input$algorithme, function(i) {
      
      #Information à propos du modèle
      doc <- getModelInfo(model = i, regex = FALSE)[[1]]
      
      #Partie ui
      column(
        4,
        h4(i), #Titre avec nom de l'algo
        apply(doc$parameters, 1, function(j){
          textInput(paste0(i, j[1]), j[1], placeholder=j[2], width = "100%") #Affichage du textinput correspondant
        })
      )
    })
    
  })
  
  observe({
    updateTextInput(session, inputId = "params", placeholder="test")
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
    
})
