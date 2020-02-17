source("scripts/code.R")
shinyServer(function(input, output, session) {
  
  #Changement de capacité maximum de fichier de chargement
  options(shiny.maxRequestSize=30*1024^2)
  
  # 1 Chargement des données -----------------------------------------------------------

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
  
  # 2 Stat Desc -----------------------------------------------------------
  
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
  
  #  output$data_desc <- renderDiagonalNetwork({
  #    pl_str <- plot_str(data_inp())
  #    diagonalNetwork(pl_str)
  #  })
  
  observe({
    updateSelectizeInput(session, "variable_desc", choices = names(data_inp()))
    # updateSelectizeInput(session, "xvar_desc", choices = names(data_inp())[-which(names(data_inp())==input$yvar_desc)])
  })
  
  output$plt_variable_desc <- renderPlotly({
    y <- data_inp()[, input$variable_desc]
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

  # 3 ML Parametres -----------------------------------------------------------

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
        #Ajout des différents paramètres pris par l'algorithme
        apply(doc$parameters, 1, function(j){
          textInput(paste0(i, j[[1]]), j[[1]], placeholder=j[[2]], width = "100%") #Affichage du textinput correspondant
        }),
        #Ajoute du choix des preprocessing
        selectizeInput(
          inputId = paste0(i, 'preprocess'),
          label = 'Choice preprocessing',
          choices = c(
            "'BoxCox'",
            "'YeoJohnson'",
            "'expoTrans'",
            "'center'",
            "'scale'",
            "'range'",
            "'knnImpute'",
            "'bagImpute'",
            "'medianImpute'",
            "'pca'",
            "'ica'",
            "'spatialSign'",
            "'corr'",
            "'zv'",
            "'nzv'",
            "'conditionalX'"
          ),
          multiple = TRUE
        )
      )
    })
  })
      
  observe({
    updateTextInput(session, inputId = "algos11")
  })

  # Création de la df de configuration,
  # Soit le fichier de config uploadé par l'utilisateur
  # Soit les paramètres entrés par l'utilisateur
  data_config <- eventReactive(input$load_config, {
    
    # S'il n'y a pas de fichier de configuration, on utilise les paramètres de l'utilisateur
    if (input$choix_config == "Manuelle") {
      
      #Initialisation des variables de la future dataframe contenant la configuration
      name <- vector()
      preProc <- vector()
      trainMetric <- vector()
      trainControl <- vector()
      tuneGrid <- vector()
      
      #Remplissage des vecteurs
      sapply(input$algorithme, function(i){
        
        #Récupération des infos du modèle
        doc <- getModelInfo(model = i, regex = FALSE)[[1]]
        
        #On récupère les paramètres entrés par l'utilisateur pour chaque algo
        params <- apply(doc$parameters, 1, function(j){
          eval(parse(text = paste0("input$", i, j[[1]])))
        })
        
        #On récupère le preprocessing choisi par l'utilisateur
        preproc <- eval(parse(text = paste0("input$", i, 'preprocess')))
        
        #Ajout de la valeur correspondante pour chaque paramètre de la df
        name <<- c(name, doc$label)
        preProc <<- c(preProc, paste(c("c(",paste(preproc, collapse= ","), ")"), collapse=""))
        trainMetric <<- c(trainMetric, " ")
        trainControl <<- c(trainControl, " ")
        tuneGrid <<- c(tuneGrid, paste(c("{",paste(params, collapse= ","), "}"), collapse=""))
      })
      
      method = input$algorithme
      return(data.frame(name, method, preProc, trainMetric, trainControl, tuneGrid))
      #return(NULL)
    }
    
    # Fichier d'entrée
    infile_config <- input$dataConfig
    
    # Lecture du fichier uploadé par l'utilisateur
    df_config <- read.csv(file = infile_config$datapath, header = TRUE, sep = ";")
    return(df_config)
  })
  
  # 4 ML Algorithme ---------------------------------------------------------
  
  #-------------- Pour réactualiser la variable y à expliquer
  observe({
    updateSelectizeInput(session, "yvar", choices = names(data_inp()))
  })
  
  #-------------- Comparaison des algo
  
  
  
  observeEvent(input$exec, {
    responsesOfInterest <- input$yvar # Récupère la valeur à prédire
    set.seed(7323)
    
    # Partitionnement du dataframe.
    # Si l'utilisateur veut faire une validation croisée, la dataframe est divisée selon la proportion qu'il a choisi
    choice_cv <- reactive({
      df <- data_inp()
      if (input$cv_y_n == "Oui") {
        train_index <- sample(1:nrow(df), round(.8 * nrow(df)), replace = FALSE)
        
        # train_index <- createDataPartition(df[[responsesOfInterest]],
        #                                   p = input$prop_learn, list = FALSE, times = 1)
        isolate({
          data_train <<- df[train_index, ]
          # data_test <<- df[-train_index,]
        })
      }
      else if (input$cv_y_n == "Non") {
        isolate(data_train <<- df)
      }
    })
    
    compDf <- choice_cv() %>% # Notre data frame
      # dplyr::filter(!is.na(delta) & delta >= 0) %>%
      dplyr::select(responsesOfInterest[indResp], everything()) %>% # Sélectionne la colonne LABEL et les colonnes commençant par X
      na.omit(.) %>% # Enlève les lignes ayant des NA
      dplyr::group_by(responsesOfInterest[indResp]) %>% # Regroupe par rapport à la variable LABEL
      dplyr::sample_frac(pSubSample) %>% # Sélectionne pour chaque LABEL une proportion de 0.01
      dplyr::ungroup()
    
    # Récupère la classe de la variable LABEL
    yTrain <- compDf[, responsesOfInterest[indResp]][[1]]
    classResp <- class(yTrain)
    # classResp_level <- length(unique(yTrain))
    
    # if(classResp == 'integer' && classResp_level < 10) {
    #  yTrain <- factor(yTrain)
    # }
    test <- compDf[, -c(1, ncol(compDf))]
    compMat <- model.matrix(~ . - 1, test)
    
    #-----------------algo
    algo_choice <- isolate(input$algorithme)
    mydata <- data.frame(y = yTrain, compMat)
    
    #------------ Création de la liste des algo
    control <- trainControl(method = "cv", number = input$k_folds)
    set.seed(7323)
    form_algo <- list(
      "adaboost" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "adaboost",
        trControl = control,
        tuneGrid = grid[["adaboost"]]
      ),
      "AdaBoost.M1" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "AdaBoost.M1",
        trControl = control,
        tuneGrid = grid[["AdaBoost.M1"]]
      ),
      "rpart" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "rpart",
        trControl = control,
        tuneGrid = grid[["rpart"]]
      ),
      "ranger" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "ranger",
        trControl = control,
        tuneGrid = grid[["ranger"]]
      ),
      "rf" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "rf",
        trControl = control,
        tuneGrid = grid[["rf"]],
        ntree = 500
      ),
      "knn" = list(
        form = y ~ .,
        data = mydata,
        preProcess = c("scale", "center"),
        method = "knn",
        trControl = control,
        tuneGrid = grid[["knn"]]
      ),
      "glmnet" = list(
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
    names(train_algo) <- input$algorithme # On attribut un nom à chaque liste
    cv_train <- train_algo
    
    #---- réactive pour détecter s'il y'a au moins un algo choisi
    choices_algo <- reactive({ # A ne surtout pas enlever
      if (is.null(input$algorithme)) {
        return()
      }
      algo_reactive <- cv_train[c(input$algorithme)]
      algo_reactive
    })
    
    #-------------- Affichage des résultats
    # Graphique pour comparer tous les algos
    output$plot_compare_algo <- renderPlot({
      results <- resamples(choices_algo())
      scales <- list(x = list(relation = "free"), y = list(relation = "free"))
      ggplot(results, metric = results$metrics, colour = results$metrics, conf.level = .95, scales = scales) +
        theme_linedraw()
    })
    
    output$graph_algo <- renderUI({
      lapply(input$algorithme, function(i) {
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
              geom_point(col = ifelse(df_metrics[,1] == get_best_result(choices_algo()[[i]])[, (position_accuracy-1)],
                                      "red", "white")) +
              geom_line() +
              labs(title = names(choices_algo()[i]), x = names(df_metrics)[1], y = " ") +
              theme(legend.position = "none") +
              scale_color_manual(values = mycolors)
            ggplotly(p)
          })
        )
      })
    })
  })
  
  # Test ---------------------------------
  # Ce qui sera mis dans la table de données affichée
  output$contents_test <- renderTable({
    data_config()
  })
})
