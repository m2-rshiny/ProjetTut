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
    data_inp()[1:4,1:4]
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
  
  observe({
    updateSelectizeInput(session, "variable_desc", choices = names(data_inp()))
    # updateSelectizeInput(session, "xvar_desc",
    # choices = names(data_inp())[-which(names(data_inp())==input$yvar_desc)])
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
  
  # Imputation
  output$imputation <- renderUI({
    if (sum(is.na(data_inp())) != 0) {
      checkboxInput("imput", "Data Imputation", FALSE)
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
    return(data_im)
  })
  
  output$tbl <- renderTable({
    ifelse(!is.null(data_imput()), df <- data_imput(), df <- data_inp())
    df
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
          paste(j[[1]],eval(parse(text = paste0("input$", i, j[[1]]))), sep="=")
        })
        
        #On récupère le preprocessing choisi par l'utilisateur
        preproc <- eval(parse(text = paste0("input$", i, 'preprocess')))
        
        #Ajout de la valeur correspondante pour chaque paramètre de la df
        name <<- c(name, doc$label)
        preProc <<- c(preProc, paste(c("c(",paste(preproc, collapse= ","), ")"), collapse=""))
        trainMetric <<- c(trainMetric, " ")
        trainControl <<- c(trainControl, " ")
        tuneGrid <<- c(tuneGrid, paste(params, collapse= ","))
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
  
  ### Export of the data configuration (to csv)
  output$downloadConfig <- downloadHandler(
    filename = function() {
      paste("config-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(data_config(), file)
    }
  )
  
  # 4 ML Algorithme ---------------------------------------------------------
  
  #-------------- Pour réactualiser la variable y à expliquer
  observe({
    updateSelectizeInput(session, "yvar", choices = names(data_inp()))
  })
  
  #-------------- Comparaison des algo
  
  observeEvent(input$exec, {
    responsesOfInterest <- input$yvar # Récupère la valeur à prédire
    set.seed(7323)
    
    ifelse(input$exec_imput != 0, df <- data_imput(), df <- data_inp())
    
    # Récupère la variable à expliquer
    yTrain <- df[, responsesOfInterest[indResp]]
    
    # Réalise du one hot encoding sauf sur la variable à expliquer
    df_x <- select(df, -responsesOfInterest[indResp])
    compMat <- model.matrix(~ . - 1, df_x)
    
    # Partitionnement du dataframe.
    choice_cv <- reactive({
      
      compMat <- data.frame(y = yTrain, compMat)
      if (input$cv_y_n == "Oui") { # Si l'utilisateur veut faire une validation croisée
        
        train_index <- sample(1:nrow(compMat), round(.8 * nrow(compMat)), replace = FALSE)
        
        # train_index <- createDataPartition(df[[responsesOfInterest]],
        #                                   p = input$prop_learn, list = FALSE, times = 1)
        isolate({
          data_train <<- compMat[train_index, ]
          # data_test <<- compMat[-train_index,]
        })
      }
      else if (input$cv_y_n == "Non") {# Si les données ne sont pas imputées, on prend les données d'origine
        isolate({
          data_train <<- compMat %>% # Notre data frame
            dplyr::select(y, everything()) %>% # Sélectionne la colonne LABEL et les colonnes commençant par X
            na.omit(.) %>% # Enlève les lignes ayant des NA
            dplyr::group_by(y) %>% # Regroupe par rapport à la variable LABEL
            dplyr::sample_frac(pSubSample) %>% # Sélectionne pour chaque LABEL une proportion de 0.01
            dplyr::ungroup()
        })
      }
    })
    
    mydata <- choice_cv()
    class(mydata$y)
    
    #-----------------algo
    algo_choice <- isolate(input$algorithme)
    
    
    #------------ Création de la liste des algo
    control <- trainControl(method = "cv", number = input$k_folds)
    set.seed(7323)
    
    form_algo <- list()
    apply(data_config(), 1, function(x){
      preproc <- x[which(names(data_config())=="preProc")]
      tunegrid <- paste("expand.grid(", x[which(names(data_config())=="tuneGrid")], ")", sep="")
      method <- paste("'", x[which(names(data_config())=="method")], "'")
      expr1 <- paste("form_algo[['", x[which(names(data_config())=="method")], "']]", sep="")
      expr2 <- paste(
        "form = y ~ ." ,
        "data = mydata",
          paste("preProcess", preproc, sep=" = "),
        paste("method", method, sep=" = "),
        "trControl = control",
        paste("tuneGrid", tunegrid, sep=" = "),
        sep = ", " 
      )
      expr3 <- paste("list(", expr2, ")", sep="")
      expr <- paste(expr1, expr3, sep="<<-")
      eval(parse(text = expr))
    })
    
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
      if (length(choices_algo()) < 2) {
        return(NULL)
      }
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
  
  # Test ---------------------------------
  # Ce qui sera mis dans la table de données affichée
  output$contents_test <- renderTable({
    data_config()
  })
})
