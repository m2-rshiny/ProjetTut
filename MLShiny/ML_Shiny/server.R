source("code.R")
shinyServer(function(input, output) {
   
    #-------------Variable à expliquer

    
    observeEvent(input$yvar,{
        responsesOfInterest <- input$yvar
        featuresOfInterest <-  paste0("X", 1:144)
        
        set.seed(123)
        compDf <- sqAnoDf %>%   # Notre data frame
            dplyr::select(responsesOfInterest[indResp], everything()) %>% 
            na.omit(.) %>% 
            dplyr::group_by(responsesOfInterest[indResp]) %>% 
            dplyr::sample_frac(pSubSample) %>%  
            dplyr::ungroup() 
        
        # Récupère la classe de la variable LABEL
        yTrain <- compDf[, responsesOfInterest[indResp]][[1]]

        compMat <- model.matrix(~ .-1, compDf[,-c(1, ncol(compDf))])

        #-----------------algo
        algo_choice <- isolate(input$algo)
        mydata <- data.frame(y = yTrain, compMat)
        
        
        control <- trainControl(method="cv", number=10)
        set.seed(123)
        form_algo <- list(
            'RF'=list(form=y ~ .,
                      data = mydata,
                      preProcess = c('scale','center'),
                      method = 'rf',
                      trControl = control,
                      tuneGrid=grid[['rf']],
                      ntree=500),
            'Knn'=list(form=y ~ .,
                       data = mydata,
                       preProcess = c('scale','center'),
                       method = 'knn',
                       trControl = control,
                       tuneGrid=grid[['knn']]),
            'Lasso'=list(form=y ~ .,
                         data = mydata,
                         preProcess = c('scale','center'),
                         method = 'glmnet',
                         trControl = control,
                         tuneGrid=grid[['glmnet']])
            
        )
        
        c1 <- makePSOCKcluster(nCores)
        registerDoParallel(c1)
        
        train_algo <- lapply(algo_choice,function(m){
            do.call('train',form_algo[[m]])
        })
        stopCluster(c1)
        names(train_algo) <- algo_choice
        cv_train <- train_algo
        # Comparaison des algo
        
        choices_algo <- reactive({
            if(is.null(input$algo)) {return ("Sélectionnez un algo")}
            algo_reactive <- cv_train[c(input$algo)]
            algo_reactive
        })
        
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
            #ggplot(choices_algo()[[1]]) +
            #    ggtitle(names(choices_algo())[1]) +
            #    scale_fill_manual(values = "orange")
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
            #ggplot(choices_algo()[[2]]) +
               # ggtitle(names(choices_algo())[2])
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
            #ggplot(choices_algo()[[3]]) +
              #  ggtitle(names(choices_algo())[3])
        })
  
    })
    
    
    
    
    
    
    
    
    #--------------- Data --------------#  
    output$Donnees <- renderText({
        sqAnoDf[1:30,1:10] %>% 
            kable("html", escape = FALSE) %>%
            kable_styling(font_size = 11, full_width = FALSE,
                          bootstrap_options = c("striped", "condensed", "bordered")) %>%
            scroll_box(width = "1350px", height = "600px")
    })
    

})
