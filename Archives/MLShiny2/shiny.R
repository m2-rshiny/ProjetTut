source("code.R")

ui <- fluidPage(theme = shinytheme("simplex"),
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                navbarPage(title = "Machine Learning BD",
                           position = "static-top", responsive = TRUE,
                           
                           tabPanel("ML Algorithme", class = "pages",
                                    fluidRow(
                                      column(2,
                                             checkboxGroupInput("algo", "Algorithmes:", choices = algo, selected = algo)), # colonne pour choisir les algo
                                      column(5, plotOutput("compare")), # colonne pour ploter les algo
                                      column(5, verbatimTextOutput("value"))  # colonne pour afficher les summary
                                    ),
                                    
                                    fluidRow(
                                        column(4, plotOutput("lasso")), # graphe lasso
                                        
                                        column(4, plotOutput("ridge")), # graphe ridge
                                        
                                        column(4, plotOutput("boruta"), # graph boruta
                                               sliderInput("nrowBoruta", "Variables importantes:", 1, nrowBoruta, inputBoruta))
                                        
                                    )),
                           
                           # On pourra mettre des stat desc ou autre chose. Pour l'instant y'a rien dedans
                           tabPanel("Stat Desc", class = "pages",
                                    fluidRow(
                                      column(4,),
                                      column(4,),
                                      column(4,)
                                    ),
                                    
                                    ),
                           # Données utilisées
                           tabPanel("Data", htmlOutput(outputId = "Donnees"))
                           
                ))



#---------------- Server --------------------#
server <- function(input, output) {
  algo_choice <- reactive({
    switch (input$algo,
            "Tree" = fit.tree,
            "Knn" = fit.knn,
            "RF" = fit.rf,
            "Lasso" = fit.lass,
            "Ridge" = fit.ridg,
            "SVM" = fit.svm
    )
  })
  # Comparaison des algo
  output$compare <- renderPlot({
    scales <- list(x=list(relation="free"), y=list(relation="free"))
    bwplot(results, scales=scales)
  })
  #summary des algo
  output$value <- renderPrint({ 
    # la réactive doit être ici
    results <- resamples(list(LASSO=fit.lass, RIDGE=fit.ridg, SVM=fit.svm, 
                              KNN=fit.knn, TREE = fit.arbre, RF=fit.rf))
    summary(results)
  })
  
  # graphe lasso
  output$lasso <- renderPlot({
    ggplot(tidy.lasso, aes(lambda, estimate)) +
      geom_point(color = "red") +
      scale_x_log10() + 
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
      geom_vline(xintercept = glance.lasso$lambda.min, lty = 2) +
      geom_vline(xintercept = glance.lasso$lambda.1se, lty = 2)
  })
  
  # graphe ridge
  output$ridge <- renderPlot({
    ggplot(tidy.ridge, aes(lambda, estimate)) +
      geom_point(color = "red") +
      scale_x_log10() + 
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
      geom_vline(xintercept = glance.ridge$lambda.min, lty = 2) +
      geom_vline(xintercept = glance.ridge$lambda.1se, lty = 2)
  })
  
  # graphe boruta
  output$boruta <- renderPlot({
    resBorutaImpDf %>%
      arrange(Decision) %>% 
      slice(1:input$nrowBoruta) %>% 
      dplyr::mutate(Feature = factor(Feature,levels = xLevels)) %>% 
      ggplot(aes(x = Feature, y = Importance, fill = Decision)) +
      geom_boxplot() +
      scale_fill_manual(values = c("green", "yellow", "red", "blue"), drop = FALSE) +
      coord_flip() +
      theme(axis.text.x = element_text(size = 8,hjust = 1,vjust = 1)) +
      ggtitle("Output of Boruta Algorithm") 
  })
  
  #--------------- Data --------------#  
  output$Donnees <- renderText({
    sqAnoDf[,1:30] %>% 
      kable("html", escape = FALSE, caption = "Données de 56 x 60") %>%
      kable_styling(font_size = 11, full_width = FALSE,
                    bootstrap_options = c("striped", "condensed", "bordered")) %>%
      scroll_box(width = "1350px", height = "600px")
  })
  
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)