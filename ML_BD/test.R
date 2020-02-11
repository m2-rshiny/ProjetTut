output$best_param1 <- renderText({
    if(is.null(choices_algo())){return(NULL)}
    get_best_result(choices_algo()[[1]])[1:3] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
    
})


output$best_param2 <- renderText({
    if(length(choices_algo())<2){return(NULL)}
    get_best_result(choices_algo()[[2]])[1:3] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
    
})


output$best_param3 <- renderText({
    if(length(choices_algo())<3){return(NULL)}
    get_best_result(choices_algo()[[3]])[2:4] %>% 
        kable("html", escape = FALSE, align = 'r') %>%
        kable_styling(full_width = FALSE,
                      bootstrap_options = c("striped", "condensed", "bordered"))
    
})



fluidRow(
    column(2),
    # colonne 1 pour afficher les graphiques des algo
    column(
        3,
        html1, #Texte
        graphique1 #Graphique
    ),
    # colonne 2 pour afficher les graphiques des algo
    column(
        3,
        html2, #Texte
        graphique2 #Graphique
    ),
    # colonne 3 pour afficher les graphiques des algo
    column(
        4,
        html3, #Texte
        graphique3 #Graphique
    )
)





output$algo1 <- renderPlotly({
    if(is.null(choices_algo())){return(NULL)}
    df_metrics <- choices_algo()[[1]]$results[,1:3]
    df_metrics <- melt(df_metrics, id.vars="mtry",variable.name = "metrics",value.name="value_metrics")
    p <-  ggplot(df_metrics, aes(x = mtry, y = value_metrics, color = metrics)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Random Forest", x = "mtry", y = "Cross-Validation") +
        theme(legend.position = "top") +
        scale_color_manual(values = mycolors)
    ggplotly(p)%>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
})


output$algo2 <- renderPlotly({
    if(length(choices_algo())<2){return(NULL)}
    df_metrics <- choices_algo()[[2]]$results[,1:3]
    df_metrics <- melt(df_metrics, id.vars="k",variable.name = "metrics",value.name="value_metrics")
    p <-  ggplot(df_metrics, aes(x = k, y = value_metrics, color = metrics)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Knn", x = "k", y = " ") +
        theme(legend.position = "none") +
        scale_color_manual(values = mycolors)
    ggplotly(p)
})



output$algo3 <- renderPlotly({
    if(length(choices_algo())<3){return(NULL)}
    df_metrics <- choices_algo()[[3]]$results[,2:4]
    df_metrics <- melt(df_metrics, id.vars="lambda",variable.name = "metrics",value.name="value_metrics")
    p <-  ggplot(df_metrics, aes(x = lambda, y = value_metrics, color = metrics)) + 
        geom_point() +
        geom_line() + 
        labs(title = "Lasso", x = "lambda", y = " ") +
        theme(legend.position = "none") +
        scale_color_manual(values = mycolors)
    ggplotly(p)
})



output$graph_algo <- renderUI({
    lapply(input$algorithme, function(i) {
        column(
            3,
            apply(doc$parameters, 1, function(j) {
                textInput(paste0("algos", i, j[1]), j[1], placeholder = j[2], width = "100%") # Affichage du textinput correspondant
            })
        )
    })
})

