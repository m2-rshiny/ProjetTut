shinyServer(function(input, output) {
  #Changement de capacité maximum de fichier de chargement 
  # x*1021^2 Mb
  # if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  # That way the max limit is 10GB when the app is run locally and 5MB when run from the server.
  options(shiny.maxRequestSize=30*1024^2)
  
  observeEvent(eventExpr=input$exec, autoDestroy=TRUE, handlerExpr = {
    
    #Ce qui sera mis dans la table de données affichée
    output$contents <- renderTable({
      
      #Fichier d'entrée
      infile <- input$dataSet
      
      #Au cas où il n'y aurait pas de fichier d'entrée
      if (is.null(infile))
        return(NULL)
      
      #Lecture du fichier uploadé par l'utilisateur
      read.csv(
        file = infile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      
    })
    
  })
  
})