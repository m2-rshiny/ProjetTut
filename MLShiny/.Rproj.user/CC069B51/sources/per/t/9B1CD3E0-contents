library(shiny)

shinyServer(function(input, output) {
  
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