shinyServer(function(input, output) {
  #Changement de capacité maximum de fichier de chargement 
  # x*1021^2 Mb
  # if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  # That way the max limit is 10GB when the app is run locally and 5MB when run from the server.
  options(shiny.maxRequestSize=30*1024^2)

	#Variable qui va stocker la sortie de la table (pour avoir un truc bien réactif qui ne se charge que quand on clique sur le bouton)
	data_inp <- eventReactive(input$load_data,{

		#Fichier d'entrée
		infile <- input$dataSet

		#Au cas où il n'y aurait pas de fichier d'entrée
		if (is.null(infile)){return(NULL)}

		#Lecture du fichier uploadé par l'utilisateur
		df <- read.csv(file = infile$datapath, header = input$header, sep = input$sep, quote = input$quote)
		return(df)

	})

	#Ce qui sera mis dans la table de données affichée
	output$contents <- renderTable({
		data_inp()
	})
  
})
