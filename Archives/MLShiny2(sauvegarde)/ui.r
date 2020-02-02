library(shiny)

##### Traitements préliminaires au lancement de l'appli  --------------------------------------------------------

#Importation du fichier csv contenant la liste des algorithmes
train_conf <- read.csv("www/titanic.csv", header = TRUE)

##### Interface utilisateur (ici, j'ai pris un format prédéfini) --------------------------------------------------------
shinyUI(pageWithSidebar(
  
  # Texte de titre en haut de l'appli
  headerPanel("Tests algo"),
  
  #Contenu de la barre sur le côté
  sidebarPanel(
    
    #Importation d'un fichier extérieur
    fileInput(
      inputId = 'dataSet',
      label = 'Chemin du fichier (csv ou tsv)',
      #Types de fichiers acceptés
      accept = c('text/csv', '.csv')
    ),
    
    #Petite barre de séparation
    tags$hr(),
    
    #Bouton choix Header
    checkboxInput(
      inputId = 'header',
      label = 'Header',
      value = TRUE
    ),
    
    #Choix du séparateur
    radioButtons(
      inputId = 'sep',
      label = 'Separator',
      choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t'),
      selected = '\t'
    ),
    
    #Choix du charactère de citacion
    radioButtons(
      inputId = 'quote',
      label = 'Quote',
      choices = c('None' = '', '"' = '"', "'" = "'"),
      selected = "'"
    ),

    #Petite barre de séparation
    tags$hr(),
    
    #Choix des algos à utiliser
    radioButtons(
      inputId = 'algorithme',
      label = 'Algorithme',
      choices = train_conf$name
    ),
    
    #Petite barre de séparation
    tags$hr(),
    
    #Bouton pour lancer l'éxecution des algorythmes.
    actionButton("exec", "Executer")
  ),
  
  #Contenu au centre de l'appli
  mainPanel(
    
    #Tableau des données importées
    tableOutput(outputId = 'contents')
    
  )
  
))