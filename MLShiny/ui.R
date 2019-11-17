library(shiny)

# Interface utilisateur (ici, j'ai pris un format prédéfini)
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
    )
    
  ),
  
  #Contenu au centre de l'appli
  mainPanel(
    
    #Tableau des données importées
    tableOutput(outputId = 'contents')
    
  )
  
))