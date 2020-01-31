#Appel des packages dont nous avons besoin pour ce script
source("scripts/packages.R")

#Importation du fichier csv contenant la liste des algorithmes
train_conf <- read.csv("train_conf.csv", sep=";", header=TRUE)

##### Interface utilisateur (ici, j'ai pris un format prédéfini) --------------------------------------------------------	
shinyUI(
   fluidPage(
      theme = shinytheme("simplex"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      
      #Nous avons une page avec plusieur onglets
      navbarPage(
         title = "Machine Learning BD",
         position = "static-top", responsive = TRUE,
         
         ### Onglet 1 : Importation des données
         tabPanel(
            "Préparation données",
            
            #Layout de l'onglet
            sidebarLayout(
               
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
                  
                  #Bouton pour charger les données
                  actionButton("load_data", "Charger les données"),
                  
                  #Petite barre de séparation
                  tags$hr()
                  
               ),
               
               #Contenu au centre de la page
               mainPanel(
                  
                  #Tableau des données importées
                  tableOutput(outputId = 'contents')
               )
            )
         ),
         
         ### Onglet 2 : Paramétrage des algorithmes
         tabPanel(
            "ML Algorithme",
            class = "pages",
            fluidRow(
               # colonne pour ploter les algo
               column(12,

                  #Choix des algos à utiliser
                  selectizeInput(
                     inputId = 'algorithme',
                     label = 'Algorithmes',
                     choices = 
                  ),
                  checkboxGroupInput(
                     inputId = 'algorithme',
                     label = 'Algorithme',
                     choices = c()
                  ),
                      
                  #Choix de la proportion d'apprentissage
                  sliderInput(
                     inputId = "prop_learn",
                     label = "Proportion de données d'apprentissage",
                     min = 5,
                     max = 95,
                     value = 70
                  ),
                      
                  #Petite barre de séparation
                  #tags$hr(),
                      
                  #Bouton pour lancer l'éxecution des algorithmes.
                  actionButton("exec", "Executer")
               )
            )
         ),
         
            # On pourra mettre des stat desc ou autre chose. Pour l'instant y'a rien dedans
            tabPanel(
            	"Stat Desc",
            	class = "pages",
                fluidRow(
                	column(4,),
                    column(4,),
                    column(4,)
				),
                              
			),
            # Données utilisées
            tabPanel("Data", htmlOutput(outputId = "Donnees"))
		)
	)
)