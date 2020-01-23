#source("code.R")
##### Traitements préliminaires au lancement de l'appli  --------------------------------------------------------
#options(encoding = "UTF-8")
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
      		
      		### Onglet 1 : Importaion des données/choix des algos/choix des variables/exécution
      		tabPanel(
      			"Préparation algorithmes",
      			
      			#Layout de l'onglet
      			sidebarLayout(
      				
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
      					
      					#Bouton pour charger les données
      					actionButton("load_data", "Charger les données"),
      					
      					#Petite barre de séparation
      					tags$hr(),
      					
      					#Choix des algos à utiliser
      					checkboxGroupInput(
      						inputId = 'algorithme',
      						label = 'Algorithme',
      						choices = train_conf$name
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
      					tags$hr(),
      					
      					#Bouton pour lancer l'éxecution des algorithmes.
      					actionButton("exec", "Executer")
      				),
      				
      				#Contenu au centre de la page
      				mainPanel(
      					
      					#Tableau des données importées
      					tableOutput(outputId = 'contents')
      				)
      			)
      		),

        	tabPanel(
        		"ML Algorithme",
        		class = "pages",
                fluidRow(
                	column(2, checkboxGroupInput("algo")), # colonne pour choisir les algo
                    column(5, plotOutput("compare")), # colonne pour ploter les algo
                    column(5, verbatimTextOutput("value"))  # colonne pour afficher les summary
				),
				fluidRow(
                	column(4, plotOutput("lasso")), # graphe lasso
                    column(4, plotOutput("ridge")), # graphe ridge
                    column(4, plotOutput("boruta"))#, # graph boruta
                    #sliderInput("nrowBoruta", "Variables importantes:", 1, nrowBoruta, inputBoruta))
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