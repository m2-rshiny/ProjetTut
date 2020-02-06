source("scripts/code.R")
source("scripts/chargements.R")
# LIBRARIES ###################################################################



# ELEMENTS ####################################################################

### Onglet 1 : Importation d'un fichier extérieur -----

# Module d'importation
import_fich <- fileInput(
    inputId = "dataSet",
    label = "Chemin du fichier (csv ou tsv)",
    # Types de fichiers acceptés
    accept = c("text/csv", ".csv")
)

# Bouton choix Header
header_but <- checkboxInput(
    inputId = "header",
    label = "Header",
    value = TRUE
)

# Choix du séparateur
sep_but <- radioButtons(
    inputId = "sep",
    label = "Separator",
    choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
    selected = "\t"
)

# Choix du charactère de citacion
cit_but <- radioButtons(
    inputId = "quote",
    label = "Quote",
    choices = c("None" = "", '"' = '"', "'" = "'"),
    selected = "'"
)

# Bouton pour charger les données
load_but <- actionButton("load_data", "Charger les données")

# Tableau des données importées
table_data <- tableOutput(outputId = "contents")



### Onglet 2 : Choix des algos -----

#choix variable à expliquer
choix_y <- selectInput("yvar", "Variable Y:", choices = var_response)

#Choix de la proportion d'apprentissage
p_learn <- sliderInput(
    inputId = "prop_learn",
    label = "Proportion de données d'apprentissage",
    min = 5,
    max = 95,
    value = 70
)

#Choix si cross validation ou non
choix_cv <- radioButtons(
    inputId = "cv_y_n",
    label = "Réaliser une validation croisée ?",
    choices = c("Oui", "Non"),
    selected = "Non"
)

#Nombre de k-folds
k_folds <- numericInput(
    inputId = "k_folds",
    label = "Nombre k de partitions",
    value = 5,
    min = 2
) 


#Bouton pour lancer l'execution des algorithmes
exec_algo <- actionButton("exec", "Executer")

#Graphique 1
html1 <- htmlOutput("best_param1")
graphique1 <- plotOutput("algo1")

#Graphique 2
html2 <- htmlOutput("best_param2")
graphique2 <- plotOutput("algo2")

#Graphique 3
html3 <- htmlOutput("best_param3")
graphique3 <- plotOutput("algo3")




### Onglet 4 : Paramètres des algos

# Choix méthodes
choix_methode <- radioButtons("class_reg", "Choisir une méthode:",
  choices = c("Classification", "Régression")
)

# Choix algo
choix_algo <- selectizeInput(
    inputId = 'algorithme',
    label = 'Algorithme',
    choices = algos$method,
    multiple = TRUE
)

# Affichage des algo pour insérer leurs paramètres
param_algos <- uiOutput("params")

# PAGES #####################################################################

### Onglet 1 : Importation des données
tab1 <- tabPanel(
    "Préparation données",
    # Layout de l'onglet
    sidebarLayout(
        # Contenu de la barre sur le côté
        sidebarPanel(
            import_fich, # Importation d'un fichier extérieur
            tags$hr(), # Petite barre de séparation
            header_but, # Bouton choix Header
            sep_but, # Choix du séparateur
            cit_but, # Choix du charactère de citacion
            load_but # Bouton pour charger les données
        ),
        # Contenu au centre de la page (tableau de données)
        mainPanel(table_data)
    )
)

### Onglet 2 : Choix des algos
tab2 <- tabPanel(
    "ML Algorithme",
    class = "pages",
    fluidRow(
        # colonne pour choisir les algo
        column(
            2,
            choix_y, #Choix variable à expliquer
            p_learn, #Proportion apprentissage
            tags$hr(), # Petite barre de séparation
            choix_cv, #Choix si validation croisée ou non
            k_folds, #k folds
            exec_algo # Bouton pour lancer l'éxecution des algorithmes
        ),
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
)

### Onglet 3 : On pourra mettre des stat desc ou autre chose. Pour l'instant y'a rien dedans
tab3 <- tabPanel(
    "Stat Desc",
    class = "pages",
    fluidRow(
        column(4, ),
        column(4, ),
        column(4, )
    ),
)


### Onglet 4 : Paramètres des algos
tab4 <- tabPanel(
    "ML Paramètres",
    class = "pages",
    fluidRow(
        column(
            2,
            choix_methode,
            choix_algo
        ),
        column(
          10,
          param_algos
        )
    )
)

### Page de navigation qui rassemble tous les onglets
navbar_page <- navbarPage(
    title = "Machine Learning BD",
    position = "static-top",
    responsive = TRUE,
    tab1,
    tab2,
		tab3,
		tab4
)


# SHINY UI ####################################################################
shinyUI(
    fluidPage(
        theme = shinytheme("simplex"),
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        navbar_page
    )
)
