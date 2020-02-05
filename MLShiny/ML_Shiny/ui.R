source("code.R")
shinyUI(
  fluidPage(
    theme = shinytheme("simplex"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    # Nous avons une page avec plusieur onglets
    navbarPage(
      title = "Machine Learning BD",
      position = "static-top",
     # responsive = TRUE,

      ### Onglet 1 : Importation des données
      tabPanel(
        "Préparation données",

        # Layout de l'onglet
        sidebarLayout(

          # Contenu de la barre sur le côté
          sidebarPanel(

            # Importation d'un fichier extérieur
            fileInput(
              inputId = "dataSet",
              label = "Chemin du fichier (csv ou tsv)",
              # Types de fichiers acceptés
              accept = c("text/csv", ".csv")
            ),

            # Petite barre de séparation
            tags$hr(),

            # Bouton choix Header
            checkboxInput(
              inputId = "header",
              label = "Header",
              value = TRUE
            ),

            # Choix du séparateur
            radioButtons(
              inputId = "sep",
              label = "Separator",
              choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
              selected = ","
            ),

            # Choix du charactère de citacion
            radioButtons(
              inputId = "quote",
              label = "Quote",
              choices = c("None" = "", '"' = '"', "'" = "'"),
              selected = '"'
            ),

            # Bouton pour charger les données
            actionButton("load_data", "Charger les données"),

            # Petite barre de séparation
            tags$hr()
          ),

          # Contenu au centre de la page
          mainPanel(

            # Tableau des données importées
            tableOutput(outputId = "contents")
          )
        )
      ),

      tabPanel(
        "ML Parametres",
        fluidRow(

          column(
            2,
            radioButtons("class_reg", "Choisir une méthode:",
              choices = c("Classification", "Régression")
            ),
            checkboxGroupInput(
              inputId = "algo", "Algorithmes:",
              choices = algos
            )
          ),

          uiOutput("params1"),
          uiOutput("params2"),
          uiOutput("params3"),
          uiOutput("params4"),
          uiOutput("params5"),
          uiOutput("params6")
        )
      ),
     
      ### Onglet 2 : Choix des algos
      tabPanel(
        "ML Algorithme",
        class = "pages",
        fluidRow(
          # colonne pour choisir les algo
          column(
            2,
            selectInput("yvar", "Variable Y:", choices = ""),
            checkboxGroupInput("algo", "Algorithmes:", choices = algo_choices, selected = c("RF","Knn")),
            # Bouton pour lancer l'éxecution des algorithmes.
            actionButton("exec", "Executer")
          ),
          # colonne 1 pour afficher les graphiques des algo
          column(
            3,
            htmlOutput("best_param1"),
            plotlyOutput("algo1")
          ),
          # colonne 2 pour afficher les graphiques des algo
          column(
            3,
            htmlOutput("best_param2"),
            plotlyOutput("algo2")
          ),
          # colonne 3 pour afficher les graphiques des algo
          column(
            4,
            htmlOutput("best_param3"),
            plotlyOutput("algo3")
          )
          # column(5, verbatimTextOutput("value"))  # colonne pour afficher les summary
        )
      ),

      ### Onglet 3 : On pourra mettre des stat desc ou autre chose. Pour l'instant y'a rien dedans
      tabPanel(
        "Stat Desc",
        class = "pages",
        fluidRow(
          column(4, ),
          column(4, ),
          column(4, )
        ),
      )

      # Données utilisées
      # tabPanel("Data", htmlOutput(outputId = "Donnees"))
    )
  )
)
