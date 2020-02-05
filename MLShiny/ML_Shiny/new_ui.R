#---- Onglet 4 Paramètres des algos
# Choix méthodes
choix_methode <- radioButtons("class_reg", "Choisir une méthode:",
  choices = c("Classification", "Régression")
)

# Choix algo
choix_algo <- checkboxGroupInput(
    inputId = "algo", "Algorithmes:",
    choices = algos
)

# Affichage des algo pour insérer leurs paramètres
affiche_algo1 <- uiOutput("params1")
affiche_algo2 <- uiOutput("params2")
affiche_algo3 <- uiOutput("params3")
affiche_algo4 <- uiOutput("params4")
affiche_algo5 <- uiOutput("params5")
affiche_algo6 <- uiOutput("params6")

tab4 <- tabPanel(
    "ML Paramètres",
    class = "pages",
    fluidRow(
        column(
            2,
            choix_methode,
            choix_algo
        ),
        affiche_algo1,
        affiche_algo2,
        affiche_algo3,
        affiche_algo4,
        affiche_algo5,
        affiche_algo6
    )
)


