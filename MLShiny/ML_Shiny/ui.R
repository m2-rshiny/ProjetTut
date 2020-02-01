source("code.R")
shinyUI(fluidPage(theme = shinytheme("simplex"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                  navbarPage(title = "Machine Learning BD",
                             position = "static-top", responsive = TRUE,
                             
                             tabPanel("ML Algorithme", class = "pages",
                                      fluidRow(
                                          column(2,
                                                 selectInput("yvar", "Variable Y:",choices = var_response),
                                                 checkboxGroupInput("algo", "Algorithmes:", choices = algo, selected = algo)), # colonne pour choisir les algo
                                          column(3,htmlOutput("best_param1"),
                                                 plotOutput("algo1")), # colonne pour ploter les algo
                                          column(3, htmlOutput("best_param2"),
                                                 plotOutput("algo2")),
                                          column(4, htmlOutput("best_param3"),
                                                 plotOutput("algo3"))
                                          #column(5, verbatimTextOutput("value"))  # colonne pour afficher les summary
                                      )),
                             
                             # On pourra mettre des stat desc ou autre chose. Pour l'instant y'a rien dedans
                             tabPanel("Stat Desc", class = "pages",
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
