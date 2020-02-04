# LIBRARIES ####################################################################

library(shiny)
library(readr)
library(DT)

# SHINY MODULES ################################################################

source("bd-shiny-modules/data-importation.R")
source("bd-shiny-modules/data-preprocessing.R")
source("bd-shiny-modules/exploratory-data-analysis.R")
source("bd-shiny-modules/model-selection.R")
source("bd-shiny-modules/export-results.R")

# SHINY APP ####################################################################

app_name <- "BD Shiny"

ui <- navbarPage(
  app_name,
  tabPanel("Data Importation", dataImportationUI("di")),
  tabPanel("Data Preprocessing", dataPreprocessingUI("dp")),
  tabPanel("Exploratory Data Analysis", exploratoryDataAnalysisUI("eda")),
  tabPanel("Model Selection", modelSelectionUI("ms")),
  tabPanel("Export Results", exportResultsUI("er"))
)

server <- function(input, output) {
  callModule(dataImportation, "di")
  callModule(dataPreprocessing, "dp")
  callModule(exploratoryDataAnalysis, "eda")
  callModule(modelSelection, "ms")
  callModule(exportResults, "er")
}

shinyApp(ui = ui, server = server)
