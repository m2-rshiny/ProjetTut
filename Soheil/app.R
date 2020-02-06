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
  tabPanel("Data Importation", data_importation_ui("di")),
  tabPanel("Data Preprocessing", data_preprocessing_ui("dp")),
  tabPanel("Exploratory Data Analysis", exploratory_data_analysis_ui("eda")),
  tabPanel("Model Selection", model_selection_ui("ms")),
  tabPanel("Export Results", export_results_ui("er"))
)

server <- function(input, output) {
  callModule(data_importation, "di")
  callModule(data_preprocessing, "dp")
  callModule(exploratory_data_analysis, "eda")
  callModule(model_selection, "ms")
  callModule(export_results, "er")
}

shinyApp(ui = ui, server = server)
