# LIBRARIES ####################################################################

library(shiny)
library(tidyverse)
library(DT)
library(jsonlite)
library(caret)
library(rlang)
library(doParallel)
library(plotly)
library(reshape2)
library(mice)
library(DataExplorer)

# SHINY MODULES ################################################################

source("bd-shiny-modules/data-importation.R")
source("bd-shiny-modules/data-preparation.R")
source("bd-shiny-modules/exploratory-data-analysis.R")
source("bd-shiny-modules/model-selection.R")
source("bd-shiny-modules/export-results.R")

# SHINY APP ####################################################################

# Application name
app_name <- "BD Shiny"

# List of caret ML algorithms to take account
caret_ml_algorithms_list <- fromJSON("config/caret-ml-algorithms-list.json")

# List of caret preprocessing methods to take account
caret_preprocessing_list <- fromJSON("config/caret-preprocessing-list.json")

# Details of all caret ML algorithms
caret_ml_algorithms_details <- fromJSON("config/caret-ml-algorithms-details.json")

# Shiny UI
ui <- navbarPage(
  app_name,
  tabPanel("Data Importation", data_importation_ui("di")),
  tabPanel("Data Preparation", data_preparation_ui("dp")),
  tabPanel("Exploratory Data Analysis", exploratory_data_analysis_ui("eda")),
  tabPanel("Model Selection", model_selection_ui("ms")),
  tabPanel("Export Results", export_results_ui("er"))
)

# Server logic (with Shiny modules)
server <- function(input, output) {
  data_imported <- callModule(data_importation, "di")
  data_prepared <- callModule(data_preparation, "dp", data_imported)
  callModule(exploratory_data_analysis, "eda", data_prepared)
  callModule(model_selection,"ms", data_prepared, caret_ml_algorithms_list, caret_ml_algorithms_details, caret_preprocessing_list)
  callModule(export_results, "er")
}

# Shiny App
shinyApp(ui = ui, server = server)
