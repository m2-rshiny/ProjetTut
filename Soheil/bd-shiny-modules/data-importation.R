# By default, the file size limit is 5MB. It can be changed by setting this
# option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize=30*1024^2)

# SHINY UI #####################################################################

data_importation_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # File input element
  file_input <- fileInput(
    inputId = ns("fileInput"),
    label = "Chemin du fichier (csv ou tsv)",
    accept = c("text/csv", ".csv")
  )

  # Checkbox element to indicate if there is a header
  has_header <- checkboxInput(
    inputId = ns("hasHeader"),
    label = "Header",
    value = TRUE
  )

  # Radio buttons to select the separator character
  sep_selected <- radioButtons(
    inputId = ns("sepSelected"),
    label = "Separator",
    choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
    selected = "\t"
  )

  # Radio buttons to select the quote character
  quote_selected <- radioButtons(
    inputId = ns("quoteSelected"),
    label = "Quote",
    choices = c("None" = "", '"' = '"', "'" = "'"),
    selected = "'"
  )

  # Button to load data
  load_data_button <- actionButton(ns("loadDataButton"), "Charger les données")

  # DataTable output
  datatable_output <- dataTableOutput(ns("datatable"))

  # LAYOUTS ####################################################################

  # Sidebar panel
  sidebar_panel <- sidebarPanel(
    file_input,
    tags$hr(),
    has_header,
    sep_selected,
    quote_selected,
    load_data_button
  )

  # Main panel
  main_panel <- mainPanel(datatable_output)

  # Sidebar layout
  sidebar_layout <- sidebarLayout(sidebar_panel, main_panel)

  # Return UI
  tagList(
    sidebar_layout
  )
}

# SERVER LOGIC #################################################################

data_importation <- function(input, output, session) {
  # Store the dataset imported
  data_imported <- eventReactive(input$loadDataButton, {
    # Input file element
    file_input <- input$fileInput

    # If there isn't input file
    if (is.null(file_input)) {
      return(NULL)
    }

    # Read the uploaded dataset
    df <- read.csv(
      file = file_input$datapath,
      header = input$hasHeader,
      sep = input$sepSelected,
      quote = input$quoteSelected
    )

    # Return the dataset
    return(df)
  })

  # Ce qui sera mis dans la table de données affichée
  output$datatable <- renderDataTable({
    data_imported()
  })
}
