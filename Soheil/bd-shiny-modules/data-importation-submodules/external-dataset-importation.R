# SHINY UI #####################################################################

external_dataset_importation_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # File input element
  select_file <- fileInput(
    inputId = ns("fileInput"),
    label = "Select a file (only delimited flat files accepted)"
  )

  # Radio buttons to indicate whether the file contains a header
  has_header <- radioButtons(
    inputId = ns("hasHeader"),
    label = "Indicate whether the file contains the names of the variables as its first line",
    choices = list("Yes" = TRUE, "No" = FALSE)
  )

  # Radio buttons to select the field separator character
  select_sep <- radioButtons(
    inputId = ns("sep"),
    label = "Indicate the field separator character",
    choices = c("Comma" = ",", "Semicolon" = ";", "Tabulation" = "\t"),
    selected = ","
  )

  # Radio buttons to select the character used to quote strings
  select_quote <- radioButtons(
    inputId = ns("quote"),
    label = "Indicate the character used to quote strings",
    choices = c("None" = "", 'Single quotes' = "'", "Double quotes" = '"'),
    selected = ""
  )
  
  # Radio buttons to select the character used to quote strings
  select_dec <- radioButtons(
    inputId = ns("dec"),
    label = "Indicate the character used for decimal points",
    choices = list("Point" = ".", "Comma" = ","),
    selected = "."
  )

  # Return the UI
  tagList(
    select_file,
    has_header,
    select_sep,
    select_quote,
    select_dec
  )
}

# SERVER LOGIC #################################################################

external_dataset_importation <- function(input, output, session) {
  # Uploaded dataset (reactive object)
  dataset <- reactive({
    # Input file element
    file_input <- input$fileInput
    
    # If there isn't input file, return NULL
    if (is.null(file_input)) {
      return(NULL)
    }
    
    print(input$hasHeader)
    print(input$sep)
    print(input$quote)
    print(input$dec)
    
    # Read the uploaded data file
    read.csv(
      file = file_input$datapath,
      header = as.logical(input$hasHeader),
      sep = input$sep,
      quote = input$quote,
      dec = input$dec
    )
  })

  # Return the dataset
  dataset
}
