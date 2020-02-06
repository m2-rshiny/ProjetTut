# SHINY UI #####################################################################

dataPreprocessingUI <- function(id) {
  # Namespace
  ns <- NS(id)
  
  # UI ELEMENTS ################################################################

  # DataTable output
  datatable_output <- dataTableOutput(ns("datatable"))

  # To select which columns to display (default: first 6 columns)
  columns_selected_input <- textInput(
    inputId = ns("columnsSelected"),
    label = "Select columns to display",
    value = ""
  )

  # To select which rows to display (default: all rows)
  row_selected_input <- textInput(
    inputId = ns("rowsSelected"),
    label = "Select rows to display",
    value = ""
  )

  # Return all the elements
  tagList(
    datatable_output,
    columns_selected_input,
    row_selected_input
  )
}

# SERVER LOGIC #################################################################

dataPreprocessing <- function(input, output, session) {
  df.orig <- read_tsv("../anonymized-sq-dataset.tsv")
  # DataTable to display
  output$datatable <- renderDataTable({
    columns_selected <- 1:6
    rows_selected <- 1:nrow(df.orig)
    if (input$columnsSelected != "") {
      columns_selected_char <- str_split(input$columnsSelected, ",")[[1]]
      columns_selected <- numeric()
      for (elt in columns_selected_char) {
        if (grepl(":", elt)) {
          range <- as.integer(str_split(elt, ":")[[1]])
          columns_selected <- c(columns_selected, range[1]:range[2])
        } else {
          columns_selected <- c(columns_selected, as.integer(elt))
        }
      }
    }
    if (input$rowsSelected != "") {
      rows_selected_char <- str_split(input$rowsSelected, ",")[[1]]
      rows_selected <- numeric()
      for (elt in rows_selected_char) {
        if (grepl(":", elt)) {
          range <- as.integer(str_split(elt, ":")[[1]])
          rows_selected <- c(rows_selected, range[1]:range[2])
        } else {
          rows_selected <- c(rows_selected, as.integer(elt))
        }
      }
    }
    df.orig[rows_selected, columns_selected]
  })
}
