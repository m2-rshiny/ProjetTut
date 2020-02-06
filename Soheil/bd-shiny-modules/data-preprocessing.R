df.orig <- read_tsv("../anonymized-sq-dataset.tsv")
df.colnames <- colnames(df.orig)
df.rownames <- rownames(df.orig)

# SHINY UI #####################################################################

data_preprocessing_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # UI ELEMENTS ################################################################

  # DataTable output
  datatable_output <- dataTableOutput(ns("datatable"))

  # To select which rows to display (default: all rows)
  rows_selected_by_name_input <- selectInput(
    inputId = ns("rowsSelectedByName"),
    label = "Select rows by name",
    df.rownames,
    multiple = TRUE,
    selectize = FALSE
  )

  # To select which columns to display (default: first 6 columns)
  columns_selected_by_name_input <- selectInput(
    inputId = ns("columnsSelectedByName"),
    label = "Select columns by name",
    df.colnames,
    multiple = TRUE,
    selectize = FALSE
  )

  # To select which rows to display (default: all rows)
  rows_selected_by_id_input <- textInput(
    inputId = ns("rowsSelectedById"),
    label = "Select/Arrange rows by ID",
    value = ""
  )

  # To select which columns to display (default: first 6 columns)
  columns_selected_by_id_input <- textInput(
    inputId = ns("columnsSelectedById"),
    label = "Select/Arrange columns by ID",
    value = ""
  )

  # UI LAYOUTS #################################################################

  # Fluid rows
  parameters_row <- fluidRow(
    column(
      3,
      rows_selected_by_name_input,
      p("TODO")
    ),
    column(
      3,
      columns_selected_by_name_input,
      p("TODO")
    ),
    column(
      3,
      rows_selected_by_id_input,
      p("TODO")
    ),
    column(
      3,
      columns_selected_by_id_input,
      p("TODO")
    )
  )
  datatable_row <- fluidRow(
    column(
      12,
      hr(),
      datatable_output
    )
  )

  # Fluid page
  fluid_page <- fluidPage(
    parameters_row,
    datatable_row
  )

  # Return UI
  tagList(
    h3("Data Preprocessing"),
    hr(),
    fluid_page
  )
}

# SERVER LOGIC #################################################################

data_preprocessing <- function(input, output, session) {
  df.orig <- read_tsv("../anonymized-sq-dataset.tsv")
  # DataTable to display
  output$datatable <- renderDataTable({
    columns_selected <- 1:6
    rows_selected <- 1:nrow(df.orig)
    if (length(input$rowsSelectedByName) != 0) {
      rows_selected <- input$rowsSelectedByName
    }
    if (length(input$columnsSelectedByName) != 0) {
      columns_selected <- input$columnsSelectedByName
    }
    if (input$rowsSelectedById != "") {
      rows_selected_char <- str_split(input$rowsSelectedById, ",")[[1]]
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
    if (input$columnsSelectedById != "") {
      columns_selected_char <- str_split(input$columnsSelectedById, ",")[[1]]
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
    df.orig[rows_selected, columns_selected]
  })
}
