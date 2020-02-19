rename_feature_ui <- function(id, col_names) {
  # Namespace
  ns <- NS(id)

  # TODO
  select_column <- selectInput(
    inputId = ns("selectColumn"),
    label = "Select a column",
    choices = col_names,
    multiple = FALSE,
    selectize = FALSE
  )

  # TODO
  choose_new_name <- textInput(
    inputId = ns("newName"),
    label = "Choose a new name"
  )

  tagList(
    select_column,
    choose_new_name
  )
}

# TODO
rename_feature <- function(input, output, session, dataset) {
  output_tbl <- reactive({
    dataset() %>% rename(!! input$newName := input$selectColumn)
  })
  
  output_tbl()
}
