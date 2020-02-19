select_filter_feature_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # To select which columns to display (default: first 6 columns)
  columns_selected_by_name_input <- textInput(
    inputId = ns("selectColumns"),
    label = "Select or arrange columns"
  )
}

# TODO
select_filter_feature <- function(input, output, session, dataset) {
  a <- reactive({
    selected_columns <- str_split(input$selectColumns, ",")[[1]] %>%
      map(str_trim) %>%
      map(~ parse_quo(., env = caller_env()))
    dataset() %>% select(!!! selected_columns)
  })
  a()
}
