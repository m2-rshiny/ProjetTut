change_type_feature_ui <- function(id, col_names) {
  # Namespace
  ns <- NS(id)

  # TODO
  select_columns <- selectInput(
    inputId = ns("selectColumns"),
    label = "Select columns",
    choices = col_names,
    multiple = TRUE,
    selectize = FALSE
  )

  # TODO
  select_type <- selectInput(
    inputId = ns("selectType"),
    label = "Select type",
    choices = list(
      "Logical <lgl>" = "lgl",
      "Integer <int>" = "int",
      "Double <dbl>" = "dbl",
      "Character <chr>" = "chr",
      "Factor <fct>" = "fct"
    )
  )

  tagList(
    select_columns,
    select_type
  )
}

# TODO
change_type_feature <- function(input, output, session, dataset) {
  output_tbl <- reactive({
    type_function <- switch(input$selectType,
      "lgl" = as.logical,
      "int" = as.integer,
      "dbl" = as.double,
      "chr" = as.character,
      "fct" = as_factor
    )
    dataset() %>% mutate_at(.vars = input$selectColumns, .funs = type_function)
  })
  
  output_tbl()
}
