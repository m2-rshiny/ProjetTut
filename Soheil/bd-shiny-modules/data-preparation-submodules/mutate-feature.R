mutate_feature_ui <- function(id) {
  # Namespace
  ns <- NS(id)

  # TODO
  choose_var_name <- textInput(
    inputId = ns("varName"),
    label = "Choose a variable name"
  )
  
  # TODO
  new_var_operation <- textInput(
    inputId = ns("operation"),
    label = "Write an operation for your new variable"
  )
  
  tagList(
    choose_var_name,
    new_var_operation
  )
}

# TODO
mutate_feature <- function(input, output, session, dataset) {
  a <- reactive({
    var_name <- input$varName
    operation <- input$operation
    print(var_name)
    print(operation)
    dataset() %>% mutate(!! var_name := !! parse_quo(operation, env = caller_env()))
  })
  a()
}
