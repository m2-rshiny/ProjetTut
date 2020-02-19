visualising_distributions_ui <- function(id, col_names) {
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
  tagList(
    select_column
  )
}

# TODO
visualising_distributions <- function(input, output, session, dataset) {
  # TODO
  type <- pillar::type_sum(dataset()[[input$selectColumn]])

  # TODO
  plot <- NULL
  if (type %in% c("fct", "lgl", "chr")) {
    plot <- dataset() %>%
      ggplot(aes(x = !! parse_quo(input$selectColumn, env = caller_env()))) +
      geom_bar()
  }
  if (type %in% c("int", "dbl")) {
    plot <- dataset() %>%
      ggplot(aes(x = !! parse_quo(input$selectColumn, env = caller_env()))) +
      geom_histogram(bins = 12)
  }

  # TODO
  plot + theme_gray()
}
