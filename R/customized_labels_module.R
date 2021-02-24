
#' @title UI
#' @description words
#' @export

beam_customized_plot_labels_UI <- function(id,
                                           name = "Plot") {
  ns <- NS(id)
  tagList(
    "Adjust labels",
    textInput(inputId = ns("plot_title"),
              label = paste(name, "title:"),
              value = " "),
    textInput(inputId = ns("plot_x_label"),
              label = paste(name, "axis x label:"),
              value = " "),
    textInput(inputId = ns("plot_y_label"),
              label = paste(name, "axis y label:"),
              value = " ")
  )
}


#' @title Server
#' @description words
#' @export

beam_customized_plot_labels_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      list(
        plot_title = reactive({ input$plot_title }),
        plot_x_label = reactive({ input$plot_x_label }),
        plot_y_label = reactive({ input$plot_y_label })
      )
    )
  })
}
