library(shinydetails)
library(shiny)
library(ggplot2)

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


ui <- fluidPage(
  fluidRow(
    column(4,
           titlePanel("You can customize your labels here"),
           beam_customized_plot_labels_UI("example", "Example plot")
    ),
    column(8,
           plotOutput("Example_plot")
    )
  )

)

server <- function(input, output, session) {

  labels_example <- beam_customized_plot_labels_SERVER("example")

  output[["Example_plot"]] <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, col = Species)) +
      geom_point() +
      ggtitle(labels_example[["plot_title"]]()) +
      xlab(labels_example[["plot_x_label"]]()) +
      ylab(labels_example[["plot_y_label"]]())
  })
}

shinyApp(ui, server)











