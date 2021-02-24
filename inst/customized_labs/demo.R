

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
