library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(4,
           titlePanel("You can customize your labels here"),
           beam_customized_plot_labels_UI("airquality_lab", "Airquality plot")
    ),
    column(8,
           beam_plot_panel_UI("airquality_panel")
    )
  )

)

server <- function(input, output, session) {

  labels_airquality <- beam_customized_plot_labels_SERVER("airquality_lab")


  airquality_plot <- reactive({
    ggplot(airquality, aes(x = Wind, y = Temp, col = as.character(Month))) +
      geom_point() +
      ggtitle(labels_airquality[["plot_title"]]()) +
      xlab(labels_airquality[["plot_x_label"]]()) +
      ylab(labels_airquality[["plot_y_label"]]())
  })

  airquality_data <- reactive({
    airquality
  })

  beam_plot_panel_SERVER("airquality_panel",
                         plot_out = airquality_plot,
                         table_out = airquality_data)

}

shinyApp(ui, server)
