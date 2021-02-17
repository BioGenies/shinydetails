library(shiny)
library(ggplot2)

ui <- fluidPage(title = "Example app",
                beam_plot_panel_UI("airquality_basic"),
                beam_plot_panel_UI("airquality_advanced",
                                   tab_plot = "An advanced plot",
                                   tab_table = "An elegant table for an advanced plot",
                                   helpfiles = "custom_helpfiles")
)

server <- function(input, output, session) {

  airquality_basic_plot <- reactive({
    ggplot(airquality, aes(x = Wind, y = Temp, col = as.character(Month))) +
      geom_point()
  })

  airquality_basic_data <- reactive({
    airquality
  })

  beam_plot_panel_SERVER("airquality_basic",
                         plot_out = airquality_basic_plot(),
                         table_out = airquality_basic_data())

  airquality_advanced_data <- reactive({

    df <- data.frame(aggregate(. ~ Month, airquality, function(x) c(mean = mean(x), sd = sd(x)))[, c(1, 5)])
    df[["Month"]] <- month.name[df[["Month"]]]
    data.frame(Month = df[["Month"]],
               Temp_mean = df$Temp[, 1],
               Error = df$Temp[, 2],
               Year = 1973)
  })


  airquality_advanced_plot <- reactive({
    ggplot(airquality_advanced_data(), aes(y = 0, yend = Temp_mean, x = Month, xend = Month)) +
      geom_segment() +
      geom_point(mapping = aes(x = Month, y = Temp_mean), size = 3) +
      geom_point() +
      ylab("average temperature")
  })

  beam_plot_panel_SERVER("airquality_advanced",
                         plot_out = airquality_advanced_plot(),
                         table_out = airquality_advanced_data(),
                         plot_type = "geom_segment",
                         tt_content = list(row_text = c("Date: %s %i",
                                                        "Average temperature: %f °F",
                                                        "Error: %f",
                                                        "Place: La Guardia Airport"),
                                           chosen_cols = c("Month", "Year", "Temp_mean", "Error")))



}


shinyApp(ui, server)

