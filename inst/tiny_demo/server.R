library(shiny)
library(shinyhelper)
library(ggplot2)

server <- shinyServer(function(input, output) {

  observe_helpers(session = getDefaultReactiveDomain(), help_dir = "helpfiles")

  #geom_segment
  geom_segment_data <- reactive({
    data.frame(mtcars,
               car = rownames(mtcars))
  })

  geom_segment_plot_out <- reactive({
    ggplot() +
      geom_point(stat ='identity', fill="black")  +
      geom_segment(geom_segment_data(), mapping = aes(y = 0,
                                                      x = car,
                                                      yend = mpg,
                                                      xend = car),
                   color = "black")
  })

  tabsetPanel_SERVER(id = "geom_segment",
                     plot_out = geom_segment_plot_out,
                     table_out = geom_segment_data,
                     plot_type = "geom_segment",
                     tt_content = list(row_text = c("Mpg: %f",  "Car: %s"),
                                       chosen_cols = c("mpg", "car")))

  #geom_segment
  geom_segment_data2 <- reactive({
    data.frame(x = c(1,2,5,6,8),
               y = c(3,6,2,8,7),
               vx = c(1,1.5,0.8,0.5,1.3),
               vy = c(0.2,1.3,1.7,0.8,1.4))
  })

  geom_segment_plot_out2 <- reactive({
    ggplot() +
      geom_segment(data = geom_segment_data2(),
                   mapping = aes(x = x, y = y, xend = x + vx, yend = y + vy),
                   size = 2, color = "blue")
  })

  tabsetPanel_SERVER(id = "geom_segment2",
                     plot_out = geom_segment_plot_out2,
                     table_out = geom_segment_data2,
                     plot_type = "geom_segment",
                     tt_range = 10)

  #geom_point

  geom_point_data <- reactive({
    iris
  })

  geom_point_plot <- reactive({
    ggplot(geom_point_data(),
           aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
      geom_point()
  })

  tabsetPanel_SERVER(id = "geom_point",
                     plot_out = geom_point_plot,
                     table_out = geom_point_data,
                     plot_type = "geom_point",
                     tt_range = 10)

  #geom_bar

  geom_bar_data <- reactive({
    data.frame(Sepal.Length = table(cut(iris[["Sepal.Length"]], seq(4, 8, length.out = 10))))
  })

  geom_bar_plot <- reactive({
    ggplot(geom_bar_data(),
           aes(x = Sepal.Length.Var1, y = Sepal.Length.Freq)) +
      geom_col()
  })

  tabsetPanel_SERVER(id = "geom_col",
                     plot_out = geom_bar_plot,
                     table_out = geom_bar_data,
                     plot_type = "geom_col",
                     tt_range = 10)



})
