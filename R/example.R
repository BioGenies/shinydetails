library(shiny)
library(shinyhelper)

source("modules.r")
source("server_functions.r")


ui <- shinyUI(fluidPage(mainPanel(
  tabsetPanel(tabPanel(title = "geom_point",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("point"))),
              tabPanel(title = "geom_col",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("col"))),
              tabPanel("plots 3"),
              tabPanel("plots 4"),
              tabPanel("plots 5")
  )
)))

server <-shinyServer(function(input, output) {
  
  observe_helpers(session = getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  #point plot and data
  point_data <- reactive({
    iris
  })
  
  point_plot <- reactive({
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, col = Species)) +
      geom_point()
  })
  
  tabsetPanel_SERVER(id = "point",
                     plot_out = point_plot,
                     table_out = point_data,
                     plot_type = "geom_point",
                     tt_content = list(row_text = c("Sepal length x width: %f x %f",
                                                    "Petal length x width: %f x %f",
                                                    "Species: %s"),
                                       chosen_cols = c("Sepal.Length", "Sepal.Width", 
                                                       "Petal.Length", "Petal.Width", 
                                                       "Species")))
  #geom_col plot and data
  
  col_data <- reactive({
    
    count = as.vector(table(iris[["Species"]]))
    species = names(table(iris[["Species"]]))
    data.frame(count, species)
  })
  
  col_plot <- reactive({
    ggplot(col_data(), aes(x = species, y = count)) +
      geom_col()
  })
  
  tabsetPanel_SERVER(id = "col",
                     plot_out = col_plot,
                     table_out = col_data,
                     plot_type = "geom_col")
  
})



shinyApp(ui, server)

