library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)
library(svglite)
library(dplyr)
library(DT)

source("mod.R")



ui <- fluidPage(mainPanel(
  column(width = 3,
         br(),
         sliderInput_h("x_lim", label = h3("x lim"), min = 0, 
                       max = 2000, value = c(0, 500)),
         br(),
         sliderInput_h("y_lim", label = h3("y lim"), min = 0, 
                       max = 400, value = c(0, 200))),
  column(width = 9,
         tabsetPanel(tabPanel(title = "plots 1",
                              tabsetPanel_UI("orange1"),
                              tabsetPanel_UI("orange2"),
                              tabsetPanel_UI("orange3")),
                     tabPanel("plots 2"),
                     tabPanel("plots 3"),
                     tabPanel("plots 4"),
                     tabPanel("plots 5"))
         
  )
)
)


server <- function(input, output) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  #orange plot 1
  orange1_data <- reactive({
    Orange %>% 
      filter(age >= input[["x_lim"]][[1]], 
             age <= input[["x_lim"]][[2]]) %>% 
      filter(circumference >= input[["y_lim"]][[1]], 
             circumference <= input[["y_lim"]][[2]])
  })
  orange1_plot_out <- reactive({
    orange1_data() %>% 
      ggplot(aes(x = age, y = circumference, col = Tree))+
      geom_line() +
      geom_point()
  })
  
  #orange plot 2
  orange2_data <- reactive({
    Orange %>% 
      filter(age >= input[["x_lim"]][[1]], 
             age <= input[["x_lim"]][[2]]) %>% 
      filter(circumference >= input[["y_lim"]][[1]], 
             circumference <= input[["y_lim"]][[2]])
  })
  orange2_plot_out <- reactive({
    orange1_data() %>% 
      ggplot(aes(x = age, y = circumference, col = Tree))+
      geom_point()
  })
  
  #orange plot 3
  orange3_data <- reactive({
    Orange %>% 
      filter(age >= input[["x_lim"]][[1]], 
             age <= input[["x_lim"]][[2]]) %>% 
      filter(circumference >= input[["y_lim"]][[1]], 
             circumference <= input[["y_lim"]][[2]])
  })
  orange3_plot_out <- reactive({
    orange1_data() %>% 
      ggplot(aes(y = age, x = circumference, col = Tree))+
      geom_line() +
      geom_point()
  })
  
  tabsetPanel_SERVER(id = "orange1", 
                     data = reactive(Orange), 
                     plot = orange1_plot_out, 
                     table = orange1_data)
  tabsetPanel_SERVER(id = "orange2", 
                     data = reactive(Orange), 
                     plot = orange2_plot_out, 
                     table = orange2_data)
  tabsetPanel_SERVER(id = "orange3", 
                     data = reactive(Orange), 
                     plot = orange3_plot_out, 
                     table = orange3_data)
}


shinyApp(ui = ui, server = server)
