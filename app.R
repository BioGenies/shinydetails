library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)
library(svglite)
library(dplyr)
library(DT)

source("mod.R")



ui <- fluidPage(mainPanel(
  tabsetPanel(tabPanel(title = "plots 1",
                       column(width = 3,
                              br(),
                              sliderInput_h("x_lim", label = h3("Age"), min = 0, 
                                            max = 2000, value = c(0, 500)),
                              br(),
                              sliderInput_h("y_lim", label = h3("Circumference"), min = 0, 
                                            max = 400, value = c(0, 200)),
                              br(),
                              numericInput_h("breaks", label = h3("Breaks"), value = 10, min = 2)),
                       column(width = 9,
                              
                              tabsetPanel_UI("orange1"),
                              tabsetPanel_UI("orange2"),
                              tabsetPanel_UI("orange3"))),
              tabPanel("plots 2"),
              tabPanel("plots 3"),
              tabPanel("plots 4"),
              tabPanel("plots 5")
  )
)
)


server <- function(input, output) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  #orange plot 1
  orange1_data <- reactive({
    
    data = Orange %>%
      filter(age <= input[["x_lim"]][2], age >= input[["x_lim"]][1])
    
    df = data.frame(count = table(cut(data[["age"]], input[["breaks"]])))
    colnames(df) = c("age", "count")
    df 
  })
  orange1_plot_out <- reactive({
    orange1_data() %>% 
      ggplot(aes(x = age, y = count))+
      geom_col() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
  })
  
  
  
  
  #orange plot 2
  orange2_data <- reactive({
    Orange 
  })
  orange2_plot_out <- reactive({
    orange2_data() %>% 
      ggplot(aes(x = age, y = circumference, col = Tree))+
      ylim(input[["y_lim"]]) +
      xlim(input[["x_lim"]]) +
      geom_point()
  })
  
  #orange plot 3
  orange3_data <- reactive({
    Orange 
  })
  orange3_plot_out <- reactive({
    orange3_data() %>% 
      ggplot(aes(x = age, y = circumference, col = Tree))+
      ylim(input[["y_lim"]]) +
      xlim(input[["x_lim"]]) +
      geom_line()
  })
  
  tabsetPanel_SERVER(id = "orange1", 
                     plot_out = orange1_plot_out, 
                     table_out = orange1_data,
                     plot_type = "bar")
  tabsetPanel_SERVER(id = "orange2", 
                     plot_out = orange2_plot_out, 
                     table_out = orange2_data,
                     tt_cols = c("age", "Tree"))
  tabsetPanel_SERVER(id = "orange3",
                     plot_out = orange3_plot_out, 
                     table_out = orange3_data)
}


shinyApp(ui = ui, server = server)
