library(shiny)

shinyUI(fluidPage(mainPanel(
  tabsetPanel(tabPanel(title = "comparison plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("comparison"))),
              tabPanel(title = "differential plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("differential"))),
              tabPanel(title = "bar plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("bar"))),
              tabPanel("plots 3"),
              tabPanel("plots 4"),
              tabPanel("plots 5")
  )
)))
