library(shiny)

shinyUI(fluidPage(mainPanel(
  tabsetPanel(tabPanel(title = "comparison plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("comparison", helpfiles = "./inst/example-app/helpfiles"))),
              tabPanel(title = "differential plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("differential", helpfiles = "./inst/example-app/helpfiles"))),
              tabPanel(title = "bar plot",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("bar", helpfiles = "./inst/example-app/helpfiles"))),
              tabPanel("plots 3"),
              tabPanel("plots 4"),
              tabPanel("plots 5")
  )
)))
