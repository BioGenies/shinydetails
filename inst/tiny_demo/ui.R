library(shiny)

shinyUI(fluidPage(mainPanel(
  tabsetPanel(tabPanel(title = "geom_segment",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("geom_segment"))),
              tabPanel("geom_segment",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("geom_segment2"))),
              tabPanel("geom_point",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("geom_point"))),
              tabPanel("geom_col",
                       column(width = 3),
                       column(width = 9,
                              tabsetPanel_UI("geom_col")))
  )
)))
