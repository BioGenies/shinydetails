library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)


plotOutput_h <- function(outputId, ...) {
  helper(plotOutput(outputId, ...), content = outputId, type = "markdown")
}


numericInput_h <- function(inputId, ...) {
  helper(numericInput(inputId, ...), content = inputId, type = "markdown")
}


tableOutput_h <- function(inputId, ...) {
  helper(tableOutput(inputId, ...), content = inputId, type = "markdown")
}


generate_tooltip = function(df, hv) {
  if(!is.null(hv)) {
    tt_df <- nearPoints(df, hv, maxpoints = 1)
    
    if(nrow(tt_df) != 0) { 
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]], 
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                      tt_pos_adj, ":", tt_pos, 
                      "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
      
      div(style = style,
          p(HTML(paste(colnames(tt_df), ": ", t(tt_df ), c(rep("<br/>", ncol(tt_df)-1), ""))))
      )
    }
  }
}


generate_tabsetPanel = function(panel, ...) {
  tagList(tabsetPanel(tabPanel(title = paste(panel, "plot 1"),
                               plotOutput_h(paste0(panel, "_1_plot"), hover = paste0(panel, "_1_hover"), ...),
                               uiOutput(paste0(panel, "_1_tooltip")),
                               downloadButton(paste0(panel, "_1_download_png"), "Download png"),
                               downloadButton(paste0(panel, "_1_download_jpeg"), "Download jpeg")),
                      
                      tabPanel(title = paste(panel, "plot 2"),
                               plotOutput_h(paste0(panel, "_2_plot"), hover = paste0(panel, "_2_hover"), ...),
                               uiOutput(paste0(panel, "_2_tooltip")),
                               downloadButton(paste0(panel, "_2_download_png"), "Download png"),
                               downloadButton(paste0(panel, "_2_download_jpeg"), "Download jpeg")),
                      
                      tabPanel(title = paste(panel, "plot 3"),
                               plotOutput_h(paste0(panel, "_3_plot"), hover = paste0(panel, "_3_hover"), ...),
                               uiOutput(paste0(panel, "_3_tooltip")),
                               downloadButton(paste0(panel, "_3_download_png"), "Download png"),
                               downloadButton(paste0(panel, "_3_download_jpeg"), "Download jpeg")),
                      
                      tabPanel(title = paste(panel, "data"), 
                               tableOutput_h(paste0(panel, "_data"), ...))))
}


generate_plot_output = function(id, data, output, input, ...) {
  
  output[[paste0(id, "_plot")]] <- renderPlot({eval(parse(text = paste0(id, "_plot_out()")))})
  
  output[[paste0(id,"_tooltip")]] <- renderUI({generate_tooltip(data, input[[paste0(id, "_hover")]])})
  
  output[[paste0(id, "_download_png")]] <- downloadHandler(paste0(id, "_plot.png"),
                                                           content = function(file){
                                                             ggsave(file, 
                                                                    eval(parse(text = paste0(id, "_plot_out()"))),
                                                                    width = 7, 
                                                                    height = 4, 
                                                                    dpi = 300, 
                                                                    units = "in", 
                                                                    device='png')
                                                           })
  output[[paste0(id, "_download_jpeg")]] <- downloadHandler(paste0(id, "_plot.jpeg"),
                                                            content = function(file){
                                                              ggsave(file, 
                                                                     eval(parse(text = paste0(id, "_plot_out()"))),
                                                                     width = 7, 
                                                                     height = 4, 
                                                                     dpi = 300, 
                                                                     units = "in", 
                                                                     device='jpeg')
                                                            })
  output
}


draw_plots = function(data, ...) {
  
  plt1 = ggplot(data = data, aes(...))+
    geom_point()
  
  plt2 = ggplot(data = data, aes(...))+
    geom_line()
  
  plt3 = ggplot(data = data, aes(...)) +
    geom_point() +
    geom_line()
  
  list(plt1, plt2, plt3)
}


generate_output = function(panel, data, output, input, ...) {
  
  plots = draw_plots(data, ...)
  assign(paste0(panel, "_1_plot_out"), reactive({plots[[1]]}), envir=.GlobalEnv)
  assign(paste0(panel, "_2_plot_out"), reactive({plots[[2]]}), envir=.GlobalEnv)
  assign(paste0(panel, "_3_plot_out"), reactive({plots[[3]]}), envir=.GlobalEnv)
  
  output = generate_plot_output(paste0(panel, "_1"), data, output, input, ...)
  output = generate_plot_output(paste0(panel, "_2"), data, output, input, ...)
  output = generate_plot_output(paste0(panel, "_3"), data, output, input, ...)
  
  output[[paste0(panel, "_data")]] <- renderTable({data})
  output
}



ui <- fluidPage(mainPanel(
  column(width = 3,
         br(),
         numericInput_h("n_input", label = "Numeric input", 
                        value = 1, min = 1, max = 5)),
  column(width = 9,
         generate_tabsetPanel("orange"),
         br(),
         generate_tabsetPanel("CO2"),
         br(),
         generate_tabsetPanel("mtcars"),
         br(),
         generate_tabsetPanel("iris"),
         br(),
         generate_tabsetPanel("rock")
  )
))


server <- function(input, output) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  output <- generate_output("CO2", CO2, output, input, x = conc, y = uptake, col = Plant)
  output <- generate_output("rock", rock, output, input, y = peri, x = shape, col = as.factor(perm))
  output <- generate_output("iris", iris, output, input, x = Sepal.Length, y = Petal.Length, col = Species)
  output <- generate_output("mtcars", mtcars, output, input, x = mpg, y = disp, col = as.factor(gear))
  output <- generate_output("orange", Orange, output, input, x = age, y = circumference, col = Tree)
}


shinyApp(ui = ui, server = server)
