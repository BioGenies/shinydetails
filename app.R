library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)
library(svglite)


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
  tagList(tabsetPanel(tabPanel(title = paste(panel, "plot"),
                               br(),
                               div(style = "position:relative",
                                   plotOutput_h(paste0(panel, "_plot"),  
                                                hover = hoverOpts(paste0(panel, "_hover"), 
                                                                  delay = 10, 
                                                                  delayType = "debounce"), ...),
                                   uiOutput(paste0(panel, "_tooltip")),
                                   downloadButton(paste0(panel, "_download_png"), "Download png"),
                                   downloadButton(paste0(panel, "_download_jpeg"), "Download jpeg"),
                                   downloadButton(paste0(panel, "_download_svg"), "Download svg"))
  ),
  tabPanel(title = paste(panel, "data"), 
           tableOutput_h(paste0(panel, "_data"), ...))))
}



create_downloadButton = function(id, device) {
  downloadHandler(paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, 
                           eval(parse(text = paste0(id, "_plot_out()"))),
                           device = device, height = 300, width = 400, 
                           units = "mm")})
}


generate_plot_output = function(id, data, output, input, ...) {
  
  output[[paste0(id, "_plot")]] <- renderPlot({eval(parse(text = paste0(id, "_plot_out()")))})
  
  output[[paste0(id,"_tooltip")]] <- renderUI({generate_tooltip(data, input[[paste0(id, "_hover")]])})
  
  output[[paste0(id, "_download_png")]] <- create_downloadButton(id, "png")
  output[[paste0(id, "_download_jpeg")]] <- create_downloadButton(id, "jpeg")
  output[[paste0(id, "_download_svg")]] <- create_downloadButton(id, "svg")
  output
}


generate_output = function(panel, data, output, input, ...) {
  
  assign(paste0(panel, "_plot_out"), reactive({
    ggplot(data = data, aes(...))+
      geom_point()
  }), envir=.GlobalEnv)
  
  if(any(c(paste0(panel, "_plot_out"), 
           paste0(panel, "_plot"),
           paste0(panel,"_tooltip"),
           paste0(panel, "_download_jpeg"),
           paste0(panel, "_download_png"),
           paste0(panel, "_download_svg"),
           paste0(panel, "_data")) %in% names(output))) stop("Names conflict")
  
  output = generate_plot_output(panel, data, output, input, ...)
  output[[paste0(panel, "_data")]] <- renderTable({data})
  output
}


ui <- fluidPage(mainPanel(
  column(width = 3,
         br(),
         numericInput_h("n_input", label = "Numeric input", 
                        value = 1, min = 1, max = 5)),
  column(width = 9,
         tabsetPanel(tabPanel(title = "plots 1",
                              generate_tabsetPanel("orange"),
                              br(),
                              generate_tabsetPanel("mtcars"),
                              br(),
                              generate_tabsetPanel("iris")),
                     tabPanel(title = "plots 2",
                              generate_tabsetPanel("CO2"),
                              br(),
                              generate_tabsetPanel("rock"),
                              br(),
                              generate_tabsetPanel("trees")),
                     tabPanel(title = "plots 3",
                              generate_tabsetPanel("airquality"),
                              br(),
                              generate_tabsetPanel("sleep"),
                              br(),
                              generate_tabsetPanel("attenu")),
                     tabPanel(title = "plots 4",
                              generate_tabsetPanel("attitude"),
                              br(),
                              generate_tabsetPanel("cars"),
                              br(),
                              generate_tabsetPanel("beaver1")),
                     tabPanel(title = "plots 5",
                              generate_tabsetPanel("stackloss"),
                              br(),
                              generate_tabsetPanel("warpbreaks"),
                              br(),
                              generate_tabsetPanel("women")))
         
  )
)
)


server <- function(input, output) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  output <- generate_output("iris", iris, output, input, x = Sepal.Length, y = Petal.Length, col = Species)
  output <- generate_output("mtcars", mtcars, output, input, x = mpg, y = disp, col = as.factor(gear))
  output <- generate_output("orange", Orange, output, input, x = age, y = circumference, col = Tree)
  
  output <- generate_output("CO2", CO2, output, input, x = conc, y = uptake, col = Plant)
  output <- generate_output("rock", rock, output, input, y = peri, x = shape, col = as.factor(perm))
  output <- generate_output("trees", trees, output, input, x = Height , y = Volume)
  
  output <- generate_output("sleep", sleep, output, input, y = extra  , x = group, col = ID)
  output <- generate_output("airquality", airquality, output, input, y = Wind, x = Solar.R, col = Month )
  output <- generate_output("attenu", attenu, output, input, y = dist , x = station  , col = event)
  
  output <- generate_output("attitude", attitude, output, input, y = rating , x = complaints, col = raises)
  output <- generate_output("cars", cars, output, input, y = speed, x = dist)
  output <- generate_output("beaver1", beaver1, output, input, y = temp, x = time, col = as.character(activ))
  
  output <- generate_output("stackloss", stackloss, output, input, y = Water.Temp , x = Acid.Conc.)
  output <- generate_output("warpbreaks", warpbreaks , output, input, y = wool , x = breaks, col = tension)
  output <- generate_output("women", women, output, input, y = height , x = weight)
}


shinyApp(ui = ui, server = server)
