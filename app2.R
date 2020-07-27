library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)
library(svglite)
library(dplyr)
library(DT)


dt_format <- function(dat, cols = colnames(dat)) {
  
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
            filter = "bottom",
            rownames = FALSE)
}



plotOutput_h <- function(outputId, ...) {
  helper(plotOutput(outputId, ...), content = outputId, type = "markdown")
}


sliderInput_h <- function(inputId, ...) {
  helper(sliderInput(inputId, ...), content = inputId, type = "markdown")
}

numericInput_h <- function(inputId, ...) {
  helper(numericInput(inputId, ...), content = inputId, type = "markdown")
}


tableOutput_h <- function(inputId, ...) {
  helper(DT::dataTableOutput(inputId, ...), content = inputId, type = "markdown")
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


generate_plot_output = function(id, data, output, input) {
  output[[paste0(id, "_plot")]] <- renderPlot({eval(parse(text = paste0(id, "_plot_out()")))})
  output[[paste0(id,"_tooltip")]] <- renderUI({generate_tooltip(data, input[[paste0(id, "_hover")]])})
  output[[paste0(id, "_download_png")]] <- create_downloadButton(id, "png")
  output[[paste0(id, "_download_jpeg")]] <- create_downloadButton(id, "jpeg")
  output[[paste0(id, "_download_svg")]] <- create_downloadButton(id, "svg")
  output
}


generate_output = function(panel, data, output, input) {
  if(any(c(paste0(panel, "_plot"),
           paste0(panel,"_tooltip"),
           paste0(panel, "_download_jpeg"),
           paste0(panel, "_download_png"),
           paste0(panel, "_download_svg"),
           paste0(panel, "_data")) %in% names(output))) stop("Names conflict")
  
  output = generate_plot_output(panel, data, output, input)
  
  
  output[[paste0(panel, "_data")]] <- DT::renderDataTable(server = FALSE, {
    eval(parse(text = paste0(panel, "_data()")))
  })
  output
}


ui <- fluidPage(mainPanel(
  column(width = 3,
         br(),
         sliderInput_h("x_lim", label = h3("x lim"), min = 0, 
                       max = 2000, value = c(0, 200)),
         br(),
         sliderInput_h("y_lim", label = h3("y lim"), min = 0, 
                     max = 400, value = c(0, 200))),
  column(width = 9,
         tabsetPanel(tabPanel(title = "plots 1",
                              generate_tabsetPanel("orange"),
                              br(),
                              generate_tabsetPanel("mtcars"),
                              br(),
                              generate_tabsetPanel("iris")),
                     tabPanel("plots 2"),
                     tabPanel("plots 3"),
                     tabPanel("plots 4"),
                     tabPanel("plots 5"))
         
  )
)
)


server <- function(input, output) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(), help_dir = "helpfiles")
  
  
  
  #plot 1
  
  iris_data <<- reactive({
    iris %>% 
      mutate(c = "lala")
  })
  
  iris_plot_out <<- reactive({
    iris_data() %>% 
      ggplot(aes(x = Sepal.Length, y = Petal.Length, col = Species)) +
      geom_point()
  })
  
  
  output <- generate_output("iris", iris_data(), output, input)
  
  
  # plot 2
  
  mtcars_data <<- reactive({
    mtcars
  })
  
  mtcars_plot_out <<- reactive({
    mtcars_data() %>% 
    ggplot(aes(x = mpg, y = disp, col = as.factor(gear)))+
      geom_point()
  })
  
  
  output <- generate_output("mtcars", mtcars, output, input)
  
  
  # plot 3
  
  orange_data <<- reactive({
    Orange %>% 
      filter(age >= input[["x_lim"]][[1]], 
             age <= input[["x_lim"]][[2]]) %>% 
      filter(circumference >= input[["y_lim"]][[1]], 
             circumference <= input[["y_lim"]][[2]])
  })
  
  orange_plot_out <<- reactive({
    orange_data() %>% 
    ggplot(aes(x = age, y = circumference, col = Tree))+
      geom_line() +
      geom_point()
  })

  output <- generate_output("orange", Orange, output, input)
}


shinyApp(ui = ui, server = server)
