

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



dt_format <- function(dat, cols = colnames(dat)) {
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
            filter = "bottom",
            rownames = FALSE)
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


create_downloadButton = function(id, device) {
  downloadHandler(paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, 
                           eval(parse(text = paste0(id, "_plot_out()"))),
                           device = device, height = 300, width = 400, 
                           units = "mm")})
}


tabsetPanel_UI <- function(id, label = NULL) {
  ns <- NS(id)
  tagList(tabsetPanel(tabPanel(title = paste(id, "plot"),
                               br(),
                               div(style = "position:relative",
                                   plotOutput_h(ns(paste0(id, "_plot")),  
                                                hover = hoverOpts(ns(paste0(id, "_hover")), 
                                                                  delay = 10, 
                                                                  delayType = "debounce")),
                                   uiOutput(ns(paste0(id, "_tooltip"))),
                                   downloadButton(ns(paste0(id, "_download_png")), "Download png"),
                                   downloadButton(ns(paste0(id, "_download_jpeg")), "Download jpeg"),
                                   downloadButton(ns(paste0(id, "_download_svg")), "Download svg"))
  ),
  tabPanel(title = paste(id, "data"), 
           tableOutput_h(ns(paste0(id, "_data"))))))
}


tabsetPanel_SERVER <- function(id, data, plot, table) {
  
  moduleServer(id, function(input, output, session) {
    
    output[[paste0(id, "_plot")]] <- renderPlot({plot()})
    output[[paste0(id,"_tooltip")]] <- renderUI({
      ns <- session$ns
      generate_tooltip(data(), input[[paste0(id, "_hover")]])
    })
    output[[paste0(id, "_download_png")]] <- create_downloadButton(id, "png")
    output[[paste0(id, "_download_jpeg")]] <- create_downloadButton(id, "jpeg")
    output[[paste0(id, "_download_svg")]] <- create_downloadButton(id, "svg")
    
    
    output[[paste0(id, "_data")]] <- DT::renderDataTable(server = FALSE, {
      table() %>% 
        dt_format()
    })
    
  })
}

