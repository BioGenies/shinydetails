

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
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "csv")),
            filter = "bottom",
            rownames = FALSE)
}


generate_tooltip = function(hv, tt_df, content = NA) {
  
  if(nrow(tt_df) != 0) { 
    tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                         "left", "right")
    
    tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                     hv[["coords_css"]][["x"]], 
                     hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
    
    style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                    tt_pos_adj, ":", tt_pos, 
                    "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
    if(is.na(content)) {
      div(style = style,
          p(HTML(paste(colnames(tt_df), ": ", t(tt_df ), c(rep("<br/>", ncol(tt_df)-1), ""))))
      )
    }else {
      div(style = style,
          p(HTML(content))
      )
    }
  }
}


tabsetPanel_UI <- function(id, label = NULL) {
  ns <- NS(id)
  tagList(tabsetPanel(tabPanel(title = paste(id, "plot"),
                               br(),
                               div(style = "position:relative",
                                   plotOutput_h(ns("plot"),  
                                                hover = hoverOpts(ns("hover"), 
                                                                  delay = 10, 
                                                                  delayType = "debounce")),
                                   uiOutput(ns("tooltip")),
                                   downloadButton(ns("download_png"), "Download png"),
                                   downloadButton(ns("download_jpeg"), "Download jpeg"),
                                   downloadButton(ns("download_svg"), "Download svg"))
  ),
  tabPanel(title = paste(id, "data"), 
           tableOutput_h(ns("data")))))
}


generate_downloadButton = function(id, plot_out, device) {
  downloadHandler(paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, plot_out(), device = device, height = 300, 
                           width = 400, units = "mm")})
}


tabsetPanel_SERVER <- function(id, data, plot_out, table_out, bar_plot = FALSE) {
  
  moduleServer(id, function(input, output, session) {
    output[["plot"]] <- renderPlot({plot_out()})
    
    output[["tooltip"]] <- renderUI({
      
      hv = input[["hover"]]
      plot_data <- plot_out()[["data"]]
      
      if(!is.null(hv)) {
        
        if(bar_plot) {
          hv_data = data.frame(x = hv[["x"]],
                              y = hv[["y"]],
                              xmin = ggplot_build(plot_out())[["data"]][[1]][["xmin"]],
                              xmax = ggplot_build(plot_out())[["data"]][[1]][["xmax"]],
                              plot_data) 
          tt_df = filter(hv_data, x < xmax & x >= xmin) %>% 
            select(-x, -y, -xmax, -xmin)
        }else {
          tt_df <- nearPoints(df = plot_data, coordinfo = hv, maxpoints = 1)
        }
        generate_tooltip(hv, tt_df)
      }
    })
    
    output[["download_png"]] <- generate_downloadButton(id, plot_out, "png")
    output[["download_jpeg"]] <- generate_downloadButton(id, plot_out, "jpeg")
    output[["download_svg"]] <- generate_downloadButton(id, plot_out, "svg")
    
    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      table_out() %>% 
        dt_format()
    })
    
  })
}

