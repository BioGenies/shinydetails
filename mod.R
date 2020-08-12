
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


#' @title Table widget
#' @description  \code{dt_format} is a function for creating a graphical widget containing a table with download buttons.
#' @param data Dataset. A matrix or data frame.
#' @param cols Names of columns to display.
#' @details This function uses \link{[DT]{datatable}}.
#' @export
#' 

dt_format <- function(dat, cols = colnames(dat)) {
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "csv")),
            filter = "bottom",
            rownames = FALSE)
}


#' @title Download button
#' @description  Generates download button.
#' @param id Id name of tabset panel.
#' @param plot_out Reactive. Plot to save.
#' @param device Parameter from \code{\link{[ggplot2]{ggsave}}}.
#' @return \code{generate_downloadButton} returns output of \code{downloadHandler} for given plot and device.
#' @details This function uses \link{[shiny]{downloadHandler}}
#' @export

generate_downloadButton = function(id, plot_out, device) {
  downloadHandler(paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, plot_out(), device = device, height = 300, 
                           width = 400, units = "mm")})
}


#' @title Tooltip data
#' @description  Prepares data in order that it may be displayed in tooltip.
#' @param hv Hoover.
#' @param plot_out Reactive. Plot to extract data from.
#' @param plot_type Type of plot. Accepts either \code{'point'} or \code{'bar'}. Default \code{'point'}.
#' @return \code{prepare_tt_data} returns prepared data frame containing one record.
#' @details This function filters one row of the imputed data in order that it coresponds the most to the hoover coordinates.
#' @export


prepare_tt_data = function(hv, plot_out, plot_type = "point") {
  
  plot_data = plot_out()[["data"]]
  
  if(plot_type == "bar") {
    hv_data = data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         xmin = ggplot_build(plot_out())[["data"]][[1]][["xmin"]],
                         xmax = ggplot_build(plot_out())[["data"]][[1]][["xmax"]],
                         plot_data) 
    tt_df = filter(hv_data, x < xmax & x >= xmin) %>% 
      select(-x, -y, -xmax, -xmin)
  }
  if(plot_type == "point") {
    tt_df = nearPoints(df = plot_data, coordinfo = hv, maxpoints = 1)
  }
  tt_df
}


#' @title Generate tooltip
#' @description  Generates tooltip .
#' @inheritParams prepare_tt_data
#' @param tt_cols Vector of strings. Names of columns to display in tooltip.
#' @export
#' 

generate_tooltip = function(hv, plot_out, plot_type, tt_cols) {
  
  tt_df = prepare_tt_data(hv, plot_out, plot_type)
  
  if(nrow(tt_df) != 0) { 
    tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                         "left", "right")
    
    tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                     hv[["coords_css"]][["x"]], 
                     hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
    
    style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                    tt_pos_adj, ":", tt_pos, 
                    "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
    
    tt_df = tt_df %>% 
      select(tt_cols)
    div(style = style,
        p(HTML(paste(colnames(tt_df), ": ", t(tt_df ), c(rep("<br/>", ncol(tt_df)-1), ""))))
    )
  }
}


#' @title  UI for tabset panel.
#' @description  Creates module ui for tabset panel with plot and table.
#' @param id Id name of tabset panel.
#' @export

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


#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id name of tabset panel.
#' @inheritParams generate_tooltip
#' @export


tabsetPanel_SERVER <- function(id, plot_out, table_out, plot_type = "point", 
                               tt_cols = colnames(table_out())) {
  
  if(!(plot_type %in% c("bar", "point"))) stop("plot_type must be either bar or point.")
  
  moduleServer(id, function(input, output, session) {
    output[["plot"]] <- renderPlot({plot_out()})
    
    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        generate_tooltip(hv, plot_out, plot_type, tt_cols)
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