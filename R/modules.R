#TODO: change types of plots


#' @title  UI for tabset panel.
#' @description  Creates module ui for tabset panel. It contains two tabs - with plot and an elegenat table with respective data.
#' @param id Id name of tabset panel.
#' @param tab_plot Character. Title of tab containing plot. Default to \code{paste(id, "plot")}.
#' @param tab_table Character. Title of tab containing table. Default to \code{paste(id, "data")}.
#' @details \code{tabsetPanel_UI} provides three download buttons (png, svg and jpeg) and tooltip for plot and helpers for both plot and table.
#' @export

tabsetPanel_UI <- function(id,
                           tab_plot = paste(id, "plot"),
                           tab_table = paste(id, "data")) {
  ns <- NS(id)
  tagList(tabsetPanel(tabPanel(title = tab_plot,
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
  tabPanel(title = tab_table,
           tableOutput_h(ns("data")))))
}


#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id of tabset panel.
#' @param plot_out Reactive. Plot to display in tab.
#' @param table_out Reactive. Table to display in tab.
#' @param plot_type Type of plot. Accepts either \code{'point'}, \code{'comparison'},
#' \code{'differential'} or \code{'bar'}. Default \code{'point'}.
#' @param tt_content Optional
#' @importFrom DT renderDataTable
#' @export


tabsetPanel_SERVER <- function(id, plot_out, table_out, plot_type = "point", tt_content = NULL) {

  if(!(plot_type %in% c("geom_col", "geom_point", "comparison", "differential"))) stop("plot_type must be either bar, differential, comparison or point.")

  moduleServer(id, function(input, output, session) {
    output[["plot"]] <- renderPlot({plot_out()})

    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        generate_tooltip(hv, plot_out, plot_type, tt_content)
      }
    })

    output[["download_png"]] <- generate_downloadButton(id, plot_out, "png")
    output[["download_jpeg"]] <- generate_downloadButton(id, plot_out, "jpeg")
    output[["download_svg"]] <- generate_downloadButton(id, plot_out, "svg")

    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      dt_format(table_out())
    })
  })
}
