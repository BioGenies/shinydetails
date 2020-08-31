
#' @title  UI for tabset panel.
#' @description  Creates module ui for tabset panel with plot and table.
#' @param id Id name of tabset panel.
#' @export

tabsetPanel_UI <- function(id) {
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
#' @param id Id of tabset panel.
#' @param plot_out Reactive. Plot to display in tabset panel.
#' @param table_out Reactive. Table to display in tabset panel.
#' @param plot_type Type of plot. Type of plot. Accepts either \code{'point'}, \code{'comparison'},
#' \code{'differential'} or \code{'bar'}. Default \code{'point'}.
#' @param tt_content Optional
#' @importFrom DT renderDataTable
#' @export


tabsetPanel_SERVER <- function(id, plot_out, table_out, plot_type = "point", tt_content = NULL) {

  if(!(plot_type %in% c("bar", "point", "comparison", "differential"))) stop("plot_type must be either bar, differential, comparison or point.")

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
