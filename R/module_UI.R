
#' @title  UI for tabset panel.
#' @description  Creates module ui for tabset panel. It contains two tabs - with
#' plot and an elegenat table with respective data.
#' @param id Id name of tabset panel.
#' @param tab_plot Character. Title of tab containing plot. Default to
#' \code{paste(id, "plot")}.
#' @param tab_table Character. Title of tab containing table. Default to
#' \code{paste(id, "data")}.
#' @param helpfiles A character string denoting directory to save empty help files.
#' If \code{NULL} help files will not be created. Default \code{NULL}.
#' @details \code{tabsetPanel_UI} provides three download buttons (png, svg and jpeg)
#' and tooltip for plot and helpers for both plot and table.
#' @export

tabsetPanel_UI <- function(id,
                           tab_plot = paste(id, "plot"),
                           tab_table = paste(id, "data"),
                           helpfiles = NULL) {
  ns <- NS(id)
  tagList(tabsetPanel(tabPanel(title = tab_plot,
                               br(),
                               div(style = "position:relative",
                                   plotOutput_h(outputId = ns("plot"),
                                                hover = hoverOpts(ns("hover"),
                                                                  delay = 10,
                                                                  delayType = "debounce")),
                                   uiOutput(ns("tooltip")),
                                   downloadButton(ns("download_png"), "Download png"),
                                   downloadButton(ns("download_jpeg"), "Download jpeg"),
                                   downloadButton(ns("download_svg"), "Download svg"))
  ),
  tabPanel(title = tab_table,
           tableOutput_h(outputId = ns("data")))))
}
