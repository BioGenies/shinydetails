
#' @title Table widget
#' @description  \code{dt_format} is a function for creating a graphical
#' widget containing a table with download buttons (Excel and CSV).
#' @param dat Dataset. A matrix or data frame.
#' @param cols Names of columns to display.
#' @details This function uses \code{\link[DT]{datatable}}.
#'

dt_format <- function(dat, cols = colnames(dat)) {
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10,
                           dom = "tBip",
                           autoWidth = TRUE,
                           buttons = c("excel", "csv")),
            filter = "bottom",
            rownames = FALSE)
}


#' @title  Supplementary function for ggplot_build
#' @description  Changes ggplot_build \code{data} output in order to make it
#' compatible with the plot when coordinates are flipped.
#' @param ggplot_build_data Element \code{data} from the output of \code{\link[ggplot2]{ggplot_build}}.
#' @export

flip_ggplot_build <- function(ggplot_build_data){

  if("CoordFlip" %in% class(ggplot_build_data$plot$coordinates)) {
    ggplot_build_data_names <- colnames(ggplot_build_data)
    names(ggplot_build_data) <- sapply(ggplot_build_data_names, function(name) {
      switch(substr(name, 1, 1),
             x = {
               substr(name, 1, 1) = "y"
             },
             y = {
               substr(name, 1, 1) = "x"
             })
      name
    })
  }
  ggplot_build_data
}


#' @title  UI for tabset panel.
#' @description  Creates module ui for tabset panel. It contains two tabs - with
#' plot and an elegant table with respective data.
#' @param id Id name of tabset panel.
#' @param tab_plot Character. Title of tab containing plot. Default to
#' \code{paste(id, "plot")}.
#' @param tab_table Character. Title of tab containing table. Default to
#' \code{paste(id, "data")}.
#' @inheritParams plotOutput_h
#' @details \code{tabsetPanel_UI} provides three download buttons (png, svg and jpeg)
#' and tooltip for plot and helpers for both plot and table.
#' @export

beam_plot_panel_UI <- function(id,
                               tab_plot = paste(id, "plot"),
                               tab_table = paste(id, "data"),
                               helpfiles = "helpfiles") {
  ns <- NS(id)
  tagList(tabsetPanel(tabPanel(title = tab_plot,
                               br(),
                               div(style = "position:relative",
                                   plotOutput_h(outputId = ns("plot"),
                                                hover = hoverOpts(ns("hover"),
                                                                  delay = 10,
                                                                  delayType = "debounce"),
                                                helpfiles = helpfiles),
                                   uiOutput(ns("tooltip")),
                                   downloadButton(ns("download_png"), "Download png"),
                                   downloadButton(ns("download_jpeg"), "Download jpeg"),
                                   downloadButton(ns("download_svg"), "Download svg"))
  ),
  tabPanel(title = tab_table,
           tableOutput_h(outputId = ns("data"), helpfiles = helpfiles))))
}


#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id of tabset panel.
#' @param plot_out Plot to display in tab. The items from first mapping will be connected with tooltip.
#' @param table_out Table to display in tab.
#' @inheritParams extract_tt_data_row
#' @inheritParams plotOutput_h
#' @param tt_content Optional.
#' @importFrom DT renderDataTable
#' @export


beam_plot_panel_SERVER <- function(id,
                               plot_out,
                               table_out,
                               plot_type = "geom_point",
                               tt_content = NULL,
                               tt_range = 5,
                               helpfiles = "helpfiles") {

  match.arg(plot_type, c("geom_col", "geom_point", "geom_segment"), several.ok = FALSE)

  moduleServer(id, function(input, output, session) {

    shinyhelper::observe_helpers(session = shiny::getDefaultReactiveDomain(),
                                 help_dir = helpfiles)

    output[["plot"]] <- renderPlot({plot_out})

    plot_info_data <- reactive({
      plot_info <- ggplot_build(plot_out)
      flip_ggplot_build(plot_info[["data"]][[1]])
    })

    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        beam_tooltip(hv, plot_info_data(), table_out, plot_type, tt_content, tt_range)
      }
    })

    output[["download_png"]] <- beam_downloadButton(id, plot_out, "png")
    output[["download_jpeg"]] <- beam_downloadButton(id, plot_out, "jpeg")
    output[["download_svg"]] <- beam_downloadButton(id, plot_out, "svg")

    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      dt_format(table_out)
    })
  })
}
