
#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id of tabset panel.
#' @param plot_out Reactive. Plot to display in tab.
#' @param table_out Reactive. Table to display in tab.
#' @inheritParams produce_tt_data
#' @inheritParams plotOutput_h
#' @param tt_content Optional.
#' @param tt_range An extra range for tooltip. Default \code{5}.
#' @importFrom DT renderDataTable
#' @export


tabsetPanel_SERVER <- function(id,
                               plot_out,
                               table_out,
                               plot_type = "point",
                               tt_content = NULL,
                               tt_range = 5,
                               helpfiles = "helpfiles") {

  if(!(plot_type %in% c("geom_col", "geom_point", "geom_segment")))
    stop("plot_type must be either geom_col, geom_segment or geom_point.")

  moduleServer(id, function(input, output, session) {

    shinyhelper::observe_helpers(session = shiny::getDefaultReactiveDomain(),
                                 help_dir = helpfiles)

    output[["plot"]] <- renderPlot({plot_out()})

    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        spark_tooltip(hv, plot_out(), table_out(), plot_type, tt_content, tt_range)
      }
    })

    output[["download_png"]] <- spark_downloadButton(id, plot_out, "png")
    output[["download_jpeg"]] <- spark_downloadButton(id, plot_out, "jpeg")
    output[["download_svg"]] <- spark_downloadButton(id, plot_out, "svg")

    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      dt_format(table_out())
    })
  })
}
