#TODO: change types of plots


#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id of tabset panel.
#' @param plot_out Reactive. Plot to display in tab.
#' @param table_out Reactive. Table to display in tab.
#' @param plot_type Type of plot. Accepts either \code{'geom_point'},
#' \code{'comparison'},
#' \code{'differential'} or \code{'geom_col'}. Default \code{'geom_point'}.
#' @param tt_content Optional.
#' @importFrom DT renderDataTable
#' @export


tabsetPanel_SERVER <- function(id, plot_out, table_out,
                               plot_type = "point", tt_content = NULL) {

  if(!(plot_type %in% c("geom_col", "geom_point", "comparison", "differential")))
    stop("plot_type must be either bar, differential, comparison or point.")

  moduleServer(id, function(input, output, session) {

    shinyhelper::observe_helpers(session = shiny::getDefaultReactiveDomain(),
                                 help_dir = helpfiles)

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
