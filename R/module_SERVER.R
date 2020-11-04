
#' @title  Supplementary function for ggplot_build
#' @description  Changes ggplot_build \code{data} output in order to make it
#' compatible with the plot when coordinates are flipped.
#' @param ggplot_build_data Element \code{data} from the output of \code{\link[ggplot2]{ggplot_build}}.
#' @export

flip_ggplot_build <- function(ggplot_build_data){
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
  ggplot_build_data
}


#' @title  Server for tabset panel.
#' @description  Creates module server for tabset panel with plot and table.
#' @param id Id of tabset panel.
#' @param plot_out Reactive. Plot to display in tab. The items from first mapping will be connected with tooltip.
#' @param table_out Reactive. Table to display in tab.
#' @inheritParams produce_tt_data
#' @inheritParams plotOutput_h
#' @param tt_content Optional.
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

    plot_info_data <- reactive({
      plot_info <- ggplot_build(plot_out())
      plot_info_data <- NULL

      if("CoordFlip" %in% class(plot_info$plot$coordinates)) {
        plot_info_data <- flip_ggplot_build(plot_info[["data"]][[1]])
      }else {
        plot_info_data <- plot_info[["data"]][[1]]
      }
      plot_info_data
    })

    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        beam_tooltip(hv, plot_info_data(), table_out(), plot_type, tt_content, tt_range)
      }
    })

    output[["download_png"]] <- beam_downloadButton(id, plot_out, "png")
    output[["download_jpeg"]] <- beam_downloadButton(id, plot_out, "jpeg")
    output[["download_svg"]] <- beam_downloadButton(id, plot_out, "svg")

    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      dt_format(table_out())
    })
  })
}
