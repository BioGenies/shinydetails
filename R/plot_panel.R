
#' @title Table widget
#' @description  \code{dt_format} is a function for creating a graphical
#' widget containing a table with download buttons (Excel and CSV).
#' @param dat a data object - a matrix or a data frame.
#' @param colnames Names of columns to display in the table. By default it is
#' equal to the names of the columns from the \code{dat}. Parameter \code{colnames} is
#' correspond the argument \code{colnames} from \code{\link[DT]{datatable}}.
#' @details This function uses \code{\link[DT]{datatable}}.
#'

dt_format <- function(dat, colnames = colnames(dat)) {
  datatable(data = dat,
            colnames = colnames,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10,
                           dom = "tBip",
                           autoWidth = TRUE,
                           buttons = c("excel", "csv")),
            filter = "bottom",
            rownames = FALSE)
}


#' @title Ggplot_build for flipped plots
#' @description  \code{flip_ggplot_build} is a supplementary function for ggplot_build.
#' Changes ggplot_build output in order to make it compatible with the
#' plot when coordinates are flipped.
#' @param ggplot_build_data output of \code{\link[ggplot2]{ggplot_build}}
#' @details \code{data} from the output of \code{\link[ggplot2]{ggplot_build}} has
#' names of columns compatible with the mapping. In case when coordinates are flipped,
#' it is not consistent with the displayed plot. \code{flip_ggplot_build} changes
#' the names so that coordinates from the output of  \code{\link[ggplot2]{ggplot_build}}
#' are compatible with the plot and returns it. In case when coordinates are not flipped
#' \code{flip_ggplot_build} returns \code{ggplot_build_data}.
#' @examples
#' \dontrun{
#'
#' # returns plot info without any change because coordinates are not flipped
#' p <- ggplot(Orange, aes(x = age, y = circumference, col = Tree)) +
#' geom_point() +
#' geom_line()
#' plot_info <- ggplot_build(p)
#' flip_ggplot_build(plot_info) # it is the same as plot_info
#'
#' # returns plot info with data modified for flipped coordinates
#' p <- ggplot(Orange, aes(x = age, y = circumference, col = Tree)) +
#' geom_point() +
#' geom_line() +
#' coord_flip()
#' plot_info <- ggplot_build(p)
#' flip_ggplot_build(plot_info)
#' }
#'
#' @export

flip_ggplot_build <- function(ggplot_build_data){

  if("CoordFlip" %in% class(ggplot_build_data$plot$coordinates)) {

    ggplot_build_data[["data"]] <- lapply(ggplot_build_data[["data"]], function(df) {
      df_names <- colnames(df)
      names(df) <- sapply(df_names, function(name) {
        switch(substr(name, 1, 1),
               x = {
                 substr(name, 1, 1) = "y"
               },
               y = {
                 substr(name, 1, 1) = "x"
               })
        name
      })
      df
    })
  }
  ggplot_build_data
}


#' @title  Creating UI for a Shiny module with plot and data
#' @description  Creates module's UI for tabset panel containing two tabs - with
#' plot and an elegant table with data.
#' @param id unique ID name of tabset. The same ID as in the corresponding server for
#' a module
#' @param tab_plot Character. Title of tab containing plot. Default to
#' \code{paste(id, "plot")}.
#' @param tab_table Character. Title of tab containing table. Default to
#' \code{paste(id, "data")}.
#' @inheritParams plotOutput_h
#' @details Module's UI involves a panel with two tabs. The first tab consists of
#' displayed plot and three download buttons (png, svg and jpeg). There is also a
#' compatible with the plot tooltip. The second tab consists of a table
#'  with two download buttons (Excel and CSV). For both table and plot there are
#'  provided helpers. \
#' For more information see \code{\link[shinydetails]{plotOutput_h}} and
#' \code{\link[shinydetails]{tableOutput_h}}
#' @seealso To build Shiny module within a Shiny app using \code{beam_plot_panel_UI}
#' see \code{\link[shinydetails]{beam_plot_panel_SERVER}}.
#' @examples
#' \dontrun{
#' ui <- fluidPage(title = "Example app",
#'              beam_plot_panel_UI("airquality_basic"),
#'              beam_plot_panel_UI("airquality_advanced",
#'                                 tab_plot = "An advanced plot",
#'                                 tab_table = "An elegant table for an advanced plot",
#'                                 helpfiles = "custom_helpfiles"))
#'
#'server <- function(input, output, session) {
#'    airquality_basic_plot <- reactive({
#'        ggplot(airquality, aes(x = Wind, y = Temp, col = as.character(Month))) +
#'        geom_point()
#'    })
#'    airquality_basic_data <- reactive({
#'        airquality
#'    })
#'    beam_plot_panel_SERVER("airquality_basic",
#'                           plot_out = airquality_basic_plot(),
#'                           table_out = airquality_basic_data())
#'
#'    airquality_advanced_data <- reactive({
#'        df <- data.frame(aggregate(. ~ Month, airquality, function(x) c(mean = mean(x), sd = sd(x)))[, c(1, 5)])
#'        df[["Month"]] <- month.name[df[["Month"]]]
#'        data.frame(Month = df[["Month"]],
#'        Temp_mean = df$Temp[, 1],
#'        Error = df$Temp[, 2],
#'        Year = 1973)})
#'
#'    airquality_advanced_plot <- reactive({
#'        ggplot(airquality_advanced_data(), aes(y = 0, yend = Temp_mean, x = Month, xend = Month)) +
#'        geom_segment() +
#'        geom_point(mapping = aes(x = Month, y = Temp_mean), size = 3) +
#'        geom_point() +
#'        ylab("average temperature")
#'    })
#'    beam_plot_panel_SERVER("airquality_advanced",
#'                           plot_out = airquality_advanced_plot(),
#'                           table_out = airquality_advanced_data(),
#'                           plot_type = "geom_segment",
#'                           tt_content = list(row_text = c("Date: %s %i",
#'                                                          "Average temperature: %f °F",
#'                                                          "Error: %f",
#'                                                          "Place: La Guardia Airport"),
#'                                             chosen_cols = c("Month",
#'                                                             "Year",
#'                                                             "Temp_mean",
#'                                                             "Error"))
#'                          )
#'
#'}
#'}
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


#' @title  Creating server for a Shiny module with plot and data.
#' @description  Creates module server for tabset panel containing two tabs - with
#' plot and an elegant table with data.
#' @param id unique Id of tabset panel. The same ID as in the corresponding UI for
#' a module
#' @param plot_out Plot to display in tab. There will be provided an automatically
#' generated tooltip compatible with the plot. The items from first mapping will be
#' connected with tooltip.
#' @param table_out Table to display in tab.
#' @inheritParams extract_tt_data_row
#' @inheritParams plotOutput_h
#' @inheritParams beam_tooltip
#' @details Module's server involves a panel with two tabs. The first tab consists of
#' displayed plot and three download buttons (png, svg and jpeg). There is also a
#' compatible with plot tooltip obtained via \code{\link[shinydetails]{beam_tooltip}}.
#' The second tab consists of a table with two download buttons (Excel and CSV).
#' The table \code{table_out} before  displaying is formatted using
#' \code{\link[shinydetails]{dt_format}}. For both table and plot there are provided
#' helpers.
#' @seealso To build Shiny module within a Shiny app using \code{beam_plot_panel_SERVER}
#' see \code{\link[shinydetails]{beam_plot_panel_UI}}.
#' @examples
#' \dontrun{
#' ui <- fluidPage(title = "Example app",
#'              beam_plot_panel_UI("airquality_basic"),
#'              beam_plot_panel_UI("airquality_advanced",
#'                                 tab_plot = "An advanced plot",
#'                                 tab_table = "An elegant table for an advanced plot",
#'                                 helpfiles = "custom_helpfiles"))
#'
#'server <- function(input, output, session) {
#'    airquality_basic_plot <- reactive({
#'        ggplot(airquality, aes(x = Wind, y = Temp, col = as.character(Month))) +
#'        geom_point()
#'    })
#'    airquality_basic_data <- reactive({
#'        airquality
#'    })
#'    beam_plot_panel_SERVER("airquality_basic",
#'                           plot_out = airquality_basic_plot(),
#'                           table_out = airquality_basic_data())
#'
#'    airquality_advanced_data <- reactive({
#'        df <- data.frame(aggregate(. ~ Month, airquality, function(x) c(mean = mean(x), sd = sd(x)))[, c(1, 5)])
#'        df[["Month"]] <- month.name[df[["Month"]]]
#'        data.frame(Month = df[["Month"]],
#'        Temp_mean = df$Temp[, 1],
#'        Error = df$Temp[, 2],
#'        Year = 1973)})
#'
#'    airquality_advanced_plot <- reactive({
#'        ggplot(airquality_advanced_data(), aes(y = 0, yend = Temp_mean, x = Month, xend = Month)) +
#'        geom_segment() +
#'        geom_point(mapping = aes(x = Month, y = Temp_mean), size = 3) +
#'        geom_point() +
#'        ylab("average temperature")
#'    })
#'    beam_plot_panel_SERVER("airquality_advanced",
#'                           plot_out = airquality_advanced_plot(),
#'                           table_out = airquality_advanced_data(),
#'                           plot_type = "geom_segment",
#'                           tt_content = list(row_text = c("Date: %s %i",
#'                                                          "Average temperature: %f °F",
#'                                                          "Error: %f",
#'                                                          "Place: La Guardia Airport"),
#'                                             chosen_cols = c("Month",
#'                                                             "Year",
#'                                                             "Temp_mean",
#'                                                             "Error"))
#'                          )
#'
#'}
#'}
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

    output[["plot"]] <- renderPlot({plot_out()})

    plot_info_data <- reactive({
      plot_info <- ggplot_build(plot_out())
      flip_ggplot_build(plot_info)[["data"]][[1]]
    })

    output[["tooltip"]] <- renderUI({
      hv = input[["hover"]]
      if(!is.null(hv)) {
        beam_tooltip(hv, plot_info_data(), table_out(), plot_type, tt_content, tt_range)
      }
    })

    output[["download_png"]] <- beam_downloadButton(id, plot_out(), "png")
    output[["download_jpeg"]] <- beam_downloadButton(id, plot_out(), "jpeg")
    output[["download_svg"]] <- beam_downloadButton(id, plot_out(), "svg")

    output[["data"]] <- DT::renderDataTable(server = FALSE, {
      dt_format(table_out())
    })
  })
}
