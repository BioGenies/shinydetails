
#' @title Plot output with helper
#' @description  Renders plot with helper within an application.
#' Same as \code{helper(plotOutput(outputId, ...))}
#' @inheritParams shiny::plotOutput
#' @param helpfiles A character string denoting directory to save empty help files. Default \code{'helpfiles'}.
#' @param ... Optional arguments for \code{plotOutput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

plotOutput_h <- function(outputId, helpfiles = "helpfiles", ...) {
  create_help_files(files = outputId,
                    help_dir = "helpfiles")
  helper(plotOutput(outputId, ...),
         content = outputId,
         type = "markdown")
}


#' @title Table output with helper.
#' @description  Renders table with helper within an application.
#' Same as \code{helper(dataTableOutput(outputId, ...))}
#' @inheritParams shiny::dataTableOutput
#' @inheritParams plotOutput_h
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'


tableOutput_h <- function(outputId, helpfiles = "helpfiles", ...) {
  create_help_files(files = outputId,
                    help_dir = "helpfiles")
  helper(DT::dataTableOutput(outputId, ...),
         content = outputId,
         type = "markdown")
}
