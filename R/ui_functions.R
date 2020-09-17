
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
  create_help_files(outputId, help_dir = helpfiles)
  helper(plotOutput(outputId, ...), content = outputId, type = "markdown")
}

#' @title Slider input with helper.
#' @description  Constructs a slider widget with helper.
#' Same as \code{helper(sliderInput(outputId, ...))}
#' @inheritParams shiny::sliderInput
#' @inheritParams plotOutput_h
#' @param ... Optional arguments for \code{sliderInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

sliderInput_h <- function(inputId, helpfiles = "helpfiles", ...) {
  helper(sliderInput(inputId, ...), content = inputId, type = "markdown")
}

#' @title Numeric input with helper.
#' @description  Constructs a numeric input with helper.
#' Same as \code{helper(numericInput(outputId, ...))}
#' @inheritParams shiny::numericInput
#' @inheritParams plotOutput_h
#' @param ... Optional arguments for \code{numericInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

numericInput_h <- function(inputId, helpfiles = "helpfiles", ...) {
  helper(numericInput(inputId, ...), content = inputId, type = "markdown")
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
  create_help_files(outputId, help_dir = helpfiles)
  helper(dataTableOutput(outputId, ...), content = outputId, type = "markdown")
}
