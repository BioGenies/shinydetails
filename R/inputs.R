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
  create_help_files(files = inputId,
                    help_dir = helpfiles)
  helper(sliderInput(inputId, ...),
         content = inputId,
         type = "markdown")
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
  create_help_files(files = inputId,
                    help_dir = helpfiles)
  helper(numericInput(inputId, ...),
         content = inputId,
         type = "markdown")
}