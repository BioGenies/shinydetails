
#' @title Slider input with helper
#' @description  Constructs a slider widget with helper.
#' Same as \code{helper(sliderInput(outputId, ...))}. Creates help file and/or
#' directory if it does not exist.
#' @inheritParams shiny::sliderInput
#' @inheritParams plotOutput_h
#' @param ... Optional arguments for \code{sliderInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @seealso For more elements with helpers see \code{\link[shinydetails]{numericInput_h}},
#' \code{\link[shinydetails]{plotOutput_h}}, \code{\link[shinydetails]{tableOutput_h}}
#' @export
#'

sliderInput_h <- function(inputId, helpfiles = "helpfiles", ...) {
  suppressMessages({create_help_files_custom(files = inputId,
                                             help_dir = helpfiles)})
  helper(sliderInput(inputId, ...),
         content = inputId,
         type = "markdown")
}

#' @title Numeric input with helper.
#' @description  Constructs a numeric input with helper.
#' Same as \code{helper(numericInput(outputId, ...))}. Creates help file and/or
#' directory if it does not exist.
#' @inheritParams shiny::numericInput
#' @inheritParams plotOutput_h
#' @param ... Optional arguments for \code{numericInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @seealso For more elements with helpers see \code{\link[shinydetails]{plotOutput_h}},
#' \code{\link[shinydetails]{tableOutput_h}}, \code{\link[shinydetails]{sliderInput_h}}
#' @export
#'

numericInput_h <- function(inputId, helpfiles = "helpfiles", ...) {
  suppressMessages({create_help_files_custom(files = inputId,
                                             help_dir = helpfiles)})
  helper(numericInput(inputId, ...),
         content = inputId,
         type = "markdown")
}


#' @title Plot output with helper
#' @description  Renders plot with helper within an application.
#' Same as \code{helper(plotOutput(outputId, ...))}. Creates help file and/or
#' directory if it does not exist.
#' @inheritParams shiny::plotOutput
#' @param helpfiles A character string denoting directory to save empty help files.
#' Default \code{'helpfiles'}.
#' @param ... Optional arguments for \code{plotOutput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @seealso For more elements with helpers see \code{\link[shinydetails]{numericInput_h}},
#' \code{\link[shinydetails]{tableOutput_h}},
#'  \code{\link[shinydetails]{sliderInput_h}}
#' @export
#'

plotOutput_h <- function(outputId, helpfiles = "helpfiles", ...) {
  suppressMessages({create_help_files_custom(files = outputId,
                                             help_dir = "helpfiles",
                                             content = "This help file was generated automatically.")})
  helper(plotOutput(outputId, ...),
         content = outputId,
         type = "markdown")
}


#' @title Table output with helper.
#' @description  Renders table with helper within an application. Creates help file and/or
#' directory if it does not exist.
#' Same as \code{helper(dataTableOutput(outputId, ...))}
#' @inheritParams shiny::dataTableOutput
#' @inheritParams plotOutput_h
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @seealso For more elements with helpers see \code{\link[shinydetails]{numericInput_h}},
#' \code{\link[shinydetails]{plotOutput_h}}, \code{\link[shinydetails]{sliderInput_h}}
#' @export
#'


tableOutput_h <- function(outputId, helpfiles = "helpfiles", ...) {
  suppressMessages({create_help_files_custom(files = outputId,
                                             help_dir = "helpfiles",
                                             content = "This help file was generated automatically.")})
  helper(DT::dataTableOutput(outputId, ...),
         content = outputId,
         type = "markdown")
}

