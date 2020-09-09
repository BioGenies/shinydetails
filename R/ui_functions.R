
#' @title Plot output with helper
#' @description  Renders plot with helper within an application.
#' Same as \code{helper(plotOutput(outputId, ...))}
#' @inheritParams shiny::plotOutput
#' @inheritParams tabsetPanel_UI
#' @param ... Optional arguments for \code{plotOutput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

plotOutput_h <- function(outputId, helpfiles = NULL, ...) {
  if(!is.null(helpfiles) & !file.exists(paste0(helpfiles, "/", outputId, ".md"))) {
    create_help_files(outputId, help_dir = helpfiles)
  }
  helper(plotOutput(outputId, ...), content = outputId, type = "markdown")
}

#' @title Slider input with helper.
#' @description  Constructs a slider widget with helper.
#' Same as \code{helper(sliderInput(outputId, ...))}
#' @inheritParams shiny::sliderInput
#' @param ... Optional arguments for \code{sliderInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

sliderInput_h <- function(inputId, ...) {
  helper(sliderInput(inputId, ...), content = inputId, type = "markdown")
}

#' @title Numeric input with helper.
#' @description  Constructs a numeric input with helper.
#' Same as \code{helper(numericInput(outputId, ...))}
#' @inheritParams shiny::numericInput
#' @param ... Optional arguments for \code{numericInput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'

numericInput_h <- function(inputId, ...) {
  helper(numericInput(inputId, ...), content = inputId, type = "markdown")
}

#' @title Table output with helper.
#' @description  Renders table with helper within an application.
#' Same as \code{helper(dataTableOutput(outputId, ...))}
#' @inheritParams shiny::dataTableOutput
#' @inheritParams tabsetPanel_UI
#' @param ... Optional arguments for \code{dataTableOutput}.
#' @details This function uses \code{\link[shinyhelper]{helper}}.
#' @export
#'


tableOutput_h <- function(outputId, helpfiles = NULL, ...) {
  if(!is.null(helpfiles) & !file.exists(paste0(helpfiles, "/", outputId, ".md"))) {
    create_help_files(outputId, help_dir = helpfiles)
  }
  helper(dataTableOutput(outputId, ...), content = outputId, type = "markdown")
}
